##Setting directory
setwd(getwd())

## Load workspace
load(paste0(getwd(), "/basketball_monster_R.RData"))

## Load packages
source(paste0(getwd(), "/need_pckgs.R"), local = T)

## Load engine
source(paste0(getwd(), "/engine.R"), local = T)

Sys.getpid()

## Reading needed variables (for bots, for DB, for telegram and for site)
token <- data.table(read.table("bots_vars.txt", header = T))[name == "basketball_monster", value]

for_db <- data.table(read.table("db_vars.txt", header = T))

for_url <- data.table(read.table("url_vars.txt", header = T))

for_tg <- data.table(read.table("tg_vars.txt", header = T)) 

db_tables <- data.table(read.table("db_tables.txt", header = T))

## Creation class Bot
bot <- Bot(token = token)

# Read main html
main_html <- read_html(for_url[name == "url", value], 
		       handle = curl::new_handle("useragent" = "Mozilla/5.0"),
		       options = c("RECOVER", "PEDANTIC"),
		       verbose = T) |>
	html_elements(xpath = "//*[@class='q-su-holder']")

# Getting xml length
length_list_player <- main_html |>
	xml_length()

new_player <- map_dfr(1:length_list_player, \(j) {
			
			data.table(name_player = name_player(j),
				   status_player = status_player(j),
				   position_player = position_player(j),
				   team = team(j))
	
	})[status_player %like% "high level"]

# Main process
# 1) Checking if an object 'for_bot' exists
# 2) Checking if an identical object 'for_bot' and last changes fro site
# 3) If last changes and 'for_bot' is not identical then send message to TG and in DB 
if (exists("for_bot")) {
	
	if (identical(for_bot[status_player %like% "high level", .(name_player,
																														 status_player,
																														 position_player,
																														 team)],
								new_player[, .(name_player,
															 status_player,
															 position_player,
															 team)])) {
		
		NULL
		
	} else {
		

		DT <- data.table(date_observ = Sys.time(),
				 anti_join(new_player,
					   for_bot[status_player %like% "high level"],
					   by = c("name_player", "status_player", "position_player", "team")))[!is.na(status_player)] |>
			distinct(status_player, .keep_all = T) |>
			distinct(name_player, .keep_all = T)
		
		if (nrow(DT) > 0) {
			
			lst <- map(1:nrow(DT), \(i) {
				
				bot$sendMessage(chat_id = for_tg[name == "chat_id", value],
						text = str_glue("{new_player$name_player[i]} {new_player$position_player[i]} ({new_player$team[i]})
								{new_player$status_player[i]}") |> gsub(pattern = "high level - ", replacement = ""))
				
			})
			
			pool <- dbPool(RPostgreSQL::PostgreSQL(),
				       user = for_db[name == "user", value], 
				       password = for_db[name == "password", value], 
				       dbname = for_db[name == "dbname", value], 
				       host = for_db[name == "host", value],
				       maxSize = 1,
				       idleTimeout = 1,
				       validationInterval = 0) # Connect to PostgreSQL
			
			dbWriteTable(pool,
				     value = DT,
				     name = db_tables$table_name[1],
				     append = T,
				     row.names = F)
			
			poolClose(pool)
			
		} else {
			
			NULL
			
		}
		
	}
	
} else {
	
	for_bot <- map_dfr(1:length_list_player, \(j) {
		
		data.table(name_player = name_player(j),
			   status_player = status_player(j),
			   position_player = position_player(j),
			   team = team(j))
		
	})
	
}

for_bot <- map_dfr(1:length_list_player, \(j) {
	
	data.table(name_player = name_player(j),
		   status_player = status_player(j),
		   position_player = position_player(j),
		   team = team(j))
	
})

rm(list = ls() %>% .[. != "for_bot"])

gc(reset = T, full = T)

save.image(paste0(getwd(), "/basketball_monster_R.RData"))
