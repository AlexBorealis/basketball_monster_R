# Load packages
source(paste0(getwd(), "/need_pckgs.R"), local = T)

## Reading needed variables (for bots, for DB, for telegram)
token_1 <- data.table(read.table("bots_vars.txt", header = T))[name == "basketball_monster_table", value]

for_db <- data.table(read.table("db_vars.txt", header = T))

for_tg <- data.table(read.table("tg_vars.txt", header = T)) 

db_tables <- data.table(read.table("db_tables.txt", header = T))

# Days for report
n = 0

## Creation class Bot
bot <- Bot(token = token_1)

# Reading existing table from DB
pool <- dbPool(RPostgreSQL::PostgreSQL(),
	       user = for_db[name == "user", value], 
	       password = for_db[name == "password", value], 
	       dbname = for_db[name == "dbname", value], 
	       host = for_db[name == "host", value],
	       maxSize = 1,
	       idleTimeout = 1,
	       validationInterval = 0)

alerts <- data.table(

		     dbGetQuery(pool, str_glue("select * from alerts where date_observ::date = '{Sys.Date() - n}'"))
		     
		    )[order(-date_observ)] %>%

	mutate(status_player = gsub(x = status_player, pattern = "high level - ", replacement = "")) |>
	group_by(name_player) |>
	filter(row_number() == 1) |>
        as.data.table()	%>%
	.[, .(name_player,
	      status_player,
	      position_player,
	      team)] %>%
	.[order(team)]

poolClose(pool)

if (nrow(alerts) > 0) {

			
	# Creating xlsx table
	wb <- createWorkbook()
		
	addWorksheet(wb, sheetName = Sys.Date() - n)
			
	writeDataTable(wb,
		       sheet = as.character(Sys.Date() - n),
		       x = alerts) # Creation table of articles and names in the beginning in document
			
	setColWidths(wb,
		     sheet = as.character(Sys.Date() - n),
		     cols = 1:ncol(alerts),
		     widths = c(20, 50, 15, 10))

	addStyle(wb,
		 sheet = as.character(Sys.Date() - n),
		 style = createStyle(halign = "center",
				     valign = "center"),
		 cols = 1:(ncol(alerts) + 1),
		 rows = 1:(nrow(alerts) + 1),
		 gridExpand = T)
				
	saveWorkbook(wb,
		     "alerts.xlsx",
		     overwrite = TRUE) # Saving excel document
	
	png(gsub(pattern = ".xlsx",
		 replacement = ".png",
		 x = list.files(getwd(), pattern = "alerts")),
	    height = 35*nrow(alerts),
	    width = 180*ncol(alerts))
					
	grid.table(alerts)
	dev.off()
	graphics.off()
							
	# Sending image in TG
	bot$sendPhoto(chat_id = for_tg[name == "chat_id", value],
		      photo = paste0(getwd(), "/alerts.png"))
							
	} else {
			
	NULL

}

rm(list = ls())

gc(reset = T, full = T)
