## Setting directory
setwd("D:/R/Work/My_scripts/basketball_monster/basketball_monster")

## Load workspace
load("D:/R/Work/My_scripts/basketball_monster/basketball_monster/basketball_monster.RData")

## Load packages
source(paste0(getwd(), "/need_pckgs.R"),
			 local = T)

## Load engine
source(paste0(getwd(), "/engine.R"),
			 local = T)

token <- data.table(read.table("bots.txt", header = T))[name == "basketball_monster", value]

## Creation class Bot
bot <- Bot(token = token)

updates <- bot$getUpdates()

main_html <- read_html("https://basketballmonster.com/playernews.aspx") |>
	html_elements(xpath = "//*[@class='q-su-holder']")

length_list_player <- main_html |>
	xml_length()

if (identical(for_bot[status_player %like% "high level"], 
							map_dfr(1:length_list_player, \(j) {
								
								data.table(name_player = name_player(j),
													 status_player = status_player(j),
													 position_player = position_player(j),
													 team = team(j))
								
							})[status_player %like% "high level"])) {
	
	NULL
	
} else {
	
	new_player <- map_dfr(1:length_list_player, \(j) {
		
		data.table(name_player = name_player(j),
							 status_player = status_player(j),
							 position_player = position_player(j),
							 team = team(j))
		
	})[status_player %like% "high level"]
	
	DT <- data.table(date_observ = strftime(Sys.time(), format = "%Y-%m-%d"),
									 time_observ = strftime(Sys.time(), format = "%H:%M:%S"),
									 anti_join(new_player,
									 					 for_bot[status_player %like% "high level"],
									 					 by = c("name_player", "status_player", "position_player", "team")))[!is.na(status_player)]
	
	if (nrow(DT) > 0) {
		
		lst <- map(1:nrow(DT), \(i) {
			
			bot$sendMessage(chat_id = -4029813396,
											text = str_glue("{new_player$name_player[i]} {new_player$position_player[i]} ({new_player$team[i]})
																			
																			{new_player$status_player[i]}") |> gsub(pattern = "high level - ", replacement = ""))
			
		})
		
		pool <- dbPool(RPostgreSQL::PostgreSQL(), 
									 user = "postgres", 
									 password = NULL, 
									 dbname = "basketball", 
									 host = "localhost",
									 maxSize = 1,
									 idleTimeout = 1,
									 validationInterval = 0) # Connect to PostgreSQL
		
		dbWriteTable(pool,
								 value = DT,
								 name = "alerts",
								 append = T,
								 row.names = F)
		
		poolClose(pool)
		
	} else {
		
		NULL
		
	}
	
}

for_bot <- map_dfr(1:length_list_player, \(j) {
	
	data.table(name_player = name_player(j),
						 status_player = status_player(j),
						 position_player = position_player(j),
						 team = team(j))
	
})

rm(list = ls() %>% .[. != "for_bot"])

gc(reset = T, full = T)

save.image("D:/R/Work/My_scripts/basketball_monster/basketball_monster/basketball_monster.RData")

#wb <- createWorkbook() # Creation work book object

#addWorksheet(wb, "Players")

#writeDataTable(wb,
#							 sheet = "Players",
#							 x = for_bot) # Creation table of articles and names in the beginning in document

#setColWidths(wb,
#						 sheet = "Players",
#						 cols = c(1, 2),
#						 widths = c(25, 45))

#setRowHeights(wb,
#							sheet = "Players",
#							rows = 1:(nrow(for_bot) + 1),
#							heights = 17.5) # Setting height of rows

#cell_style <- createStyle(halign = "center",
#													valign = "center")

#addStyle(wb,
#				 sheet = "Players",
#				 style = cell_style,
#				 cols = 1:(ncol(for_bot) + 1),
#				 rows = 1:(nrow(for_bot) + 1),
#				 gridExpand = T)

#saveWorkbook(wb,
#						 "Players.xlsx",
#						 overwrite = TRUE) # Saving excel document

#data.table(read.xlsx("Players.xlsx"))

## Creation report image
#png(gsub(pattern = "Players.xlsx",
#				 replacement = "Players.png",
#				 list.files(getwd(), pattern = "^Players")),
#		height = 25*nrow(for_bot),
#		width = 190*ncol(for_bot))

#grid.table(for_bot)
#dev.off()
#graphics.off()

#bot$sendPhoto(chat_id = -4029813396,
#							photo = list.files(getwd(), pattern = "png$") %>% .[length(.)]) # Sending alert message