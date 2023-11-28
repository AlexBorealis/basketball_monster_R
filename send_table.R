## Load packages
source(paste0(getwd(), "/need_pckgs.R"),
			 local = T)

token_1 <- data.table(read.table("bots_vars.txt", header = T))[name == "basketball_monster_table", value]

for_db <- data.table(read.table("db_vars.txt", header = T))

for_tg <- data.table(read.table("tg_vars.txt", header = T)) 

db_tables <- data.table(read.table("db_tables.txt", header = T))

## Creation class Bot
bot <- Bot(token = token_1)

updates <- bot$getUpdates()

pool <- dbPool(RPostgreSQL::PostgreSQL(), 
							 user = for_db[name == "user", value], 
							 password = for_db[name == "password", value], 
							 dbname = for_db[name == "dbname", value], 
							 host = for_db[name == "host", value],
							 maxSize = 1,
							 idleTimeout = 1,
							 validationInterval = 0)

alerts <- data.table(dbGetQuery(pool,
																str_glue("select * from alerts where date_observ = '{Sys.Date()}'")))[order(-time_observ)] %>%
	mutate(status_player = gsub(x = status_player, pattern = "high level - ", replacement = "")) %>%
	distinct(status_player, .keep_all = T) %>%
	distinct(name_player, .keep_all = T) %>%
	.[, .(name_player,
				status_player,
				position_player,
				team)] %>%
	.[order(team)]

poolClose(pool)

wb <- createWorkbook()

addWorksheet(wb, sheetName = Sys.Date())

writeDataTable(wb,
							 sheet = Sys.Date(),
							 x = alerts) # Creation table of articles and names in the beginning in document

setColWidths(wb,
						 sheet = Sys.Date(),
						 cols = 1:ncol(alerts),
						 widths = c(20, 50, 15, 10))

addStyle(wb,
				 sheet = Sys.Date(),
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
		height = 45*nrow(alerts),
		width = 150*ncol(alerts))

	  grid.table(alerts)
	  dev.off()
	  graphics.off()

bot$sendPhoto(chat_id = for_tg[name == "chat_id", value],
							photo = paste0(getwd(), "/alerts.png"))