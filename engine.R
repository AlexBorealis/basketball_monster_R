name_player <- function(i = 1) {
	
	main_html |>
		html_elements(xpath = "//*[@class='q-title']") %>%
		.[i] %>%
		xml_text()
	
}

status_player <- function(i = 1) {
	
	text <- main_html %>%
		html_elements(xpath = "//*[@class='status-update-player-status pb-1']") %>% 
		.[i] %>%
		xml_text()
	
	if (!is_empty(grep(text, pattern = "low level", ignore.case = T))) {
		
		text <- text |>
			str_replace(pattern = "low level", replacement = " low level")
		
	} else if (!is_empty(grep(text, pattern = "medium level", ignore.case = T))) {
		
		text <- text |>
			str_replace(pattern = "medium level", replacement = " medium level")
		
	} else if (!is_empty(grep(text, pattern = "high level", ignore.case = T))) {
		
		text <- text |>
			str_replace(pattern = "high level", replacement = " high level")
		
	} else {
		
		text
		
	}
	
	if (!is_empty(grep(text, pattern = "Off Injury Report", ignore.case = T))) {
		
		text_sub_level <- text |> 
			str_replace(pattern = "Off Injury Report", replacement = toupper("Off Injury Report "))
		
	} else if (!is_empty(grep(text, pattern = "Questionable to return", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Questionable to return", replacement = toupper("Questionable to return "))
			
	} else if (!is_empty(grep(text, pattern = "Questionable", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Questionable", replacement = toupper("Questionable "))
			
	} else if (!is_empty(grep(text, pattern = "Doubtful to return", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Doubtful to return", replacement = toupper("Doubtful to return "))
			
	} else if (!is_empty(grep(text, pattern = "Probable", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Probable", replacement = toupper("Probable "))
			
	} else if (!is_empty(grep(text, pattern = "Probable to return", ignore.case = T))) {
		
		text_sub_level <- text |> 
			str_replace(pattern = "Probable to return", replacement = toupper("Probable to return "))
			
	} else if (!is_empty(grep(text, pattern = "Playing", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Playing", replacement = toupper("Playing "))
			
	} else if (!is_empty(grep(text, pattern = "Injured", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Injured", replacement = toupper("Injured "))
			
	} else if (!is_empty(grep(text, pattern = "To Bench with Injury", ignore.case = T))) {
		
		text_sub_level <- text |> 
			str_replace(pattern = "To Bench with Injury", replacement = toupper("To Bench with Injury "))
		
	} else if (!is_empty(grep(text, pattern = "Ejected", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "Ejected", replacement = toupper("Ejected "))
			
	} else if (!is_empty(grep(text, pattern = "In the Locker Room", ignore.case = T))) {
			
		text_sub_level <- text |> 
			str_replace(pattern = "In the Locker Room", replacement = toupper("In the Locker Room "))
			
	} else if (!is_empty(grep(text, pattern = "Will not return", ignore.case = T))) {
																				
		text_sub_level <- text |> 
			str_replace(pattern = "Will not return", replacement = toupper("Will not return "))
																				
	} else if (!is_empty(grep(text, pattern = "Available to Return", ignore.case = T))) {
																					
		text_sub_level <- text |> 
			str_replace(pattern = "Available to Return", replacement = toupper("Available to Return "))
																					
	} else if (!is_empty(grep(text, pattern = "Note", ignore.case = T))) {
																						
		text_sub_level <- text |> 
			str_replace(pattern = "Note", replacement = toupper("Note "))
																						
	} else if (!is_empty(grep(text, pattern = "Off the Bench", ignore.case = T))) {
																							
		text_sub_level <- text |> 
			str_replace(pattern = "Off the Bench", replacement = toupper("Off the Bench "))
																							
	} else if (!is_empty(grep(text, pattern = "Doubtful", ignore.case = T))) {
																								
		text_sub_level <- text |> 
			str_replace(pattern = "Doubtful", replacement = toupper("Doubtful "))
																								
	} else if (!is_empty(grep(text, pattern = "Back on Bench", ignore.case = T))) {
				
		text_sub_level <- text |> 
			str_replace(pattern = "Back on Bench", replacement = toupper("Back on Bench "))
				
	} else if (!is_empty(grep(text, pattern = "Has Returned", ignore.case = T))) {
				
		text_sub_level <- text |> 
			str_replace(pattern = "Has Returned", replacement = toupper("Has Returned "))
				
	} else if (!is_empty(grep(text, pattern = "Out", ignore.case = T))) {
				
		text_sub_level <- text |> 
			str_replace(pattern = "Out", replacement = toupper("Out "))
				
	} else if (!is_empty(grep(text, pattern = "Starting", ignore.case = T))) {
	
		text_sub_level <- text |>
			str_replace(pattern = "Starting", replacement = toupper("Starting"))
	
	} else if (!is_empty(grep(text, pattern = "Started 2nd Half", ignore.case = T))) {
			
		text_sub_level <- text |>
			str_replace(pattern = "Started 2nd Half", replacement = toupper("Started 2nd Half"))
			
	} else {
																							
		text_sub_level <- text
		
	}
	
	if (!is_empty(grep(text_sub_level, pattern = "low level", ignore.case = T))) {
		
		paste0("low level - ", gsub(text_sub_level, pattern = "low level", replacement = "", ignore.case = T))
		
	} else if (!is_empty(grep(text_sub_level, pattern = "medium level", ignore.case = T))) {
			
		paste0("medium level - ", gsub(text_sub_level, pattern = "medium level", replacement = "", ignore.case = T))
			
	} else if (!is_empty(grep(text_sub_level, pattern = "high level", ignore.case = T))) {
				
		paste0("high level - ", gsub(text_sub_level, pattern = "high level", replacement = "", ignore.case = T))
				
	} else {
		
		text_sub_level
		
	}
	
}

position_player <- function(i = 1) {
	
	main_html |>
		html_elements(xpath = "//*[@class='q-player-info']") %>%
		xml_text() %>%
		.[2*i]
	
}

team <- function(i = 1) {
	
	main_html |>
		html_elements(xpath = "//*[@class='q-player-info']") %>%
		xml_text() %>%
		.[2*i - 1]
	
}

to_tg_table <- function(table,
												align = NULL,
												indents = 3,
												parse_mode = "Markdown") {
	
	# If the alignment is not set, then align to the left
	if (is.null(align)) {
		colNum <- length(table)
		align   <- str_c(rep("l", colNum), collapse = "")
	}
	# Check if the alignment is set correctly
	if (length(table) != nchar(align)) {
		align <- NULL
	}
	
	# New column alignment 
	side <- sapply(1:nchar(align), 
								 function(x) { 
								 	letter <- substr(align, x, x)
								 	switch(letter,
								 				 "l" = "right",
								 				 "r" = "left",
								 				 "c" = "both",
								 				 "left"
								 	)
								 })
	
	# Saving names
	tNames <- names(table)
	
	# Calculation width column
	namesLength <- sapply(tNames, nchar) 
	valueLength <- sapply(table, function(x) max(nchar(as.character(x))))
	maxLength   <- ifelse(valueLength > namesLength, valueLength, namesLength)
	
	# We adjust the size of the column names to their width + the number of spaces specified in the indents 
	tNames <- mapply(str_pad, 
									 string = tNames, 
									 width  = maxLength + indents, 
									 side   = side)
	
	# Unite names of column
	strNames <- str_c(tNames, collapse = "")
	
	# Args for a function str_pad
	rules <- list(string = table, width = maxLength + indents, side = side)
	
	# In turn, we translate each column to the desired form
	tStr <-   pmap_df(rules, str_pad) %>%
		unite("data", everything(), remove = TRUE, sep = "") %>%
		unlist(data) %>%
		str_c(collapse = "\n") 
	
	# If the table takes up more than 4096 characters, we crop it
	if (nchar(tStr) >= 4021) {
		warning("Таблица составляет более 4096 символов!")
		tStr <- substr(tStr, 1, 4021)
	}
	
	# Code block selection symbols according to the selected markup
	codeBlock <- switch(parse_mode, 
											"Markdown" = c("```", "```"),
											"HTML" = c("`", "`"))
	
	# Translating to code
	res <- str_c(codeBlock[1], strNames, tStr, codeBlock[2], sep = "\n")
	
	return(res)
} # Function for translation data.frame in telegram table
