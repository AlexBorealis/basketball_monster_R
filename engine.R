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
	
	if (!is_empty(grep(text, pattern = "Off Injury Report"))) {
		
		text_sub_level <- paste0(toupper("Off Injury Report "), gsub(text, pattern = "Off Injury Report", replacement = ""))
		
		} else
			if (!is_empty(grep(text, pattern = "Questionable to return"))) {
		
				text_sub_level <- paste0(toupper("Questionable to return "), gsub(text, pattern = "Questionable to return", replacement = ""))
				
				} else
					if (!is_empty(grep(text, pattern = "Questionable"))) {
						
						text_sub_level <- paste0(toupper("Questionable "), gsub(text, pattern = "Questionable", replacement = ""))
						
						} else
							if (!is_empty(grep(text, pattern = "Doubtful"))) {
								
								text_sub_level <- paste0(toupper("Doubtful "), gsub(text, pattern = "Doubtful", replacement = ""))
								
								} else
									if (!is_empty(grep(text, pattern = "Probable"))) {
										
										text_sub_level <- paste0(toupper("Probable "), gsub(text, pattern = "Probable", replacement = ""))
										
										} else
											if (!is_empty(grep(text, pattern = "Probable to return"))) {
												
												text_sub_level <- paste0(toupper("Probable to return "), gsub(text, pattern = "Probable to return", replacement = ""))
												
												} else
													if (!is_empty(grep(text, pattern = "Playing"))) {
														
														text_sub_level <- paste0(toupper("Playing "), gsub(text, pattern = "Playing", replacement = ""))
														
														} else
															if (!is_empty(grep(text, pattern = "Injured"))) {
																
																text_sub_level <- paste0(toupper("Injured "), gsub(text, pattern = "Injured", replacement = ""))
																
															} else
																if (!is_empty(grep(text, pattern = "To Bench with Injury"))) {
																	
																	text_sub_level <- paste0(toupper("OUT To Bench with Injury "), gsub(text, 
																																																			pattern = "To Bench with Injury",
																																																			replacement = "",
																																																			ignore.case = T))
																	
																} else 
																	if (!is_empty(grep(text, pattern = "ejected"))) {
																		
																		text_sub_level <- paste0(toupper("Out ejected "), gsub(text, pattern = "ejected", replacement = ""))
																		
																	} else
																		if (!is_empty(grep(text, pattern = "In the Locker Room"))) {
																			
																			text_sub_level <- paste0(toupper("OUT In the Locker Room "), gsub(text,
																																																				pattern = "In the Locker Room",
																																																				replacement = "",
																																																				ignore.case = T))
																			
																		} else
																			if (!is_empty(grep(text, pattern = "Will not return"))) {
																				
																				text_sub_level <- paste0(toupper("Out Will not return "), gsub(text,
																																																			 pattern = "Will not return",
																																																			 replacement = ""))
																				
																			} else
																				if (!is_empty(grep(text, pattern = "Available to Return"))) {
																					
																					text_sub_level <- paste0(toupper("Out Available to Return "), gsub(text,
																																																						 pattern = "Available to Return",
																																																						 replacement = ""))
																					
																				} else
																					if (!is_empty(grep(text, pattern = "Note"))) {
																						
																						text_sub_level <- paste0(toupper("Note "), gsub(text,
																																														pattern = "Note",
																																														replacement = ""))
																						
																					} else
																						if (!is_empty(grep(text, pattern = "Off the Bench"))) {
																							
																							text_sub_level <- paste0(toupper("Out Off the Bench "), 
																																			 gsub(text,
																																			 		  pattern = "Off the Bench",
																																			 		  replacement = ""))
																							
																						} else
																							if (!is_empty(grep(text, pattern = "Doubtful to return", ignore.case = F))) {
																								
																								text_sub_level <- paste0(toupper("Doubtful to return "), 
																																				 gsub(text,
																																				 		  pattern = "Doubtful to return",
																																				 		  replacement = "",
																																				 		  ignore.case = F))
																								
																							} else {
																								
																								text_sub_level <- paste0(toupper("Out "), gsub(text,
																																															 pattern = "Out",
																																															 replacement = ""))
																							
																							 }
	
	if (!is_empty(grep(text_sub_level, pattern = "low level"))) {
		
		paste0("low level - ", gsub(text_sub_level, pattern = "low level", replacement = ""))
		
	} else
		if (!is_empty(grep(text_sub_level, pattern = "medium level"))) {
			
			paste0("medium level - ", gsub(text_sub_level, pattern = "medium level", replacement = ""))
			
		} else
			if (!is_empty(grep(text_sub_level, pattern = "high level"))) {
				
				paste0("high level - ", gsub(text_sub_level, pattern = "high level", replacement = ""))
				
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