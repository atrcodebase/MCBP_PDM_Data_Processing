### Custom Functions
apply_log <- function (data, log, data_KEY = "KEY", log_columns = c(question = "question", 
                                                                    old_value = "old_value", new_value = "new_value", KEY = "KEY")) 
{
  if(nrow(log) == 0){
    print(paste0("No logs under: ", deparse(substitute(data))))
    return(data)
  } else {
    tryCatch(
      # Specifying expression
      expr = {					
        for (rowi in 1:nrow(log)) {
          var_i <- log[[log_columns[["question"]]]][rowi]
          old_i <- log[[log_columns[["old_value"]]]][rowi]
          new_i <- log[[log_columns[["new_value"]]]][rowi]
          uuid_i <- log[[log_columns[["KEY"]]]][rowi]
          if (var_i %in% colnames(data)) {
            var_type <- class(data[[var_i]])
            if (var_type %in% "character") {
              data[data[[data_KEY]] %in% uuid_i, var_i] <- as.character(new_i)
            }
            else {
              data[data[[data_KEY]] %in% uuid_i, var_i] <- as.numeric(new_i)
            }
          }
        }
        return(data)
      },
      # Specifying error message
      error = function(e){		
        stop(paste("uuid:", uuid_i, "Old value: ", old_i,
                   "changed to", new_i, "for", var_i), call. = FALSE)
      }
    )
  }
}
# Apply Translation Log: 
apply_translation_log <- function (data, log, data_KEY = "KEY", log_columns = c(question = "question", 
                                                                    old_value = "old_value", new_value = "new_value", KEY = "KEY")) 
{
  if(nrow(log) == 0){
    print(paste0("No logs under: ", deparse(substitute(data))))
    return(data)
  } else {
    tryCatch(
      # Specifying expression
      expr = {
        # New: changed the translation column to character before applying log
        data <- data %>% mutate(across(all_of(unique(log$question)), as.character))
        #
        for (rowi in 1:nrow(log)) {
          var_i <- log[[log_columns[["question"]]]][rowi]
          old_i <- log[[log_columns[["old_value"]]]][rowi]
          new_i <- log[[log_columns[["new_value"]]]][rowi]
          uuid_i <- log[[log_columns[["KEY"]]]][rowi]
          if (var_i %in% colnames(data)) {
            var_type <- class(data[[var_i]])
            if (var_type %in% "character") {
              data[data[[data_KEY]] %in% uuid_i, var_i] <- as.character(new_i)
            }
            else {
              data[data[[data_KEY]] %in% uuid_i, var_i] <- as.numeric(new_i)
            }
          }
        }
        return(data)
      },
      # Specifying error message
      error = function(e){		
        stop(paste("uuid:", uuid_i, "Old value: ", old_i,
                   "changed to", new_i, "for", var_i), call. = FALSE)
      }
    )
  }
}

# Compares dataframes and logs the changes
compare_dt <- function (df1, df2, unique_id_df1, unique_id_df2, compare_all = TRUE) 
{
  if (compare_all == FALSE) {
    df1 <- df1[, colnames(df1) %in% colnames(df2)]
    df2 <- df2[, colnames(df2) %in% colnames(df1)]
  }
  if ("KEY" %in% colnames(df1) && unique_id_df1 != "KEY") {
    df1 <- df1 %>% rename(key = KEY)
  }
  df1 <- df1 %>% select(KEY = all_of(unique_id_df1), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_1") %>% mutate(value_1 = str_squish(value_1))
  if ("KEY" %in% colnames(df2) && unique_id_df2 != "KEY") {
    df2 <- df2 %>% rename(key = KEY)
  }
  df2 <- df2 %>% select(KEY = all_of(unique_id_df2), everything()) %>% 
    mutate(across(-KEY, function(x) x = as.character(x))) %>% 
    pivot_longer(-KEY, values_to = "value_2") %>% mutate(value_2 = str_squish(value_2))
  df_both <- full_join(df1, df2, by = c("KEY", "name"))
  diff <- df_both %>% filter((value_1 != value_2) | (is.na(value_1) & 
                                                       !is.na(value_2)) | (!is.na(value_1) & is.na(value_2))) %>% 
    rename(question = name, old_value = value_1, new_value = value_2) %>% 
    mutate(question = ifelse(question == "key", "KEY", question))
  if (nrow(diff) == 0) {
    paste0("No difference in df1 and df2")
    return(diff)
  }
  else {
    return(diff)
  }
}

# Relevancy Function -------------------------------------------------------------------------------
check_relevancy_rules <- function(data, tool_relevancy, sheet_name, KEY="KEY"){
  # initiate Log
  `%notin%` <- Negate(`%in%`)
  relevancy_log <- data.frame()
  missing_cols <- c()
  missing_relev_cols <- c()
  
  # Filter the rules
  tool_relevancy <- tool_relevancy %>% filter(sheet==sheet_name)
  
  # Loop through relevancy rules
  questions <- tool_relevancy$name
  for(question_i in 1:length(questions)){
    question <- questions[question_i]
    relevant_question <- str_split(tool_relevancy$relevant_question[question_i], " - ")[[1]] %>% unique()
    check_reverse <- tool_relevancy$check_reverse[question_i]
    relevancy_sub <- tool_relevancy[tool_relevancy$name == question,]
    
    # Skip if question is missing
    if(question %notin% names(data)){
      missing_cols <- c(missing_cols, question)
      next
    } else if(any(!relevant_question %in% names(data))){
      missing_relev_cols <- c(missing_relev_cols, relevant_question[!relevant_question %in% names(data)])
    }
    
    # Conditional string
    conditional_string <- relevancy_sub$Rcondition
    conditional_str_negated <- relevancy_sub$Rcondition %>% paste0("!(", ., ")") #Negate
    
    ## Flag issues
    # Rows where Question has a value but relevant question does not apply 
    flagged_rows <- which(data[[question]] %notin% c(NA, "", NaN) & eval(parse(text=conditional_str_negated)))
    # Rows where Relevant Question applies but the actual question is null
    if(check_reverse){
      flagged_rows <- c(
        flagged_rows,
        which(data[[question]] %in% c(NA, "", NaN) & eval(parse(text=conditional_string)))
      )}
    
    # Log if rows are flagged
    len_flagged <- length(flagged_rows)
    if(len_flagged > 0){
      # Get the values of relevant questions
      relevant_values <- data[flagged_rows, c(KEY,relevant_question)] %>%
        pivot_longer(-all_of(KEY), names_to = "cols", values_to = "value", values_transform=as.character) %>% 
        group_by(across(KEY)) %>% mutate(total = paste0(value, collapse = " - "), value=NULL, cols=NULL) %>% # Summarize messed up the group order
        ungroup() %>% unique() %>% pull(total)
      
      log <- data.frame(KEY=data[[KEY]][flagged_rows],
                        question=rep(question, len_flagged),
                        value=data[[question]][flagged_rows],
                        relevancy_rule=rep(relevancy_sub$relevance_rule[1], len_flagged),
                        relevant_question=rep(paste0(relevant_question, collapse = " - "), len_flagged),
                        relev_value=relevant_values,
                        sheet=sheet_name)
      # qa_status=data$qa_status[flagged_rows])
      # Rbind 
      relevancy_log <- rbind(relevancy_log, log)
    }
  }
  ## Print Columns missing from dataset
  if(length(missing_cols) != 0){
    message("Column missing from dataset: ")
    print(unique(missing_cols))
  }
  if(length(missing_relev_cols) != 0){
    message("Relevant column missing from dataset: ")
    print(unique(missing_relev_cols))
  }
  
  ## Print if no relevancy issues found
  if (nrow(relevancy_log) == 0) {
    print(paste0("No relevancy issues found in: ", sheet_name))
  }
  # End
  return(relevancy_log)
}

## Update Select_multiple series columns 
# Updates a single column
update_series_cols_1 <- function(data, question, series_cols, question_separator){
  
  # Make all series cols numeric
  data <- data %>% mutate(across(all_of(series_cols), as.numeric))
  
  # Get rows with non-NA values
  rows <- which(!is.na(data[[question]]))
  na_rows <- which(is.na(data[[question]]))
  
  # Loop each series column
  for(column in series_cols){
    # Add word boundary for str_detect (differentiate 1 from 13)
    response <- paste0("\\b", gsub(paste0(question, question_separator), "", column),"\\b")
    # Assign 1 if value exists in main question, else 0
    data[rows, column] <- ifelse(str_detect(data[[question]][rows], response), 1, 0)
    # Make the rest of the rows na
    data[na_rows, column] <- NA_integer_
  }
  return(data)
}
# Updates multiple columns
update_series_cols <- function(data, tool_path, question_separator, excluded_col=""){
  # Read & Filter tool
  tool <- read_excel(tool_path, "survey", guess_max = 100000)
  sm_cols <- tool$name[grepl("select_multiple", tool$type) & tool$name %in% names(data)]
  # Exclude problematic columns
  sm_cols <- sm_cols[sm_cols %notin% excluded_col]
  
  for(question in sm_cols){
    # print(paste0("Updating: ", question)) # Print
    ## Get all series columns
    series_cols <- names(data)[grepl(paste0("^",question, question_separator, "[0-9]{1,4}$"), names(data))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    
    # Update Series columns for the question
    data <- update_series_cols_1(data= data,
                                 question = question,
                                 series_cols = series_cols,
                                 question_separator= question_separator)
  }
  return(data)
}

## Check Series Cols
check_select_multiple <- function(data, tool_path, question_separator, KEY="KEY", excluded_col=""){
  # Read & Filter tool
  tool <- read_excel(tool_path, "survey", guess_max = 100000)
  sm_cols <- tool$name[grepl("select_multiple", tool$type) & tool$name %in% names(data)]
  # Exclude problematic columns
  sm_cols <- sm_cols[sm_cols %notin% excluded_col]
  
  series_log <- data.frame(KEY=NA,question=NA,value=NA,series_columns=NA, series_values=NA,Remarks=NA)
  for(question in sm_cols){
    # print(paste0("Checking: ", question)) # Print
    # Get all series columns
    series_cols <- names(data)[grepl(paste0("^",question, question_separator, "[0-9]{1,4}$"), names(data))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    # Filter NA responses
    data_sub <- data %>% 
      select(all_of(question), all_of(series_cols), all_of(KEY)) %>% 
      filter(!is.na(get(question)))
    
    if(nrow(data_sub)!=0){
      for(i in 1:nrow(data_sub)){
        #question value
        val <- str_split(data_sub[[question]][i], " |-")[[1]]
        # make related series column name
        series_columns <- paste0(question,question_separator, val)
        other_columns <- names(data_sub)[names(data_sub) %notin% c(series_columns, question, "KEY")]
        
        if(!all(series_columns %in% names(data_sub))){
          log <- c(data_sub$KEY[i], 
                   question, 
                   data_sub[[question]][i], 
                   paste0(series_columns, collapse = " - "),
                   "", 
                   Remarks="Series column not in data")
          series_log <- rbind(series_log, log)
        } else if(any(data_sub[i,series_columns] %in% c(NA, 0))){
          log <- c(data_sub$KEY[i], 
                   question, 
                   data_sub[[question]][i], 
                   paste0(series_columns, collapse = " - "),
                   paste0(data_sub[i,series_columns], collapse = " - "),
                   Remarks = "Inonsistent series columns")
          series_log <- rbind(series_log, log)
        } else if(any(data_sub[i, other_columns] %in% 1)){
          
          other_cols <- other_columns[which(data_sub[i, other_columns] %in% 1)]
          log <- c(data_sub$KEY[i], 
                   question, 
                   data_sub[[question]][i], 
                   paste0(other_cols, collapse = " - "),
                   paste0(data_sub[i,other_cols], collapse = " - "),
                   Remarks = "At least one response is not in the tool choices")
          series_log <- rbind(series_log, log)
        }
      }
    }
  }
  if(nrow(series_log) == 1){
    print(paste0("No mismatches found: ", deparse(substitute(data))))
    return(series_log[-1,])
  } else {
    return(series_log[-1,])
  }
}

# Logs all the data points that are not translated
missing_translation <- function(data, KEY, excluded_cols){
  question <- c(); old_value <- c(); uuid <- c()
  data_cols <- colnames(data)[colnames(data) %notin% excluded_cols]
  # special_characters <- "–|’|é|ý|\\(|\\)|\\`|\\~|\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\-|\\+|\\=|\\||\\\\|\\{|\\}|\\[|\\]|\\:|\\;|\\'|\\<|\\>|\\,|\\.|\\?|\\/|\\_|\\.|\\−|\\‘|\\’"
  special_characters <- "–|’|é|ý|\\‘|\\’|\\−"
  #checking each column
  for(col_name in data_cols){
    
    # remove UTF characters to avoid false flagging
    cell_values <- as.character(str_remove_all(data[[col_name]], special_characters))
    # Filter UTF strings
    logical_filter <- Encoding(cell_values) %in% "UTF-8"
    cell_val <- data[[col_name]][logical_filter]
    keys <- data[[KEY]][logical_filter]
    
    # log
    question <- c(question, rep(col_name, length(cell_val)))
    old_value <- c(old_value, cell_val)
    uuid <- c(uuid, keys)
  }
  if(length(question)+length(old_value)+length(uuid) == 0){
    print(paste0("No untranslated data found in: ", deparse(substitute(data))))
    log <- data.frame()
  } else{
    log <- data.frame(question, old_value, new_value=NA, uuid, Remarks=NA) %>% unique()
  }
}

# Update questions with audio links
update_media_links_1 <- function(data, link_cols, download_link, key_col="KEY", rename=FALSE){
  # Loop through each column and recode
  for(col_i in link_cols){
    # File Extensions
    common_file_types <- ".csv$|.m4a$|.amr$|.wav$|.aac$|.mp3$|.jpg$|.ogg$"
    
    # Filter NA values and anything that does not have a proper file extension (value might be changed in the log)
    filtered_index <- which(!is.na(data[[col_i]]) & !grepl(download_link, data[[col_i]]) & grepl(common_file_types, data[[col_i]])) 
    new_keys <- str_replace(data[[key_col]][filtered_index], "uuid:", "?uuid=uuid%3A")
    
    ## Replace with cto link
    data[[col_i]] <- str_remove(data[[col_i]], "File skipped from exports: ") # Not using str_replace because some may only have the file name
    
    data[[col_i]][filtered_index] <- paste0(download_link, data[[col_i]][filtered_index])
    data[[col_i]][filtered_index] <- paste0(data[[col_i]][filtered_index], new_keys) %>% str_squish()
    
    ## Rename the new column if asked
    if(rename){
      ncol_i <- paste0("n", col_i) # New name
      names(data)[names(data) == col_i] <- ncol_i
    }
  }
  return(data)
}

update_media_links <- function(data, tool_path, download_link="https://artftpm.surveycto.com/view/submission-attachment/", key_col="KEY", rename=FALSE){
  # Data types with download link
  link_types <- c("image", "audio", "audio audit", "text audit")
  # Read & Filter tool
  tool <- read_excel(tool_path, "survey", guess_max = 100000)
  link_cols <- tool %>% filter(type %in% link_types & name %in% names(data)) %>% pull(name)
  
  # Add URL links to each file
  data <- update_media_links_1(data, link_cols, download_link, key_col, rename)
  
  # Return
  return(data)
}


labeler <-function (data, tool, survey_label = "label::English", choice_lable = "label::English", 
                    multi_response_sep = ";") 
{
  survey_questions <- read_excel(tool, "survey", guess_max = 100000)
  survey_choices <- read_excel(tool, "choices", guess_max = 100000)
  if ("value" %in% names(survey_choices)) {
    names(survey_choices)[names(survey_choices) == "value"] <- "name"
  }
  if ("list name" %in% names(survey_choices)) {
    names(survey_choices)[names(survey_choices) == "list name"] <- "list_name"
  }
  survey_choices$name <- gsub("\\.0", "", survey_choices$name)
  survey_questions <- survey_questions[grepl("\\bselect_", 
                                             survey_questions$type), ]
  survey_questions$select_type <- survey_questions$type %>% 
    str_replace_all(" .*", "")
  survey_questions$type <- survey_questions$type %>% str_replace_all("select_one ", 
                                                                     "") %>% str_replace_all("select_multiple ", "")
  survey_questions <- survey_questions %>% select(type, name, 
                                                  select_type, all_of(survey_label))
  survey_choices$name <- survey_choices$name %>% as.character
  survey_choices <- survey_choices[!is.na(survey_choices$list_name), 
  ]
  for (var in names(data)) {
    if (var %in% survey_questions$name) {
      survey_choices_i <- survey_choices[survey_choices$list_name %in% 
                                           survey_questions$type[survey_questions$name %in% 
                                                                   var], ]
      add_underscore <- function() {
        index <- gregexpr("[0-9]", survey_choices_i[[choice_lable]])
        regmatches(survey_choices_i[[choice_lable]], 
                   index) <<- lapply(regmatches(survey_choices_i[[choice_lable]], 
                                                index), function(x) paste0("_", x, "_"))
      }
      add_underscore()
      if (survey_questions$select_type[survey_questions$name %in% 
                                       var] == "select_one") {
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>% str_replace_all(paste0("\\b", 
                                                                survey_choices_i$name[choice_i], "\\b"), 
                                                         survey_choices_i[[choice_lable]][choice_i]%>% str_squish())
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", 
                                                       "")
      }
      else if (survey_questions$select_type[survey_questions$name %in% 
                                            var] == "select_multiple") {
        data[[var]] <- data[[var]] %>% str_replace_all("  ", 
                                                       " ") %>% str_replace_all(" ", paste0(multi_response_sep))
        for (choice_i in 1:nrow(survey_choices_i)) {
          data[[var]] <- data[[var]] %>% str_replace_all(paste0("\\b", 
                                                                survey_choices_i$name[choice_i], "\\b"), 
                                                         survey_choices_i[[choice_lable]][choice_i] %>% str_squish())
        }
        data[[var]] <- data[[var]] %>% str_replace_all("_", 
                                                       "")
      }
    }
  }
  return(data)
}

check_path <- function(path){
  if (!file.exists(path)) {
    dir.create(path, showWarnings = TRUE, recursive = TRUE)
    cat("Created '", path, "' folder")
  } else {
    cat("The '",path,"' folder already exists")
  }
}

# Add custom styling and export datasets
export_datasets <- function(data, file_path, font_name="Arial", font_size=9, header_color="#DCE6F1", text_dec="BOLD"){
  # Formatting
  header_style = createStyle(fontName = font_name, fontSize = font_size, textDecoration = c(text_dec), fgFill = header_color)
  body_style = createStyle(fontName = font_name, fontSize = font_size)
  
  # Create workbook
  wb = createWorkbook()
  wb_sheets <- names(data)
  
  # Apply formatting on each sheet
  for(sheet in wb_sheets){
    data_sub <- data[[sheet]]
    
    addWorksheet(wb, sheet)
    
    df_ncols = length(data_sub)
    df_nrows = nrow(data_sub)
    
    addStyle(wb, sheet=sheet, header_style, rows = 1, cols = 1:df_ncols, gridExpand = TRUE)
    if(df_nrows > 0){
      addStyle(wb, sheet=sheet, body_style, rows = 2:df_nrows, cols = 1:df_ncols, gridExpand = TRUE)
    }
    writeData(wb, sheet, data_sub)
  }
  saveWorkbook(wb, file_path, overwrite = TRUE)
} 

# Checks Audio, Image and translation columns
log_questions <- function(data, columns, columns_different="", key_col="KEY", suffix, sheet){
  # QA Image status to exclude
  image_qa_status <- c("Checked & Verified", "Checked - Irrelevant Photo", "Checked - Blur/Not Visible Photo", 
                       "Checked - Photo Not Visible at the Health Facility", "Checked - Irrelevant Photo Document not exist",
                       "Checked - Irrelevant Photo FR Uploaded Wrong Photo", "Checked - Irrelevant Photo Item not exist",
                       "Checked - Doesn't have Geopoint", "Checked - Photo missed due to form Version",
                       # New
                       "Verified", "Error/Irrelevant"
  ) # New
  # Standard download link 
  # download_links <- "https://atrconsultingaf.surveycto.com/view/submission-attachment/"
  download_links <- "https://artftpm.surveycto.com/view/submission-attachment/"
  common_file_types <- c(".csv", ".m4a", ".amr", ".wav", ".aac", ".mp3", ".jpg", ".ogg")
  # Translation texts to flag
  audio_issues <- c("No clear voice", "Not clear voice", "No audio", "Not audible", "Unclear audio",
                    "No clear audio", "No clear words", "NO VOICE", "inaudible", "Unclear language", 
                    "UNKNOWN LANGUAGE", "Unclear response", "NO ANSWER")  
  question_log <- data.frame()
  for(col in columns){
    
    # Add Translation/QA to question name 
    if(col %in% names(columns_different)){
      # If Translation/QA column name is different, get it from columns_different
      calculated_col <- columns_different[[col]]
    } else {
      calculated_col <- paste0(col, "_", suffix)
    }
    
    # Log if question is missing
    if(calculated_col %notin% names(data)){
      message(calculated_col, " not found in the dataset!")
    }
    
    ### Flag 
    flagged_data <- list(
      "Incorrect download link"=which(!is.na(data[[col]]) & (!grepl(download_links, data[[col]]) | 
                                                               !grepl("?uuid=uuid%3A", data[[col]]) | 
                                                               !grepl(paste0(common_file_types, collapse = "|"), data[[col]])) & 
                                        data[[col]] %notin% "No_audio_received_from_the_field/Translation_is_from_a_callback"
      ),
      "Download link is missing"=which(data[[calculated_col]] %notin% c(NA, "", "NA") & is.na(data[[col]]))
    )
    # Question specific checks
    if(suffix %in% c("QA", "qa_status")){ # New
      flagged_data[["Image QA status is incorrect or not added"]] <- which(!is.na(data[[col]]) & data[[calculated_col]] %notin% image_qa_status)
    } else {
      flagged_data[["Audio Translation is missing"]] <- which(!is.na(data[[col]]) & data[[calculated_col]] %in% c(image_qa_status, "-", NA, "", "NA", ".", "--", "---", ".."))
      flagged_data[["Unclear voice/audio"]] <- which(!is.na(data[[col]]) & grepl(paste0(audio_issues, collapse = "|"), data[[calculated_col]], ignore.case = TRUE))
      flagged_data[["MANUAL_REVIEW"]] <- which(!is.na(data[[col]]) & 
                                                 str_length(data[[calculated_col]]) < 20 & 
                                                 data[[calculated_col]] %notin% c(image_qa_status, "-", NA, "", "NA", ".", "--", "---", "..") &
                                                 !grepl(paste0(audio_issues, collapse = "|"), data[[calculated_col]], ignore.case = TRUE))
    }
    
    # Covert to data.frame
    flagged_data <- plyr::ldply(flagged_data, data.frame) 
    names(flagged_data) = c("issue", "rows") # Rename
    flagged_data <- flagged_data %>% 
      group_by(rows) %>% 
      mutate(issue = paste0(issue, collapse = " & ")) %>% ungroup() # Combine issues
    
    # Log
    if(nrow(flagged_data) != 0){
      log <- data.frame(KEY=data[[key_col]][flagged_data$rows],
                        question=calculated_col,
                        value=data[[calculated_col]][flagged_data$rows],
                        download_link=data[[col]][flagged_data$rows],
                        question_type=suffix,
                        sheet=sheet,
                        issue=flagged_data$issue)
      question_log <- rbind(question_log, log)
    }
  }
  return(question_log)
}

# Logs any responses not in the tool
check_responses <- function(data, tool_path, sheet, excluded_cols=""){
  # Read tool
  tool_survey <- read_excel(tool_path, "survey", guess_max = 100000)
  tool_choices <- read_excel(tool_path, "choices", guess_max = 100000)
  
  # Filter select_one/multiple questions
  tool_survey <- tool_survey %>% 
    select(type, Question=name) %>% 
    filter(grepl("\\bselect_", type) & Question %notin% excluded_cols) %>% 
    mutate(select_type = str_replace_all(type, " .*", ""),
           type = str_replace_all(type, "select_one ", "") %>% str_replace_all("select_multiple ", "")) 
  tool_choices <- tool_choices %>% 
    filter(!is.na(list_name) & !(list_name %in% label & list_name %in% label)) %>% 
    select(list_name, response_code=value, response_label=label) %>% 
    mutate(response_label=str_squish(response_label)) # New
  
  # Join questions & responses
  tool_survey <- tool_survey %>% 
    left_join(tool_choices, by=c("type"="list_name"), relationship="many-to-many") %>% 
    select(-c(type)) %>% 
    filter(!is.na(response_code) & !is.na(response_label))
  # Filter all the select_one/multiple questions in the dataset
  questions <- tool_survey %>% filter(Question %in% names(data)) %>% pull(Question) %>% unique()
  
  # Stop if 
  if(length(questions) == 0){
    print(paste0("No select_one/multiple questions in: ", deparse(substitute(data))))
    return(data.frame())
  } else {
    # 
    response_log <- data.frame()
    for(col_i in questions){
      tool_sub <- tool_survey %>% filter(Question %in% col_i)
      
      if("select_multiple" %in% tool_sub$select_type){
        filtered_index <- which(!is.na(data[[col_i]]))
        flagged_rows <- c()
        for(row_i in filtered_index){
          sm_values <- str_split_1(data[[col_i]][row_i], ";")
          
          if(any(sm_values %notin% tool_sub$response_label)){
            flagged_rows <- c(flagged_rows, row_i)
          }
        }
        
      } else {
        flagged_rows <- which(data[[col_i]] %notin% c(tool_sub$response_label, NA))
      }
      
      # data[[col_i]][!is.na(data[[col_i]])] %>% str_split(";")
      
      # Log
      if(length(flagged_rows) != 0){
        log <- data.frame(KEY=data$KEY[flagged_rows],
                          question=col_i,
                          value=data[[col_i]][flagged_rows],
                          issue="Value not in the tool",
                          sheet=sheet)
        response_log <- rbind(response_log, log)
      }
    }
    if(nrow(response_log) == 0){
      print("All responses match with the Tool!")
    } 
    return(response_log) 
  }
}

# Move Files to a different path
archive_datasets <- function(source_folder){
  # Define source and destination folders
  destination_folder <- paste0(source_folder, "/Archive/")
  # Check if destination exists else create
  check_path(destination_folder)
  
  # List all files in the source folder
  files <- list.files(source_folder, pattern = "\\.xlsx?$")
  
  if(length(files)!=0){
    # Move all files to the destination folder
    file.rename(file.path(source_folder, files), file.path(destination_folder, files))
  }
}

# Analysis related ---------------------------------------------------------------------------------
# Check question names in AP with question/column names in data
## custom function
check_ap_questions_with_data_columns <- function(ap, dt) {
  `%notin%` <- Negate(`%in%`)
  unmatched_questions <- c(
    ap$variable[ap$variable %notin% names(dt)],
    ap$disaggregation[ap$disaggregation %notin% c("all", names(dt))],
    ap$repeat_for[ap$repeat_for %notin% c("NA", NA, names(dt))]
  ) %>% unique()
  
  if (length(unmatched_questions) == 0) {
    print("All questions in DAP match with questions/columns in data")
  } else {
    print("----Below questions in DAP do not match with questions/column in data")
    print(unmatched_questions)
  }
}


remove_html_tags <- function(tool, column="label"){
  for (i in 1:length(tool[[column]])) {
    value = tool[[column]][i]
    tool[[column]][i] <- ifelse(grepl("<", value), html_text(read_html(value)), value)
  }
  
  return(tool)
}

# Reshape tool for select multiple questions
reshape_tool <- function(tool_path, excluded_cols=""){
  # Read tool
  tool_survey <- read_excel(tool_path, "survey", guess_max = 100000)
  tool_choices <- read_excel(tool_path, "choices", guess_max = 100000)
  
  # Filter select_one/multiple questions
  tool_survey <- tool_survey %>%
    select(type, Question=name) %>%
    filter(grepl("\\bselect_multiple", type) & Question %notin% excluded_cols) %>%
    mutate(select_type = str_replace_all(type, " .*", ""),
           type = str_replace_all(type, "select_one ", "") %>% str_replace_all("select_multiple ", ""))
  tool_choices <- tool_choices %>%
    filter(!is.na(list_name) & !(list_name %in% label & list_name %in% label)) %>%
    select(list_name, response_code=value, response_label=label) %>%
    mutate(response_label=str_squish(response_label),
           response_label_new=str_replace_all(response_label, " ", "_"),
           response_code=as.numeric(response_code)) # New
  
  # Join questions & responses
  tool_survey <- tool_survey %>%
    left_join(tool_choices, by=c("type"="list_name"), relationship="many-to-many") %>%
    filter(!is.na(response_code) & !is.na(response_label)) %>%
    mutate(dataset_col= paste0(Question, "_", response_code),
           labeled_col = paste0(Question, "/", response_label_new))
  # Return
  return(tool_survey)
}

# Apply new labels for Select Multiple series columns 
apply_SM_Label <- function(client_dt, tool, excluded_cols=""){
  
  # Get select multiple columns
  sm_cols <- unique(tool$Question)
  
  for(question in sm_cols){
    # Get all series columns
    series_cols <- names(client_dt)[grepl(paste0("^",question, "_", "[0-9]{1,4}$"), names(client_dt))] # Regex: detect the question ended by 1 to 4 numbers followed by nothing else
    # Exclude main columns from 0s/1s
    series_cols <- series_cols[series_cols %notin% excluded_cols]
    
    # Note: this might not apply accurately, using loop to save time
    # names(client_dt)[names(client_dt) %in% series_cols] <- tool$labeled_col[tool$Question %in% question]
    
    for(col in series_cols){
      names(client_dt)[names(client_dt) %in% col] <- tool$labeled_col[tool$dataset_col %in% col]
    }
    
  }
  
  return(client_dt)
}

# Add escape character before each character
escape_characters <- function(text, new_characters="") {
  # Typical characters that need to be escaped
  characters_to_escape <- c("/", "(", ",", ".", ")", "’", "–", "[", "^", "$", 
                            "?", "*", "+", "}") # , new_characters
  # Escape each character
  for (char in characters_to_escape) {
    text <- str_replace_all(text, fixed(char), paste0("\\", char))
  }
  return(text)
}

# Read all excel sheets and add them to a list
read_xlsx_sheets = function(path){
  guess_max <- 5000000
  convert_to_na <- c("N/A", "-", " ") # values to convert to NA
  df <- purrr::map2(path, getSheetNames(path), ~ readxl::read_excel(.x, sheet = .y, guess_max=guess_max, na=convert_to_na), .progress = TRUE) |> setNames(getSheetNames(path)) # , .name_repair = "minimal"
  if (length(df) == 1) df <- data.frame(unname(df))
  return(df)
} 

