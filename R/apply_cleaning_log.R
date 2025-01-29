# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
tabs <- c("data", "children_under2")
sm_variables <- read_excel(pdm_tool_path) %>% filter(grepl("select_multiple", type)) %>% pull(name)

## Filter empty rows
correction_log_filtered <- correction_log %>%
  filter(!(is.na(KEY_Unique) & is.na(Question) & is.na(old_value))) %>%
  mutate(New_Value = case_when(
    Question %in% sm_variables ~ str_replace_all(New_Value, "-|,|  | - ", " ") %>% str_squish(),
    TRUE ~ str_squish(New_Value)
  ),
  Tool = "PDM", Log_type = "Correction_Log" # QA: logs only added for IE & not representative
  # KEY= case_when(
  #   is.na(KEY) & !is.na(`Full_ KEY`) ~ str_squish(`Full_ KEY`),
  #   TRUE ~ str_squish(KEY)
  # )
  ) %>% 
  select(key=KEY, KEY=KEY_Unique, Tool, Tab_Name, question=Question, old_value, 
         new_value=New_Value, QAed_by=`Logged by:`, Remarks, Log_type)

# New Log: should be applied before correction log
detailed_check_log <- detailed_check %>% 
  mutate(Question=case_when(
    Check_Type %in% c("image") ~ paste0(Question, "_qa_status"),
    Check_Type %in% c("audio") ~ paste0(Question,"_translation"),
    TRUE ~ as.character(Question)
  ),
  New_Value=case_when(
    Check_Status %in% "Verified" & Check_Type %in% c("audio", "image") ~ "Verified",
    Check_Status %in% "Error/Irrelevant" & Check_Type %in% c("audio", "image") ~ "Error/Irrelevant",
    TRUE ~ as.character(New_Value)
  ), Tool="PDM", Log_type = "Detailed_Check") %>% 
  filter(Check_Type %notin% "audio audit") %>% # Audio Audit is only used for QA)
  filter(Check_Status %in% "Error/Irrelevant" | Check_Type %in% c("audio", "image")) %>% 
  select(key=KEY, KEY=KEY_Unique, Tab_Name, Check_Type, QAed_by=`Qa'd by:`, 
         question=Question, old_value=Value, new_value=New_Value, Check_Status, Tool, Log_type)

# Merge logs
correction_log_filtered <- plyr::rbind.fill(
  detailed_check_log, # should be applied first
  correction_log_filtered
  # fcs_log # Added in log already
)

# Identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    # is.na(Tools) & Tools %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    Tab_Name %in% "data" & question %notin% names(pdm_dt$data) ~ "question",
    Tab_Name %in% "data" & KEY %notin% pdm_dt$data$KEY ~ "KEY",
    Tab_Name %in% "children_under2" & question %notin% names(pdm_dt$children_under2) ~ "question",
    Tab_Name %in% "children_under2" & KEY %notin% pdm_dt$children_under2$KEY ~ "KEY",
    )) # Add tool name based on Log names

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")], fromLast = T) | duplicated(correction_log_filtered[, c("KEY", "question", "Log_type")])

# Filter issues
correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question) %>%
  mutate(question=case_when( # Temp
    Check_Type %in% "image" ~ str_remove(question, "_qa_status$"),
    Check_Type %in% "audio" ~ str_remove(question, "_translation$"),
    TRUE ~ question
  ))

correction_log_filtered <- correction_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue)) 

# Make audio translation columns character
PDM_media_cols <- read_excel(pdm_tool_path, "survey", guess_max = 100000)
PDM_media_cols <- PDM_media_cols %>% 
  filter(type %in% c("audio", "image")) %>% 
  mutate(name= case_when(
    type %in% c("image") ~ paste0(name, "_qa_status"),
    type %in% c("audio") ~ paste0(name,"_translation"),
  )) %>% pull(name) 

pdm_dt$data <- pdm_dt$data %>%
  mutate(across(any_of(PDM_media_cols), as.character))
pdm_dt$children_under2 <- pdm_dt$children_under2 %>%
  mutate(across(any_of(PDM_media_cols), as.character))

# Temp: manually fix the one date value
pdm_dt$children_under2 <- pdm_dt$children_under2 %>% 
  mutate(please_specify_the_date=case_when(
    KEY %in% "uuid:bb8e658d-9530-461d-ad15-fd34b2c4ca03/Passcode_correct-Valid_User-form_questions-beneficiary_registered-section_8-children_under2[1]" ~ as.Date("2024-10-01"),
    TRUE ~ please_specify_the_date
  ))
# Exclude from log    
correction_log_filtered <- correction_log_filtered %>% 
  filter(!(KEY %in% "uuid:bb8e658d-9530-461d-ad15-fd34b2c4ca03/Passcode_correct-Valid_User-form_questions-beneficiary_registered-section_8-children_under2[1]" & 
           question %in% "please_specify_the_date"))


# apply the correction-log -------------------------------------------
## PDM
pdm_dt_copy <- pdm_dt

# Data
pdm_dt$data <- apply_log(data = pdm_dt$data, log= filter(correction_log_filtered, Tab_Name %in% "data"), 
                                    data_KEY = "KEY",
                                    log_columns = c(question = "question",
                                                    old_value = "old_value",
                                                    new_value = "new_value",
                                                    KEY = "KEY"))

# children_under2
pdm_dt$children_under2 <- apply_log(data = pdm_dt$children_under2, log= filter(correction_log_filtered, Tab_Name %in% "children_under2"),
                         data_KEY = "KEY",
                         log_columns = c(question = "question",
                                         old_value = "old_value",
                                         new_value = "new_value",
                                         KEY = "KEY"))



# Verify correction log ----------------------------------------------------------------------------
message("Verifying Correction log, please wait!")

correction_log_discrep <- rbind(
  ## PDM
  compare_dt(df1 = pdm_dt_copy$data, df2 = pdm_dt$data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "PDM", Tab_Name="data"),
  ## children_under2
  compare_dt(df1 = pdm_dt_copy$children_under2, df2 = pdm_dt$children_under2,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "PDM", Tab_Name="children_under2")
  ) 

# Removing extra spaces from new_value before joining 
correction_log_discrep <- correction_log_discrep %>%
  anti_join(correction_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(pdm_dt_copy, sm_variables, tabs, correction_log_filtered, PDM_media_cols)



