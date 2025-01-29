# clean the translation log -----------------------------------------------------------------
options(scipen = 999)
tabs <- c("data", "children_under2")
PDM_audio_cols <- read_excel(pdm_tool_path, "survey", guess_max = 100000) %>% filter(type %in% c("audio")) %>% pull(name) 

## Filter empty rows
translation_log_filtered <- translation_log %>%
  select(KEY=KEY_Unique, Tab_Name, question=Question, old_value, new_value=New_Value)

# Identify issues
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    # is.na(Tools) & Tools %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    Tab_Name %in% "data" & question %notin% names(pdm_dt$data) ~ "question",
    Tab_Name %in% "data" & KEY %notin% pdm_dt$data$KEY ~ "KEY",
    Tab_Name %in% "children_under2" & question %notin% names(pdm_dt$children_under2) ~ "question",
    Tab_Name %in% "children_under2" & KEY %notin% pdm_dt$children_under2$KEY ~ "KEY",
  )) # Add tool name based on Log names

translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(translation_log_filtered[, c("KEY", "question")])

# Filter issues
translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question)

# Filter 
# translation_log_issues <- translation_log_issues %>% 
#   left_join(
#     select(qa_log, key=KEY_Unique, Province)
#   ) %>% filter(Province %notin% "Nimroz")

translation_log_filtered <- translation_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue))

# apply the Translation log -------------------------------------------
## PDM
pdm_dt_copy <- pdm_dt

# Data
pdm_dt$data <- apply_log(data = pdm_dt$data, log= filter(translation_log_filtered, Tab_Name %in% "data"), 
                         data_KEY = "KEY",
                         log_columns = c(question = "question",
                                         old_value = "old_value",
                                         new_value = "new_value",
                                         KEY = "KEY"))

# children_under2
pdm_dt$children_under2 <- apply_log(data = pdm_dt$children_under2, log= filter(translation_log_filtered, Tab_Name %in% "children_under2"),
                                    data_KEY = "KEY",
                                    log_columns = c(question = "question",
                                                    old_value = "old_value",
                                                    new_value = "new_value",
                                                    KEY = "KEY"))

# Verify Translation log -------------------------------------------
message("Verifying Translation log, please wait!")
translation_log_discrep <- rbind(
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
translation_log_discrep <- translation_log_discrep %>%
  anti_join(translation_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(pdm_dt_copy, tabs, translation_log_filtered)



