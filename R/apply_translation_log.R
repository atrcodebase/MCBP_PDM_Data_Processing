# clean the translation log -----------------------------------------------------------------
tabs <- c("data", "beneficiary_verification", "SBCC_Sessions", "Exit_Interviews")
DO_audio_cols <- read_excel(DO_tool_path, "survey", guess_max = 100000) %>% filter(type %in% c("audio")) %>% pull(name) 

## Filter empty rows
translation_log_filtered <- translation_log %>%
  # mutate(Translation=case_when(
  #   !is.na(`Final Translation`) ~ `Final Translation`,
  #   TRUE ~ Translation
  # ), old_value="") %>% 
  mutate(Question= case_when(
    Question %in% DO_audio_cols ~ paste0(Question,"_translation"),
    TRUE ~ Question
  )) %>% # The questions added for Badakhshan didn't need this, check why this was included
  select(key=KEY, KEY=KEY_Unique, Tab_Name, question=Question, old_value, new_value=New_Value)

# Identify issues
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    # is.na(Tool) & Tool %notin% tool_names ~ "Tool name",
    is.na(Tab_Name) | Tab_Name %notin% tabs ~ "Tab name",
    is.na(new_value) ~ "Translation is missing",
    Tab_Name %in% "data" & question %notin% names(direct_obs$data) ~ "question",
    Tab_Name %in% "data" & KEY %notin% direct_obs$data$KEY ~ "KEY",
    Tab_Name %in% "beneficiary_verification" & question %notin% names(direct_obs$beneficiary_verification) ~ "question",
    Tab_Name %in% "beneficiary_verification" & KEY %notin% direct_obs$beneficiary_verification$KEY ~ "KEY",
    Tab_Name %in% "SBCC_Sessions" & question %notin% names(direct_obs$SBCC_Sessions) ~ "question",
    Tab_Name %in% "SBCC_Sessions" & KEY %notin% direct_obs$SBCC_Sessions$KEY ~ "KEY",
    Tab_Name %in% "Exit_Interviews" & question %notin% names(direct_obs$Exit_Interviews) ~ "question",
    Tab_Name %in% "Exit_Interviews" & KEY %notin% direct_obs$Exit_Interviews$KEY ~ "KEY"
  ))

translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("KEY", "question")], fromLast = T) | duplicated(translation_log_filtered[, c("KEY", "question")])

# Filter issues
translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>%
  arrange(KEY, question)

# Filter Nimruz
translation_log_issues <- translation_log_issues %>% 
  left_join(
    select(qa_log, key=KEY_Unique, Province)
  ) %>% filter(Province %notin% "Nimroz")

# # translation_log_filtered %>% 
# direct_obs$beneficiary_verification %>% 
#   filter(KEY %in% 
#            "uuid:5a74bb7b-54d4-4d43-9dd6-d0e3c1bcbd8a/Passcode_correct-Valid_User-Disbursement-Consent_Group-Distribution_management-Beneficiary_Observation-beneficiary_verification[1]") %>% 
#   select(beneficiary_did_not_provide_a_thumbprint_in_advance, beneficiary_did_not_provide_a_thumbprint_in_advance_translation)

translation_log_filtered <- translation_log_filtered %>% 
  # filter(is.na(issue) & duplicates == FALSE) # Keeping duplicates for now
  filter(is.na(issue))

# apply the Translation log -------------------------------------------
## Direct Observation
direct_obs_copy <- direct_obs

# Data
direct_obs$data <- apply_log(data = direct_obs$data, log= filter(translation_log_filtered, Tab_Name %in% "data"), # Tools == "Direct Observation" &
                             data_KEY = "KEY",
                             log_columns = c(question = "question",
                                             old_value = "old_value",
                                             new_value = "new_value",
                                             KEY = "KEY"))
# beneficiary_verification
direct_obs$beneficiary_verification <- apply_log(data = direct_obs$beneficiary_verification, log= filter(translation_log_filtered, Tab_Name %in% "beneficiary_verification"), 
                                                 data_KEY = "KEY",
                                                 log_columns = c(question = "question",
                                                                 old_value = "old_value",
                                                                 new_value = "new_value",
                                                                 KEY = "KEY"))
# SBCC_Sessions
direct_obs$SBCC_Sessions <- apply_log(data = direct_obs$SBCC_Sessions, log= filter(translation_log_filtered, Tab_Name %in% "SBCC_Sessions"), 
                                      data_KEY = "KEY",
                                      log_columns = c(question = "question",
                                                      old_value = "old_value",
                                                      new_value = "new_value",
                                                      KEY = "KEY"))
# Exit_Interviews
direct_obs$Exit_Interviews <- apply_log(data = direct_obs$Exit_Interviews, log= filter(translation_log_filtered, Tab_Name %in% "Exit_Interviews"), 
                                        data_KEY = "KEY",
                                        log_columns = c(question = "question",
                                                        old_value = "old_value",
                                                        new_value = "new_value",
                                                        KEY = "KEY"))

# Verify Translation log -------------------------------------------
message("Verifying Correction log, please wait!")
translation_log_discrep <- rbind(
  ## Direct Observation
  compare_dt(df1 = direct_obs_copy$data, df2 = direct_obs$data,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Direct_Observation", Tab_Name="data"),
  ## beneficiary_verification
  compare_dt(df1 = direct_obs_copy$beneficiary_verification, df2 = direct_obs$beneficiary_verification,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Direct_Observation", Tab_Name="beneficiary_verification"),
  ## SBCC_Sessions
  compare_dt(df1 = direct_obs_copy$SBCC_Sessions, df2 = direct_obs$SBCC_Sessions,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Direct_Observation", Tab_Name="SBCC_Sessions"),
  ## Exit_Interviews
  compare_dt(df1 = direct_obs_copy$Exit_Interviews, df2 = direct_obs$Exit_Interviews,
             unique_id_df1 = "KEY", unique_id_df2 = "KEY") %>%
    mutate(`Tool Type` = "Direct_Observation", Tab_Name="Exit_Interviews")
  ) 

# Removing extra spaces from new_value before joining 
translation_log_discrep <- translation_log_discrep %>%
  anti_join(translation_log_filtered %>% 
              mutate(new_value = str_squish(new_value)),
            by=c("KEY", "question", "new_value"))

# remove extra objects -----------------------------------------------------------------------------
rm(direct_obs_copy, tabs, translation_log_filtered)



