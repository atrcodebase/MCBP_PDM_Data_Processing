##### Data Processing Script #####
# Install/load required packages -------------------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(writexl)) install.packages("writexl")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(janitor)) install.packages("janitor")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")

source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Declaring Global Variables -----------------------------------------------------------------------
pdm_tool_path <- "input/tools/MCBP+PDM+Tool.xlsx"
# Survey CTO Download link extension
download_link <- "https://artftpm.surveycto.com/view/submission-attachment/"

# Read data ----------------------------------------------------------------------------------------
# Post Distribution Monitoring PDM
pdm_dt = read_xlsx_sheets("input/raw_data/MCBP Post Distribution Monitoring Tool.xlsx")

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTTME6dAbkFPwF0qo3aLoUWPwqJhZe5wO8TSgbFqeeAGBOJd0BS88Lr7u44F7lYAcTtkSGcRA7-rwgH/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=0&single=true&output=csv"), col_types = "c")
detailed_check <- readr::read_csv(paste0(url, "gid=588362551&single=true&output=csv"), col_types = "c", guess_max = 500000)
correction_log <- readr::read_csv(paste0(url, "gid=1023075725&single=true&output=csv"), col_types = "c")
# translation_log <- readr::read_csv(paste0(url, "gid=1671390620&single=true&output=csv"), col_types = "c")
rejection_log <- readr::read_csv(paste0(url, "gid=732403193&single=true&output=csv"), col_types = "c")
addition_log <- readr::read_csv(paste0(url, "gid=615345402&single=true&output=csv"), col_types = "c")
# rejection_log <- data.frame()

# detailed_check %>% filter(Check_Type %in% "image") %>% View
# fcs_log <- read_excel("input/FCS_log/PDM_FCS_correction_log.xlsx")

# Join QA Status -----------------------------------------------------------------------------------
count(qa_log, Round, Province, QA_Status)
qa_log_sub <- qa_log %>%
  select(KEY=KEY_Unique, qa_status=QA_Status) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status
  )) %>% unique()

## PDM
pdm_dt$data <- pdm_dt$data %>%
  left_join(filter(qa_log_sub), by="KEY") #%>% select(-Tool)

pdm_dt$data %>% count(Round, Province, District, qa_status)

# add new KEYs -------------------------------------------------------------------------------------
pdm_dt$children_under2 <- pdm_dt$children_under2 %>% 
  bind_rows(
    # Children info added by callback (values added in correction log)
    addition_log %>% 
      mutate(PARENT_KEY = str_split_fixed(KEY_Unique, "/", 2)[,1]) %>%
      select(PARENT_KEY, KEY=KEY_Unique)
  )

# apply correction log -----------------------------------------------------------------------------
correction_log %>% count(Tab_Name)
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R") # Add Community rep if needed
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}

## Remove Rejected data ----------------------------------------------------------------------------
# # file.edit("R/remove_rejected_data.R")
source("R/remove_rejected_data.R")

# Relevancy check ----------------------------------------------------------------------------------
# file.edit("R/check_relevancy_rules.R")
source("R/check_relevancy_rules.R")

## Attach labels -----------------------------------------------------------------------------------
# file.edit("R/attach_labels.R")
source("R/attach_labels.R")

# # apply Translation log ----------------------------------------------------------------------------
# translation_log %>% count(Tab_Name)
# # file.edit("R/apply_translation_log.R")
# source("R/apply_translation_log.R")
# if(nrow(translation_log_discrep) !=0){
#   print("Correction Logs not applied -------------------")
#   correction_log_discrep
# }

## Recode ------------------------------------------------------------------------------------------
# file.edit("R/recode.R") 
source("R/recode.R") # Update here

# produce qa-backlog -------------------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(qa_status=QA_Status, KEY=KEY_Unique) %>% mutate(Tool="PDM")
## Filter
QA_backlog_keys <- rbind(
  # PDM
  pdm_dt$data %>% 
    select(SubmissionDate, KEY) %>%
    left_join(qa_log_sub, by = "KEY") %>% 
    mutate(qa_status = case_when(is.na(Tool) ~ "Not_added_in_qa_log", TRUE ~ qa_status), Tool="PDM")
  ) %>%
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>%
  filter(qa_status %notin% c("Approved", "Rejected")) # Filter Keys not yet QAed
# Count
QA_backlog <- QA_backlog_keys %>%
  group_by(SubmissionDate, Tool) %>% count(qa_status, name = "freq") %>%
  # mutate(percentage = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% arrange(SubmissionDate) %>%
  pivot_wider(names_from = "Tool", values_from = "freq")
# Print
print(knitr::kable(QA_backlog, format = "simple"))

## Filter Approved data ----------------------------------------------------------------------------
pdm_dt$data %>% count(District, Round, qa_status)
pdm_dt$data %>% count(District, phone_response_short) 
approved_qa_status <- c("Approved") # Temp
survey_status <- c("Complete")
# # file.edit("R/filter_approved_data.R")
source("R/filter_approved_data.R")

## Custom Filter -----------------------------------------------------------------------------------
pdm_dt_approved$data <- pdm_dt_approved$data %>%
  filter(Province %in% "Nimroz" & Round %in% 2)
pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>% 
  filter(PARENT_KEY %in% pdm_dt_approved$data$KEY)
pdm_dt_approved$data %>% count(Province, phone_response_short)

## Logic check -------------------------------------------------------------------------------------
# file.edit("R/logic_check.R")
source("R/logic_check.R") # Not added yet
# New Logic checks:
# questions that says the checkups were done in a different district, compare it with the District
# Compare the province and district with the name of sample province and district

## Compare dataset responses with the Tools --------------------------------------------------------
# file.edit("R/dataset_responses_check.R")
source("R/dataset_responses_check.R")

## Remove Extra columns ----------------------------------------------------------------------------
# file.edit("R/remove_extra_columns.R")
source("R/remove_extra_columns.R") 

# generate data with missing translations ----------------------------------------------------------
# file.edit("R/check_missing_translation.R")
source("R/check_missing_translation.R") # Temporary filter for QA at the end

# Anonymize Client Data ---------------------------------------------------------------------------
# file.edit("R/modify_client_data.R")
source("R/modify_client_data.R")

# Export -------------------------------------------------------------------------------------------
## QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)
correction_log_list <- list(
  correction_log=correction_log, 
  detailed_check=detailed_check
  # FCS_Log=fcs_log
)

## export cleaned datasets
check_path("output/cleaned_data") # create the output path
archive_datasets("output/cleaned_data") # Move previous datasets to Archive
writexl::write_xlsx(pdm_dt, paste0("output/cleaned_data/MCBP_PDM_Tool_cleaned_", lubridate::today(), ".xlsx"))

## export client datasets
check_path("output/client_data") # create the output path
archive_datasets("output/client_data") # Move previous datasets to Archive
export_datasets(pdm_dt_approved, paste0("output/client_data/MCBP_PDM_Tool_cleaned_approved_", lubridate::today(), ".xlsx"))

## export additional files
writexl::write_xlsx(correction_log_list, "output/correction_log.xlsx", format_headers = F) # correction
writexl::write_xlsx(correction_log_issues, "output/correction_log_issues.xlsx", format_headers = F) # correction log issues
# writexl::write_xlsx(translation_log_issues, "output/translation_log_issues.xlsx", format_headers = F) # correction log issues
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)
writexl::write_xlsx(relevancy_issues, "output/relevancy_issues.xlsx", format_headers = F)
writexl::write_xlsx(SM_issues, "output/Select_multiple_issues.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_QA_log, "output/Missing_audio_translation_&_image_QA.xlsx", format_headers = F)
writexl::write_xlsx(qa_backlog_list, "output/QA_backlog.xlsx", format_headers = F)
writexl::write_xlsx(response_log_list, "output/dataset_response_mismatch_with_tool.xlsx", format_headers = F)
writexl::write_xlsx(logical_issues_list, "output/Logical_issues.xlsx", format_headers = F) # Add later

