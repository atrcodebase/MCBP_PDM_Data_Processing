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

# Read data ----------------------------------------------------------------------------------------
# PDM Round 1 (base dataset)
pdm_dt_R1 = read_xlsx_sheets("input/cleaned_datasets/MCBP_PDM_Tool_2024-12-24.xlsx")
# PDM Round 2
pdm_dt_R2_zaranj = read_xlsx_sheets("input/cleaned_datasets/MCBP_PDM_Zaranj_R2_2025-01-20.xlsx")

# Read other documents -----------------------------------------------------------------------------
column_mapping <- read_excel("input/cleaned_datasets/Column Mapping.xlsx", sheet="Columns")
# Merge column names in mapped file
column_mapping <- column_mapping %>% 
  mutate(All_cols=case_when(
    PDM_R1_Columns==PDM_R2_Columns ~ PDM_R1_Columns,
    is.na(PDM_R1_Columns) ~ PDM_R2_Columns,
    is.na(PDM_R2_Columns) ~ PDM_R1_Columns
  ),
  All_sheets=case_when(
    Sheet_R1==Sheet_R2 ~ Sheet_R1,
    is.na(Sheet_R1) ~ Sheet_R2,
    is.na(Sheet_R2) ~ Sheet_R1
  ))
column_mapping %>% count(All_sheets) # Check

# Recode Datasets ----------------------------------------------------------------------------------
pdm_dt_R1$data <- pdm_dt_R1$data %>% 
  mutate(
    # New response was added
    who_did_you_go_with_18=case_when(
      !is.na(who_did_you_go_with_17) ~ 0
    ),
    # Change Response label based on round 2 changes
    were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement=case_when(
      were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement %in% 
        "No, I was not aware but when I got to the distribution site, I got to know about SBBC sessions" ~ "No, I was not aware beforehand, but when I/my registered alternate arrived at the distribution site, we got to know about the SBCC sessions.",
      TRUE ~ were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement
    ),
    #*** Should be checked with QA and Nabizada
    did_you_attend_the_information_session=case_when(
      did_you_attend_the_information_session %in% "No" ~ "No, I did not participate.",
      did_you_attend_the_information_session %in% "Yes" ~ "Yes, I participated.",
      TRUE ~ did_you_attend_the_information_session
    )
  )

# Merge Datasets -----------------------------------------------------------------------------------
# Initiate merged list
pdm_dt_merged <- list()

# Merge 
pdm_dt_merged$data <- bind_rows(
  pdm_dt_R1$data,
  pdm_dt_R2_zaranj$data
)
pdm_dt_merged$children_under2 <- bind_rows(
  pdm_dt_R1$children_under2,
  pdm_dt_R2_zaranj$children_under2
)
nrow(pdm_dt_merged$data)==nrow(pdm_dt_R1$data)+nrow(pdm_dt_R2_zaranj$data) # True
nrow(pdm_dt_merged$children_under2)==nrow(pdm_dt_R1$children_under2)+nrow(pdm_dt_R2_zaranj$children_under2) # True

#### Rearrange Dataset Columns
# Subset
data_cols <- column_mapping$All_cols[column_mapping$All_sheets=="data"]
child_cols <- column_mapping$All_cols[column_mapping$All_sheets=="children_under2"]

# Test: mapping columns match with dataset
data_cols[data_cols %notin% names(pdm_dt_merged$data)]; names(pdm_dt_merged$data)[names(pdm_dt_merged$data) %notin% data_cols]
child_cols[child_cols %notin% names(pdm_dt_merged$children_under2)]; names(pdm_dt_merged$children_under2)[names(pdm_dt_merged$children_under2) %notin% child_cols]

# Sort
pdm_dt_merged$data <- pdm_dt_merged$data %>% select(all_of(data_cols))
pdm_dt_merged$children_under2 <- pdm_dt_merged$children_under2 %>% select(all_of(child_cols))


# Tests  -------------------------------------------------------------------------------------------
pdm_dt_R1$data %>% count(were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement)
pdm_dt_R2_zaranj$data %>% count(were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement)

pdm_dt_R1$data %>% count(did_you_attend_the_information_session)
pdm_dt_R2_zaranj$data %>% count(did_you_attend_the_information_session)

pdm_dt_merged$data %>% count(were_you_aware_of_an_information_session_that_was_held_on_the_day_of_the_cash_disbursement)
pdm_dt_merged$data %>% count(did_you_attend_the_information_session)


# Export -------------------------------------------------------------------------------------------
check_path("output/merged_data") # create the output path
export_datasets(pdm_dt_merged, paste0("output/merged_data/MCBP_PDM_Tool_Merged_", lubridate::today(), ".xlsx"))

