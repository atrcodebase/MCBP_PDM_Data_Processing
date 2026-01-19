# Anonymize PII fields -----------------------------------------------------------------------------
anyonymize_PII <- function(x){
  x=case_when(
    grepl(download_link, x) ~ "REDACTED",
    TRUE ~ x
  )
}

## PDM
PDM_tool <- read_excel(pdm_tool_path, "survey", guess_max = 100000)
PDM_url_cols <- PDM_tool %>% filter(type %in% c("audio", "image")) %>% pull(name) %>% c("Household_ID", "Household_Name")

pdm_dt_approved$data <- pdm_dt_approved$data %>% 
  # mutate(across(all_of(c("Household_ID", "Household_Name")), as.character)) %>%
  # mutate(across(all_of(c("Household_ID", "Household_Name")), function(x){x=case_when(!is.na(x) ~ "REDACTED", TRUE ~ x)})) %>% 
  mutate(across(any_of(PDM_url_cols), anyonymize_PII))
pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>% 
  mutate(across(any_of(PDM_url_cols), anyonymize_PII))

# Remove extra objects
rm(anyonymize_PII)
