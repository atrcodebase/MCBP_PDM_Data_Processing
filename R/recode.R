# Change/Recode variables
relabel_98_99 <- function(x) {
  x = case_when(
    x %in% c(9999, "9999") ~ "Refuse to respond",
    x %in% c(8888, "8888") ~ "I don't know",
    TRUE ~ as.character(x)
  )}

# Numeric cols
PDM_numeric_cols <- read_excel(pdm_tool_path, "survey") %>% filter(type%in%c("integer", "decimal")) %>% pull(name)

## PDM ---------------------------------------------------------------------------------------------
pdm_dt$data <- pdm_dt$data %>%
  mutate(Starttime = as.POSIXct(Starttime, format="%a %b %d %Y %H:%M:%S"),
  Endtime = as.POSIXct(Endtime, format="%a %b %d %Y %H:%M:%S"),
  went_to_site = case_when(
    went_to_site %in% 1 ~ "Yes",
    went_to_site %in% 0 ~ "No",
    TRUE ~ as.character(went_to_site)
  ),
  received_aid = case_when(
    received_aid %in% 1 ~ "Yes",
    received_aid %in% 0 ~ "No",
    TRUE ~ as.character(received_aid)
  ),
  Surveyor_Gender_Paused=case_when(
    Surveyor_Gender_Paused %in% "Test" ~ "Female",
    TRUE ~ Surveyor_Gender_Paused
  ),
  Surveyor_Name = case_when(
    Surveyor_Name %in% "Test Name 1" ~ "Mahleqa Hussaini",
    TRUE ~ Surveyor_Name
  ),
  Surveyor_Id = case_when(
    Surveyor_Id %in% "FR2222" ~ "FR1857",
    TRUE ~ toupper(Surveyor_Id)
  )) %>% 
  mutate(across(all_of(PDM_numeric_cols), relabel_98_99))

# Main sheet subset
pdm_sub <- pdm_dt$data %>% 
  select(Site_Visit_ID, Province, District, Village, Distribution_Site,	Round, KEY)

pdm_dt$children_under2 <- pdm_dt$children_under2 %>% 
  left_join(pdm_sub, by=c("PARENT_KEY"="KEY")) %>% 
  relocate(Site_Visit_ID:Round, .before = 1)


# Update links
pdm_dt$data <- update_media_links(data=pdm_dt$data, 
                                  tool_path = pdm_tool_path,
                                  download_link=download_link) # No need if data is downloaded from SCTO website

pdm_dt$children_under2 <- update_media_links(data=pdm_dt$children_under2, 
                                  tool_path = pdm_tool_path,
                                  download_link=download_link, 
                                  key_col="PARENT_KEY") # No need if data is downloaded from SCTO website


# remove extra objects -----------------------------------------------------------------------------
rm(relabel_98_99, PDM_numeric_cols, pdm_sub)

