### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx", sheet="extra_columns")
extra_cols %>% count(Tool, Sheet)


unnecessary_cols <- pdm_dt_approved$data %>% 
  # select(starts_with("passcode_correct[1]"), starts_with("SET-OF-"), ends_with("_Caption")) %>% 
  select(ends_with("_Translation")) %>% # Remove translation columns if audios are not translated
  names()

# Test: check & make sure no necessary column is removed
# pdm_dt_approved$data %>%  select(any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "data"]))) %>% View
# pdm_dt_approved$children_under2 %>%  select(any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "children_under2"]))) %>% View

# Don't exclude round 1 columns in round dataset
if(unique(pdm_dt_approved$data$Round)==1){
  extra_cols <- extra_cols %>% filter(Round %notin% 1)
}
## Remove Extra columns ----------------------------------------------------------------------------
## PDM
pdm_dt_approved$data <- pdm_dt_approved$data %>%
  select(-any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "data"], unnecessary_cols)))
pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>%
  select(-any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "children_under2"], unnecessary_cols)))

# remove extra objects -----------------------------------------------------------------------------
rm(extra_cols)


