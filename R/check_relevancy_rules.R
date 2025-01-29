# Check Relevancy Rules ----------------------------------------------------------------------------
## Read
PDM_tool_relevancy <- read_excel("input/tool_relevancy_rules/MCBP_PDM_relevancy_rules.xlsx")

### PDM
## Join main columns with repeat sheets
PDM_data_sub <- pdm_dt$data %>%
  select(Passcode, Surveyor_Name, answered_response, before_this_interview_were_you_aware_that_you_were_registered_as_a_beneficiary,
         respondent_gender, how_many_children_under_2_do_you_have, how_many_children_did_you_have_at_the_time_of_registration, KEY)

PDM_relevancy_issues <- rbind(
  check_relevancy_rules(pdm_dt$data, PDM_tool_relevancy, sheet_name="data"),
  check_relevancy_rules(pdm_dt$children_under2 %>% 
                          left_join(PDM_data_sub, by=c("PARENT_KEY"="KEY")), 
                        PDM_tool_relevancy, sheet_name="children_under2")
)

if(nrow(PDM_relevancy_issues)!=0){
  PDM_relevancy_issues <- PDM_relevancy_issues %>% 
    mutate(key = str_split_fixed(KEY, "/", 2)[,1]) %>%
    left_join(
      pdm_dt$data %>% select(Province, Round, phone_response_short, key=KEY)
    ) # %>% filter(Round %notin% "1")
  # filter(phone_response_short%in%"Complete" & question %notin% 
  #          c(
  #            "why_were_you_unable_to_collect_the_cash_audio",
  #            "quality_of_the_growth_monitoring_checkup_elaborate_choices",
  #            "quality_of_the_session_elaborate_choices"
  #          )) 
}

# Update Select_multiple series columns ------------------------------------------------------------
## PDM
PDM_SM_issues <- c()
for(sheet in c("data", "children_under2")){
  pdm_dt[[sheet]] <- pdm_dt[[sheet]] %>%
    update_series_cols(tool_path = pdm_tool_path,
                       question_separator="_")
  # Check if updated correctly
  PDM_SM_issues <- rbind(
    PDM_SM_issues,
    check_select_multiple(data=pdm_dt[[sheet]],
                          tool_path = pdm_tool_path,
                          question_separator="_")
  )
}

## Export List -------------------------------------------------------------------------------------
# Relevancy
relevancy_issues <- plyr::rbind.fill(
  PDM_relevancy_issues %>% mutate(Tool="Post_Distribution_Monitoring")
) 

## Select Multiple issues
SM_issues <- list(
  PDM_SM_issues=PDM_SM_issues
)

# remove extra objects -----------------------------------------------------------------------------
rm(PDM_data_sub)

