### Filter Approved data for client
pdm_dt_approved <- pdm_dt

## PDM
pdm_dt_approved$data <- pdm_dt_approved$data %>% filter((qa_status %in% approved_qa_status & phone_response_short %in% survey_status) | 
                                                             # In-Person Interviews
                                                             beneficiary_located %in% c("Beneficiary’s household could not be found", 
                                                                                        "Beneficiary’s household is found, but there is no one eligible to be interviewed",
                                                                                        "Beneficiary’s household is found, but she is not present/available for the interview; secondary respondent from the household will be interviewed [teenager above 18 and should have information about the MCBP cash disbursement and SBCC sessions]"))

pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>% 
  filter(PARENT_KEY %in% pdm_dt_approved$data$KEY)

# pdm_dt_approved$data %>% count(beneficiary_located, qa_status) %>% View# %>% pull(beneficiary_located)
## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

