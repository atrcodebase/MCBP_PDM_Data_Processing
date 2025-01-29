### Filter Approved data for client
pdm_dt_approved <- pdm_dt

## PDM
pdm_dt_approved$data <- pdm_dt_approved$data %>% filter(qa_status %in% approved_qa_status & phone_response_short %in% survey_status)

pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>% 
  filter(PARENT_KEY %in% pdm_dt_approved$data$KEY)

## Remove extra objects ----------------------------------------------------------------------------
rm(approved_qa_status)

