### Remove Rejected QA status and keys -------------------------------------------------------------
rejected_qa_status <- "Rejected"
scto_rejected <- "REJECTED"


## Direct Observation
pdm_dt$data <- pdm_dt$data %>% 
  filter(qa_status %notin% rejected_qa_status &
           KEY %notin% rejection_log$KEY_Unique & 
           review_status1 %notin% scto_rejected)
# children_under2
pdm_dt$children_under2 <- pdm_dt$children_under2 %>%
  filter(PARENT_KEY %in% pdm_dt$data$KEY) %>%
  filter(KEY %notin% rejection_log$KEY_Unique)

## Remove extra objects ----------------------------------------------------------------------------
rm(rejected_qa_status, scto_rejected)

