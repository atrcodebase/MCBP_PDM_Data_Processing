# apply the value labels
## PDM ---------------------------------------------------------------------------------------------
pdm_dt$data <- labeler(data = pdm_dt$data,
                        tool = pdm_tool_path,
                        survey_label = "label",
                        choice_lable = "label",
                        multi_response_sep = ";")
# children_under2
pdm_dt$children_under2 <- labeler(data = pdm_dt$children_under2,
                       tool = pdm_tool_path,
                       survey_label = "label",
                       choice_lable = "label",
                       multi_response_sep = ";")

# remove extra objects -------------------------------------------
rm()

