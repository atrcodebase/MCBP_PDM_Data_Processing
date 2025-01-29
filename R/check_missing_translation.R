# Log missing audio translation and missing image QA -----------------------------------------------
## PDM
pdm_tool <- read_excel(pdm_tool_path, "survey", guess_max = 100000)
pdm_audio_cols <- pdm_tool %>% filter(type %in% c("audio")) %>% pull(name) #  & name %in% names(impact_evaluation_approved$data)
pdm_image_cols <- pdm_tool %>% filter(type %in% c("image")) %>% pull(name)

# Round 2 (QA): audios won't be translated
pdm_missing_log <- rbind(
  # # Translation
  log_questions(data=pdm_dt_approved$data,
                columns=pdm_audio_cols[pdm_audio_cols %in% names(pdm_dt_approved$data)],
                suffix="translation", sheet="data"),
  log_questions(data=pdm_dt_approved$children_under2,
                columns=pdm_audio_cols[pdm_audio_cols %in% names(pdm_dt_approved$children_under2)],
                suffix="translation", sheet="children_under2")
  # Image QA: No image
  )
# pdm_missing_log <- pdm_missing_log %>% 
  # Round 1 (QA): these questions are only for QA purposes and won't be translated
  # filter(question %notin% c(
  #   "can_you_please_tell_me_why_good_nutrition_is_important_translation",
  #   "can_you_please_tell_me_what_malnutrition_is_translation",
  #   "can_you_please_tell_me_how_to_prevent_malnutrition_translation"
  # )) %>% 
  # Round 2 (QA): No audio translations, choices columns are added
  # filter(question %notin% c(
  #   "can_you_please_tell_me_some_factors_that_lead_to_poor_nutrition_during_pregnancy_translation",
  #   "can_you_please_tell_me_what_kinds_of_foods_women_can_eat_at_home_to_prevent_poor_nutrition_during_pregnancy_translation",
  #   "can_you_please_tell_me_why_good_nutrition_is_important_during_pregnancy_translation"
  # ))

## Log Missing Translation -------------------------------------------------------------------------
excluded_cols <- c()

missing_translation_log <- rbind(
  missing_translation(data = pdm_dt_approved$data, KEY = "KEY", excluded_cols),
  missing_translation(data = pdm_dt_approved$children_under2, KEY = "KEY", excluded_cols)
  ) %>% mutate(Tool = "PDM")


# missing_translation_log <- missing_translation_log %>% 
#   # mutate(key = str_split_fixed(KEY, "/", 2)[,1]) %>%
#   left_join(
#     pdm_dt$data %>% select(Province, Round, phone_response_short, uuid=KEY)
#   ) %>% 
#   filter(phone_response_short%in%"Complete" & question %notin% 
#            c(
#              "why_were_you_unable_to_collect_the_cash_audio",
#              "quality_of_the_growth_monitoring_checkup_elaborate_choices"
#            )) 


## Export List -------------------------------------------------------------------------------------
missing_translation_QA_log <- rbind(
  pdm_missing_log %>% mutate(Tool = "PDM")
  )


## Separate translation and image logs
missing_translation_QA_log_sub <- missing_translation_QA_log %>% 
  filter(question_type == "translation")

# Export list
missing_translation_QA_log <- list(
  Image_log=filter(missing_translation_QA_log, question_type=="qa_status"),
  Audio_log=missing_translation_QA_log_sub
)

# remove extra objects -----------------------------------------------------------------------------
rm(pdm_image_cols, pdm_audio_cols, missing_translation_QA_log_sub)

