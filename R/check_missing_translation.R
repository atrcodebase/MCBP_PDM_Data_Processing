# Log missing audio translation and missing image QA -----------------------------------------------
## PDM
pdm_tool <- read_excel(pdm_tool_path, "survey", guess_max = 100000)
pdm_audio_cols <- pdm_tool %>% filter(type %in% c("audio")) %>% pull(name) #  & name %in% names(impact_evaluation_approved$data)
pdm_image_cols <- pdm_tool %>% filter(type %in% c("image")) %>% pull(name)

audio_cols_diff <- list(
  "can_you_please_tell_me_why_breastfeeding_is_important_superior_to_other_feeding_practices_in_the_first_six_months_audio"=
    "can_you_please_tell_me_why_breastfeeding_is_important_superior_to_other_feeding_practices_in_the_first_six_months_translation",
  "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_audio"=
    "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_translation",
  "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_why_an_interviewmeeting_with_the_beneficiary_beneficiary_name_is_not_possible_audio"=
    "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_why_an_interviewmeeting_with_the_beneficiary_beneficiary_name_is_not_possible_translation",
  "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_audio"=
    "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_translation",
  "can_you_please_tell_me_what_will_happen_if_a_child_is_not_breastfed_properly_audio"=
    "can_you_please_tell_me_what_will_happen_if_a_child_is_not_breastfed_properly_translation",
  "why_didnt_understand_how_to_use_account"="why_didnt_understand_how_to_use_account_translate",
  "why_didnt_understand_how_to_use_card"="why_didnt_understand_how_to_use_card_translate",
  "can_you_please_tell_me_why_handwashing_is_important_audio"="can_you_please_tell_me_why_handwashing_is_important_translation",
  "can_you_please_tell_me_why_immunization_is_important_audio"="can_you_please_tell_me_why_immunization_is_important_translation",
  "can_you_please_tell_me_one_wash_practice_audio"="can_you_please_tell_me_one_wash_practice_translation",
  "can_you_please_tell_me_what_is_complementary_feeding_audio"=
    "can_you_please_tell_me_what_is_complementary_feeding_translation",
  "can_you_please_tell_me_why_complementary_feeding_is_important_audio"=
    "can_you_please_tell_me_why_complementary_feeding_is_important_translation",
  "can_you_please_tell_me_what_will_happen_if_the_child_is_not_fed_properly_audio"=
    "can_you_please_tell_me_what_will_happen_if_the_child_is_not_fed_properly_translation"
)
image_cols_diff <- list(
  "doortag_photo"="doortag_photo_QA",
  "doortag_photo2"="doortag_photo2_QA",
  "take_a_photo_of_the_written_confirmation"="take_a_photo_of_the_written_confirmation_photo_qa_status"
)

# Round 2 (QA): audios won't be translated
pdm_missing_log <- rbind(
  # # Translation
  log_questions(data=pdm_dt_approved$data,
                columns=pdm_audio_cols[pdm_audio_cols %in% names(pdm_dt_approved$data)],
                suffix="translation", sheet="data", columns_different = audio_cols_diff),
  log_questions(data=pdm_dt_approved$children_under2,
                columns=pdm_audio_cols[pdm_audio_cols %in% names(pdm_dt_approved$children_under2)],
                suffix="translation", sheet="children_under2", columns_different = audio_cols_diff),
  # Image QA
  log_questions(data=pdm_dt_approved$data, 
                columns=pdm_image_cols[pdm_image_cols %in% names(pdm_dt_approved$data)], 
                suffix="qa_status", sheet="data", columns_different=image_cols_diff),
  log_questions(data=pdm_dt_approved$children_under2, 
                columns=pdm_image_cols[pdm_image_cols %in% names(pdm_dt_approved$children_under2)], 
                suffix="qa_status", sheet="data", columns_different=image_cols_diff)
  
  )

## Exclude SBCC related audios (not translated unless requested)
pdm_missing_log <- pdm_missing_log %>% 
  filter(question %notin% SBCC_round_cols$questions)

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
excluded_cols <- c("Round_Date", "cluster", "in_which_village_do_you_live")

missing_translation_log <- rbind(
  missing_translation(data = pdm_dt_approved$data, KEY = "KEY", excluded_cols),
  missing_translation(data = pdm_dt_approved$children_under2, KEY = "KEY", excluded_cols)
  ) %>% mutate(Tool = "PDM")


if(nrow(missing_translation_log)!=0){
  missing_translation_log <- missing_translation_log %>%
    # mutate(key = str_split_fixed(KEY, "/", 2)[,1]) %>%
    left_join(pdm_dt$data %>% select(Province, District, Round, uuid=KEY)) 
  
  #   filter(phone_response_short%in%"Complete" & question %notin% 
  #            c(
  #              "why_were_you_unable_to_collect_the_cash_audio",
  #              "quality_of_the_growth_monitoring_checkup_elaborate_choices"
  #            )) 
}


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

