### Extra columns
extra_cols <- read_excel("input/extra_columns.xlsx", sheet="extra_columns")
extra_cols %>% count(Tool, Sheet)


SBCC_round_cols <- read_excel("input/extra_columns.xlsx", sheet="Rounds") 
dist_type <- read_excel("input/extra_columns.xlsx", sheet="dist_type") 

unnecessary_cols <- pdm_dt_approved$data %>%
  select(starts_with("passcode_correct[1]"), starts_with("SET-OF-"), ends_with("_Caption")) %>% names()

# Questions changed to calculate (removed)
unnecessary_cols <- c(
  unnecessary_cols,
  c("did_alternate_attend_the_sbcc_sessions_and_receive_the_second_round_cash_payment_disbursement_on_date_of_the_second_round_disbursement_from_the_mcbp_project",
    # PII
    "Household_ID",	"Household_Name"
    
  )
)

# Test: check & make sure no necessary column is removed
# pdm_dt_approved$data %>%  select(any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "data"]))) %>% View
# pdm_dt_approved$children_under2 %>%  select(any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "children_under2"]))) %>% View

## Remove Extra columns ----------------------------------------------------------------------------
## PDM
pdm_dt_approved$data <- pdm_dt_approved$data %>%
  select(-any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "data"], unnecessary_cols)))
pdm_dt_approved$children_under2 <- pdm_dt_approved$children_under2 %>%
  select(-any_of(c(extra_cols$questions[extra_cols$Tool %in% "PDM" & extra_cols$Sheet %in% "children_under2"], unnecessary_cols)))



# Remove round 1 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 1)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 1) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}


# Remove round 2 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 2)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 2) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}

# Remove round 3 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 3)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 3) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}

# Remove round 4 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 4)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 4) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}

# Remove round 5 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 5)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 5) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}

# Remove round 6 columns
if(all(unique(pdm_dt_approved$data$Round) %in% 6)){
  SBCC_round_cols_sub <- SBCC_round_cols %>% filter(Round %notin% 6) %>% pull(questions)
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(SBCC_round_cols_sub))
}

# Remove in-person columns from Phone survey
if(all(unique(pdm_dt_approved$data$interview_type) %in% "Phone interview")){
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-any_of(c("may_I_take_a_photo_of_your_mcbp_scope_card",	"scope_card_photo",	"scope_card_photo_qa_status",
                     "may_i_take_a_photo_of_your_tazkira_or_any_available_id",	"photo_of_the_id_or_tazkira",
                     "photo_of_the_id_or_tazkira_qa_status", "sample_type",	"cluster",	"interview_type_firstlast",	"beneficiary_located", 
                     "in_which_village_do_you_live",
                     "consent_community", "consent_family_member", "external_resp_gender", 
                     "whose_contact", "whose_contact_other", "external_resp_type", 
                     "do_you_know_the_bf", "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household", 
                     "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_audio", 
                     "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_translation", 
                     "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_photo", 
                     "can_you_please_confirm_through_audio_or_by_writing_state_your_name_your_role_in_the_community_and_your_confirmation_you_do_not_know_beneficiary_name_daughter_of_beneficiary_father_name_or_her_household_photo_qa_status", 
                     "does_he_she_live_in_this_community", "can_you_please_explain_why_there_was_not_anyone_from_this_household_to_be_interviewed", 
                     "can_you_please_explain_why_there_was_not_anyone_from_this_household_to_be_interviewed_other", 
                     "why_is_he_she_not_living_in_the_vicinity_of_this_community", 
                     "when_did_she_her_household_leave_the_community", "duration_left_community", 
                     "take_a_photo_of_your_tazkira_or_any_available_id", "photo_of_the_external_id_or_tazkira", 
                     "photo_of_the_external_id_or_tazkira_qa_status", "may_I_take_a_photo_of_bFs_MCBP_SCOPE_Card", 
                     "bFs_scope_card_photo", "bFs_scope_card_photo_qa_status", "external_relationship_with_bf", 
                     "external_relationship_with_bf_other", "do_you_live_in_the_same_household_andhousehold_khaana_and_share_the_same_food_pot_destarkhaan_with_beneficiary", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_why_an_interviewmeeting_with_the_beneficiary_beneficiary_name_is_not_possible", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_why_an_interviewmeeting_with_the_beneficiary_beneficiary_name_is_not_possible_audio", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_why_an_interviewmeeting_with_the_beneficiary_beneficiary_name_is_not_possible_translation", 
                     "take_a_photo_of_the_written_confirmation", "take_a_photo_of_the_written_confirmation_photo_qa_status", 
                     "did_bf_attend_the_sbcc_sessions_and_receive_the_second_round_cash_payment_disbursement_on_date_of_the_second_round_disbursement_from_the_mcbp_project", 
                     # "did_alternate_attend_the_sbcc_sessions_and_receive_the_second_round_cash_payment_disbursement_on_date_of_the_second_round_disbursement_from_the_mcbp_project", 
                     "how_much_money_did_the_beneficiary_registered_alternate_receive_as_the_cash_payment_disbursement_from_the_mcbp_project", 
                     "how_much_money_did_the_beneficiary_registered_alternate_receive_as_the_cash_payment_disbursement_from_the_mcbp_project_n", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_from_the_mcbp_project", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_audio", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_translation", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_photo", 
                     "could_you_please_explain_and_confirm_either_with_signatures_or_an_audio_recording_that_beneficiary_and_registered_alternate_did_not_receive_the_cash_payment_disbursement_on_date_of_disbursement_from_the_mcbp_project_photo_qa_status", 
                     "doortag_number", "doortag_photo", "doortag_photo_QA", "doortag_photo2", 
                     "doortag_photo2_QA")))
}

# Remove Hesabpay questions from cash based distributions
if(all(unique(pdm_dt_approved$data$cash_or_mobile_money) %in% "Cash") | all(unique(pdm_dt_approved$data$District) %in% "Kamdesh")){
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-any_of(dist_type$questions[dist_type$Type %in% "MM"]))
}

# Remove Cashbased questions from Hesabpay distributions
if(all(unique(pdm_dt_approved$data$cash_or_mobile_money) %in% "Hesabpay")){
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-any_of(dist_type$questions[dist_type$Type %in% "Cash"]))
}

# Remove Ghazni & Paktika specific questions form other data 
if(unique(pdm_dt_approved$data$Province) %notin% c("Ghazni", "Paktika")){
  # Alert if it has data (just in case the relevancy is changed)
  if(!is.na(unique(pdm_dt_approved$data$how_many_times_have_you_received_cash_assistance_so_far))){
    print("Check Ghazni & Paktika specific questions!")
  }
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-all_of(c("how_many_times_have_you_received_cash_assistance_so_far", 
                     "how_many_times_have_you_received_cash_assistance_so_far_other", 
                     "how_much_was_the_cash_assistance_in_each_round", "how_much_was_the_cash_assistance_in_each_round_other", 
                     "who_received_the_cash_assistance", "who_received_the_cash_assistance_other", 
                     "how_many_times_have_you_attended_sbcc_sessions_at_the_distribution_sites_so_far", 
                     "how_many_times_have_you_attended_sbcc_sessions_at_the_distribution_sites_so_far_other", 
                     "who_attended_the_sbcc_sessions_in_the_distribution_sites", "who_attended_the_sbcc_sessions_in_the_distribution_sites_other")))
}

# Remove Round 1 specific column
if(unique(pdm_dt_approved$data$Round) %notin% 1){
  # Alert if it has data (just in case the relevancy is changed)
  # if(!is.na(unique(pdm_dt_approved$data$did_you_receive_a_hesabpay_card_on_date_of_distribution))){
  #   print("Check Round 1 specific questions!")
  # }
  
  pdm_dt_approved$data <- pdm_dt_approved$data %>% 
    select(-any_of(c("did_you_receive_a_hesabpay_card_on_date_of_distribution",
                     "why_didnt_you_receive_the_card_choices",	
                     "why_didnt_you_receive_the_card",	
                     "why_didnt_you_receive_the_card_translation",	
                     "were_you_given_any_instruction_on_how_to_open_a_digital_wallet_by_fsp_staff_or_cp_staff",
                     "do_you_feel_you_understand_how_to_use_the_digital_wallet",
                     "why_didnt_understand_how_to_use_account",
                     "why_didnt_understand_how_to_use_account_translate",
                     "were_you_given_any_instruction_on_how_to_use_the_card_ie_make_cashless_transactions_or_cash_out",
                     "do_you_feel_you_understand_how_to_use_the_card",
                     "why_didnt_understand_how_to_use_card",
                     "why_didnt_understand_how_to_use_card_translate"
)))
}

# remove extra objects -----------------------------------------------------------------------------
rm(extra_cols)


