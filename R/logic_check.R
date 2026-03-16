### Logic Checks
# PDM
pdm_logical_issues <- rbind(
  # Inconsistencies
  pdm_dt_approved$data %>% 
    filter((respondent_gender %in% "Male" & 
              (who_is_the_main_decision_maker_in_the_household_with_regard_to_finances_household_budget_and_spending %in% "My husband" | 
                 what_is_your_relationship_with_your_registered_alternate %in% "My husband")) |
             (respondent_gender %in% "Female" & 
             (who_is_the_main_decision_maker_in_the_household_with_regard_to_finances_household_budget_and_spending %in% "My wife"  | 
                what_is_your_relationship_with_your_registered_alternate %in% "My wife"))) %>%
    mutate(issue="The gender and the relationship is inconsistent!",
           Questions = "respondent_gender - who_is_the_main_decision_maker_in_the_household_with_regard_to_finances_household_budget_and_spending",
           Values = paste0(respondent_gender, " - ", who_is_the_main_decision_maker_in_the_household_with_regard_to_finances_household_budget_and_spending)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  
  pdm_dt_approved$data %>% 
    filter(as.numeric(how_many_children_under_2_do_you_have) > as.numeric(how_many_people_permanently_live_in_your_household_including_yourself_n)) %>%
    mutate(issue="The gender and the relationship is inconsistent!",
           Questions = "how_many_children_under_2_do_you_have - how_many_people_permanently_live_in_your_household_including_yourself_n",
           Values = paste0(how_many_children_under_2_do_you_have, " - ", how_many_people_permanently_live_in_your_household_including_yourself_n)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  
  pdm_dt_approved$children_under2 %>% 
    left_join(
      pdm_dt_approved$data %>% 
        mutate(under_two=case_when(
          do_you_currently_have_a_child_under_2 == "Yes" ~ "Yes",
          did_you_have_a_child_under_2_at_mcbp_registration == "Yes" ~ "No"
        )) %>% 
        select(under_two, do_you_currently_have_a_child_under_2, did_you_have_a_child_under_2_at_mcbp_registration, qa_status, KEY),
      by=c("PARENT_KEY"="KEY")
    ) %>% 
    filter(lubridate::year(please_specify_the_date) < 2023 & under_two=="Yes") %>% 
    mutate(issue="The respondent says she has children under two but the growth monitoring checkup was more than two years ago!",
           Questions = "please_specify_the_date - do_you_currently_have_a_child_under_2",
           Values = paste0(please_specify_the_date, " - ", do_you_currently_have_a_child_under_2)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  
  pdm_dt_approved$data %>% 
    filter(as.numeric(how_many_people_permanently_live_in_your_household_including_yourself_n) <= as.numeric(number_of_children_under2)) %>%
    mutate(issue="The number of people living in the house should logically be bigger than the number of children!",
           Questions = "how_many_people_permanently_live_in_your_household_including_yourself_n - number_of_children_under2",
           Values = paste0(how_many_people_permanently_live_in_your_household_including_yourself_n, " - ", number_of_children_under2)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  
  
  
  
  # Calculate Checks
  pdm_dt_approved$data %>% 
    mutate(phone_response_calculated = case_when(
      Can_We_Callback %in% c("Yes, the same phone number", "Yes, different phone number") | 
        answered_response %in% "Will complete the survey, but not now" ~ "Rescheduled",
      Can_We_Callback %in% "No" | answered_response %in% "Number rang but the call was dropped" ~ "Refused",
      answered_response %in% "Yes" ~ "Complete",
      answered_response %in% "No" ~ "Refused",
      Reason_Not_reached_out_to_respondent %in% c("He/she doesn’t live here anymore",
                                                  "He/she only lived here to receive the assistance and has since returned to his/her original home",
                                                  "He/she never lived here",
                                                  "I don’t know this person") ~ "Irrelevant respondent",
      phone_response %in% "Number not working at the moment/Out of the coverage area" ~ "Number not working or not active",
      phone_response %in% "Number rang but was not answered/Number busy" ~ "Number rang but was not answered/Number busy",
      phone_response %in% "Number rang but the call was dropped" ~ "Number rang but the call was dropped",
      TRUE ~ "NEW_CONDITION"
    )) %>% 
    filter(phone_response_calculated!=phone_response_short) %>%
    mutate(issue="Calculate questions needs to be updated!",
           Questions = "phone_response_calculated - phone_response_short",
           Values = paste0(phone_response_calculated, " - ", phone_response_short)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  pdm_dt_approved$data %>% 
    mutate(went_to_site_calculated = case_when(
      did_you_go_to_the_distribution_site_on_the_day_of_disbursements_to_collect_the_cash_benefit %in% "Yes" | 
        did_your_registered_alternate_go_to_the_distribution_site_to_collect_the_cash_handout_on_your_behalf %in% 
        c("Yes", "Someone else went (i.e. not my alternate and not me)") ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    filter(went_to_site_calculated!=went_to_site) %>%
    mutate(issue="Calculate questions needs to be updated!",
           Questions = "went_to_site_calculated - went_to_site",
           Values = paste0(went_to_site_calculated, " - ", went_to_site)) %>%
    select(Questions, Values, issue, KEY, qa_status),
    
  pdm_dt_approved$data %>% 
    mutate(received_aid_calculated = case_when(
      did_you_receive_the_cash_payment_disbursement_from_the_mcbp_project %in% c("Yes", "I don’t know", "Refused to respond") | 
        # did_you_receive_a_mobile_money_transfer_to_the_hesabpay_card_on_site %in% c("Yes", "I don’t know", "Refused to respond") ~ "Yes", # Old labels
        did_you_receive_a_mobile_money_transfer_to_the_hesabpay_card_on_site %in% c("Yes, I received a transfer",
                                                                                    "Yes, my registered alternate received a transfer",
                                                                                    "I don’t know", "Refused to respond") ~ "Yes",
      TRUE ~ "No"
    )) %>% 
    filter(received_aid_calculated!=received_aid) %>%
    mutate(issue="Calculate questions needs to be updated!",
           Questions = "received_aid_calculated - received_aid",
           Values = paste0(received_aid_calculated, " - ", received_aid)) %>%
    select(Questions, Values, issue, KEY, qa_status),
  pdm_dt_approved$data %>% 
    mutate(num_childrenU2_calculated = case_when(
      do_you_currently_have_a_child_under_2 %in% c("Yes") ~ how_many_children_under_2_do_you_have,
      TRUE ~ how_many_children_did_you_have_at_the_time_of_registration
    )) %>% 
    filter(num_childrenU2_calculated!=number_of_children_under2) %>%
    mutate(issue="Calculate questions needs to be updated!",
           Questions = "num_childrenU2_calculated - number_of_children_under2",
           Values = paste0(num_childrenU2_calculated, " - ", number_of_children_under2)) %>%
    select(Questions, Values, issue, KEY, qa_status)
  
)

# Food Consumption Score
fcs_cols <- pdm_dt_approved$data %>% 
  select(cereals_and_tubers_bread_rice_pasta_maize_potatoes_sweet_potatoes:sugar_or_sweet_sugar_honey_jam_cakes_candy_cookies_and_other_sweet) %>% names()

fcs_average <- pdm_dt_approved$data %>% 
  mutate(Surveyor_Id=toupper(Surveyor_Id)) %>% 
  mutate(across(all_of(fcs_cols), as.numeric)) %>% 
  group_by(Surveyor_Id, District) %>% 
  summarize(avg_cereals=round(mean(cereals_and_tubers_bread_rice_pasta_maize_potatoes_sweet_potatoes, na.rm = T), 1),
            avg_pulses=round(mean(pulses_nuts_beans_cowpeas_peanuts_lentils_nut_soy_pigeon_pea_and___or_other_nuts, na.rm = T), 1),
            avg_dairy=round(mean(milk_and_other_dairy_products_fresh_milk__sour_yogurt_cheese_other_dairy_products, na.rm = T), 1),
            avg_meat=round(mean(meat_fish_and_eggs_goat_beef_chicken_fish_including_canned_tuna_other_seafood_eggs, na.rm = T), 1),
            avg_vegies=round(mean(vegetables_and_leaves_okra_eggplant_green_beans_spinach_leak_tomato_onion_etc, na.rm = T), 1),
            avg_fruits=round(mean(fruits_any_type_banana_apple_citrus_papaya_apricot_peach_mango_etc, na.rm = T), 1),
            avg_oil=round(mean(oil_fat_butter_vegetable_oil_ghee_margarine_other_fats_oil, na.rm = T), 1),
            avg_sugar=round(mean(sugar_or_sweet_sugar_honey_jam_cakes_candy_cookies_and_other_sweet, na.rm = T), 1),
            count_of_interviews=n()
            ) %>% ungroup()
# write.xlsx(fcs_average, "test2.xlsx")

#### Flag Numeric values in Other/Numeric Questions
pdm_logical_issues <- bind_rows(
  pdm_logical_issues,
  flag_numeric_values(pdm_dt_approved$data, pdm_tool_path, Tool="PDM"),
  flag_numeric_values(pdm_dt_approved$children_under2, pdm_tool_path, Tool="PDM_children_under2")
  
)

# ## Check Constraint Issues
# constraint_rules <- read_excel("input/tool_relevancy_rules/Outcome_Monitoring_constraint_rules.xlsx")
# 
# constraint_issues <- c()
# for(question in constraint_rules$Question){
#   xls_formula <- constraint_rules$XLS_Constraint[constraint_rules$Question %in% question]
#   const_formula <- constraint_ruleXLS_Constraintconst_formula <- constraint_rules$Corrected_rule[constraint_rules$Question %in% question]
#   
#   constraint_issues <- rbind(
#     constraint_issues,
#     outcome_mon_approved$data %>% 
#       filter(eval(parse(text=const_formula))) %>% 
#       mutate(issue="Value is inconsistent with the constraint rule!",
#              Questions = question,
#              Values = get(question),
#              Constraint_Rule=xls_formula) %>%
#       select(Questions, Values, issue, Constraint_Rule, KEY, qa_status)
#   )
# }


# Cross-check with sample  -------------------------------------------------------------------------
# pdm_sample <- read_excel("input/Zaranj &Warduj_Sample_All_merged.xlsx", guess_max = 5000000)
# # Recode phone numbers in sample and create full_name
# pdm_sample <- pdm_sample %>% 
#   filter(District %in% district & Round %in% round) %>% 
#   mutate(resp_pn=str_replace(`Mobile number`, "\\+93", "0"),
#          full_name=case_when(
#            !is.na(`Principal Father Name`) ~ paste("name: ", tolower(`Principal First Name`), " | father name: ", tolower(`Principal Father Name`)),
#            TRUE ~ paste("name: ", tolower(`Principal First Name`), " | father name: ", tolower(`Principal Last Name`)),
#          ),
#          full_name=str_squish(full_name)) 
# # pdm_sample %>% count(Round, District, Sample_Type)
# 
# # Join sample with data
# pdm_data <- pdm_dt_approved$data %>% 
#   mutate(resp_pn=case_when(
#     str_length(resp_pn)==9 ~ paste0("0", resp_pn),
#     resp_pn=="93792787577" ~ "0792787577",
#     resp_pn=="0799 22 81 71" ~ "0799228171",
#     resp_pn=="0745191 946" ~ "0745191946",
#     resp_pn=="0700 643506" ~ "0700643506",
#     TRUE ~ str_squish(str_remove_all(resp_pn, " "))
#   ), full_name=tolower(str_squish(full_name))) %>% 
#   left_join(
#     pdm_sample %>% select(resp_pn, full_name, Sample_Type), by=c("resp_pn", "full_name")
#   )
# # Check respondents not in sample
# new_respondents <- pdm_data %>% 
#   filter(resp_pn %notin% pdm_sample$resp_pn) %>% 
#   select(KEY, full_name, resp_pn, phone_response_short, HHH_full_name,	Alternate_Name) %>% 
#   mutate(Remark="Respondent Phone number not found in sample!") 
# new_respondents %>% filter(full_name %notin% pdm_sample$full_name) %>% View

# Manual Checks ------------------------------------------------------------------------------------
duplicated_sites <- pdm_dt_approved$data %>% janitor::get_dupes(Site_Visit_ID)
diplicate_resp <- pdm_dt_approved$data %>% janitor::get_dupes(full_name, resp_pn) 
if(nrow(duplicated_sites) != 0){
  print("Duplicates found!")
}

# Repeat Sheet mismatch ----------------------------------------------------------------------------
repeatsheet_mismatch <- pdm_dt_approved$children_under2 %>%
  count(KEY=PARENT_KEY, name="repeat_sheet_count", Sheet="children_under2") %>%
  full_join(pdm_dt_approved$data %>% select(main_sheet_count=number_of_children_under2, KEY) %>%
              mutate(Sheet="children_under2", Question="number_of_children_under2"), by=c("KEY", "Sheet")) %>% 
  rowwise() %>% 
  filter(repeat_sheet_count %notin% main_sheet_count & !(is.na(repeat_sheet_count) & main_sheet_count == 0)) %>% 
  mutate(Tool="PDM")


# Export list --------------------------------------------------------------------------------------
logical_issues_list <- list(
  logical_issues=pdm_logical_issues,
  repeatsheet_mismatch=repeatsheet_mismatch,
  FCS_average=fcs_average,
  duplicated_sites=duplicated_sites,
  diplicate_resp=diplicate_resp
  # Sample_mismatch=new_respondents
)

# Remove extra objects -----------------------------------------------------------------------------
rm(fcs_cols) # pdm_data


