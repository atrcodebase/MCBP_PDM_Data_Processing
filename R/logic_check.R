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

  pdm_dt_approved$data %>% 
    mutate(number_of_children_under2_cal=case_when(
      do_you_currently_have_a_child_under_2 == "Yes" ~ how_many_children_under_2_do_you_have,
      TRUE ~ how_many_children_did_you_have_at_the_time_of_registration
    )) %>% 
    filter(number_of_children_under2!=number_of_children_under2_cal) %>%
    mutate(issue="The calculate question need to be updated",
           Questions = "number_of_children_under2 - how_many_children_under_2_do_you_have - how_many_children_did_you_have_at_the_time_of_registration",
           Values = paste0(number_of_children_under2, " - ", how_many_children_under_2_do_you_have, " - ", how_many_children_did_you_have_at_the_time_of_registration)) %>%
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
    select(Questions, Values, issue, KEY, qa_status)
  
)

# Food Consumption Score
fcs_cols <- pdm_dt_approved$data %>% 
  select(cereals_and_tubers_bread_rice_pasta_maize_potatoes_sweet_potatoes:sugar_or_sweet_sugar_honey_jam_cakes_candy_cookies_and_other_sweet) %>% names()

fcs_average <- pdm_dt_approved$data %>% 
  mutate(Surveyor_Id=toupper(Surveyor_Id)) %>% 
  mutate(across(all_of(fcs_cols), as.numeric)) %>% 
  group_by(Surveyor_Id) %>% 
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

# Cross-check with sample  -------------------------------------------------------------------------
pdm_sample <- read_excel("input/Zaranj &Warduj_Sample_All_merged.xlsx", guess_max = 5000000)
# Recode phone numbers in sample and create full_name
pdm_sample <- pdm_sample %>% 
  mutate(resp_pn=str_replace(`Mobile number`, "\\+93", "0"),
         full_name=case_when(
           !is.na(`Principal Father Name`) ~ paste("name: ", tolower(`Principal First Name`), " | father name: ", tolower(`Principal Father Name`)),
           TRUE ~ paste("name: ", tolower(`Principal First Name`), " | father name: ", tolower(`Principal Last Name`)),
         ),
         full_name=str_squish(full_name)) 
# pdm_sample %>% count(Round, District, Sample_Type)

# Join sample with data
pdm_data <- pdm_dt_approved$data %>% 
  mutate(resp_pn=case_when(
    str_length(resp_pn)==9 ~ paste0("0", resp_pn),
    resp_pn=="93792787577" ~ "0792787577",
    resp_pn=="0799 22 81 71" ~ "0799228171",
    resp_pn=="0745191 946" ~ "0745191946",
    resp_pn=="0700 643506" ~ "0700643506",
    TRUE ~ str_squish(str_remove_all(resp_pn, " "))
  ), full_name=tolower(str_squish(full_name))) %>% 
  left_join(
    pdm_sample %>% select(resp_pn, full_name, Sample_Type), by=c("resp_pn", "full_name")
  )
# Check respondents not in sample
new_respondents <- pdm_data %>% 
  filter(resp_pn %notin% pdm_sample$resp_pn) %>% 
  select(KEY, full_name, resp_pn, phone_response_short) %>% 
  mutate(Remark="Respondent Phone number not found in sample!") 

# Manual Checks ------------------------------------------------------------------------------------
pdm_dt_approved$data %>% janitor::get_dupes(Site_Visit_ID)
pdm_dt_approved$data %>% janitor::get_dupes(full_name, resp_pn)

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
  Sample_mismatch=new_respondents
)

# Remove extra objects -----------------------------------------------------------------------------
rm(pdm_data, fcs_cols)


