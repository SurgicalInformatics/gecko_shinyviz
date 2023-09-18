## code to prepare `appdata` dataset goes here

# Now replaced with pins
if(FALSE){
  
  
library(tidyverse)
library(finalfit)

# testing:
# appdata = tibble(age.factor = c("<18", "19-35", "40-67", "68-100"),
#                  sex= c("Female", "Male", "Female", "Missing"),
#                  fever= c("Yes", "Yes", "No", "Unknown"),
#                  cough= c("No", "Yes", "No", "Yes"),
#                  mort= c("Died", "Died", "Alive", "Unknown")) %>%
#                    mutate(ALL = factor("ALL"))
# allvars = colnames(appdata)

# copy sw_data.rda from the surgweek_data project
load("../gecko_data/patient_data_orig.rda")


patient_data_orig %>% 
  ggplot(aes(age_years)) +
  geom_histogram()

labels_keep = extract_variable_label(patient_data_orig)

appdata = patient_data_orig %>% 
  select(-redcap_data_access_group) %>% 
  # remove REDCAP internal varnames that are between {op_code}, etc (matrix variables only):
  mutate(across(where(is_character), ~str_remove_all(., " \\s*\\{[^\\)]+\\}"))) %>% 
  # remove HTML tags:
  mutate(across(where(is_character), ~str_remove_all(., "<b>|</b>"))) %>% 
  mutate(ALL = factor("ALL"), .after = 1) %>% 
  mutate(
    # age ----
    age.groups = case_when(age_years < 18 ~ "Missing",
                           between(age_years, 18, 30) ~ "18-30",
                           between(age_years, 31, 50) ~ "31-50",
                           between(age_years, 51, 70) ~ "51-70",
                           between(age_years, 71, 120) ~ "71+",
                           TRUE ~ "Missing") %>% 
      ff_label("Age"),
    # n admissions ----
    admission_prior = parse_number(admission_prior),
    admission_prior.groups = case_when(admission_prior == 0 ~ "None",
                                       between(admission_prior, 1, 1) ~ "1",
                                       between(admission_prior, 2, 3) ~ "2-3",
                                       between(admission_prior, 5, 10) ~ "5-10",
                                       between(admission_prior, 11, 10000) ~ "10+",
                                       TRUE ~ "Missing") %>% 
      fct_relevel("None", "1", "2-3", "5-10", "10+") %>% 
      ff_label("Prior admissions"),
    # LOS ----
    post30_los.groups = case_when(post30_los == 0 ~ "0",
                                  between(post30_los, 1, 1) ~ "1",
                                  between(post30_los, 2, 3) ~ "2-3",
                                  between(post30_los, 5, 10) ~ "5-10",
                                  between(post30_los, 11, 10000) ~ "10+",
                                  TRUE ~ "Missing") %>%
      fct_relevel("0", "1", "2-3", "5-10", "10+") %>% 
      ff_label("Length of Stay (days)"),
    # symptom onset diagnosis ----
    pre_symp_adm_day = parse_number(pre_symp_adm_day),
    pre_symp_adm_day.groups = case_when(pre_symp_adm_day == 0 ~ "0 days",
                                        between(pre_symp_adm_day, 1, 2) ~ "1-2 days",
                                        between(pre_symp_adm_day, 3, 7) ~ "3-7 days",
                                        between(pre_symp_adm_day, 8, 14) ~ "1-2 weeks",
                                        between(pre_symp_adm_day, 15, 28) ~ "2-4 weeks",
                                        between(pre_symp_adm_day, 29, 182) ~ "1-6 months",
                                        between(pre_symp_adm_day, 182, 10000000) ~ "6+ months",
                                        TRUE ~ "Missing") %>%
      fct_relevel("0 days", "1-2 days", "3-7 days", "1-2 weeks", "2-4 weeks", "1-6 months", "6+ months") %>% 
      ff_label("Symptom onset vs diagnosis (days)"),
    # diagnosis vs decision to operate ----
    pre_diag_dec_day.groups = case_when(pre_diag_dec_day == 0 ~ "0 days",
                                        between(pre_diag_dec_day, 1, 2) ~ "1-2 days",
                                        between(pre_diag_dec_day, 3, 7) ~ "3-7 days",
                                        between(pre_diag_dec_day, 8, 14) ~ "1-2 weeks",
                                        between(pre_diag_dec_day, 15, 28) ~ "2-4 weeks",
                                        between(pre_diag_dec_day, 29, 182) ~ "1-6 months",
                                        between(pre_diag_dec_day, 182, 10000000) ~ "6+ months",
                                        TRUE ~ "Missing") %>%
      fct_relevel("0 days", "1-2 days", "3-7 days", "1-2 weeks", "2-4 weeks", "1-6 months", "6+ months") %>% 
      ff_label("Diagnosis vs decision to operate (days)"),
    # decision to operate vs operation ----
    pre_dec_op_day = parse_number(pre_dec_op_day),
    pre_dec_op_day.groups = case_when(pre_dec_op_day == 0 ~ "0 days",
                                      between(pre_dec_op_day, 1, 2) ~ "1-2 days",
                                      between(pre_dec_op_day, 3, 7) ~ "3-7 days",
                                      between(pre_dec_op_day, 8, 14) ~ "1-2 weeks",
                                      between(pre_dec_op_day, 15, 28) ~ "2-4 weeks",
                                      between(pre_dec_op_day, 29, 182) ~ "1-6 months",
                                      between(pre_dec_op_day, 182, 10000000) ~ "6+ months",
                                      TRUE ~ "Missing") %>%
      fct_relevel("0 days", "1-2 days", "3-7 days", "1-2 weeks", "2-4 weeks", "1-6 months", "6+ months") %>% 
      ff_label("Decision to operate vs operation (days)"),
    # anaesthetic type ----
    op_anaes.grouped = op_anaes %>% 
      str_replace("Total Intravenous Volatile Anaesthetic \\(TIVA\\)", "TIVA") %>% 
      str_remove(" \\(e.g., midazolam\\)") %>% 
      fct_explicit_na("Missing") %>% 
      fct_lump(8, other_level = "Other")
    # end ----
  ) %>% 
  ff_relabel(labels_keep)

  

appdata %>% 
  count(op_contam, sort = TRUE)


appdata %>% 
  filter(pre_dec_op_day < 1000) %>% 
  ggplot(aes(pre_dec_op_day)) +
  geom_histogram(binwidth = 1)

appdata %>%
  #filter(admission_prior < 10) %>%
  ggplot(aes(pre_dec_op_day.groups)) +
  geom_bar()

# %>% #select_if(is.factor) %>% 
#mutate_if(is.factor, fct_explicit_na, na_level = "Missing") %>% 
# 
# mutate(mortality2 = fct_collapse(mortality, 
#                                  "Died" = c("[b] Died in hospital, within 30 days of surgery",
#                                             "[c] Died outside of hospital, within 30 days of surgery")))


#%>% 
#  modify(function(.x){levels(.x)=str_trunc(levels(.x),25)})

# levels(appdata$diagnosis_timing) = str_trunc(levels(appdata$diagnosis_timing), width = 25)


appdata = appdata %>%
  select(-record_id, -age_years, -admission_prior, -post30_los,
         -pre_symp_adm_day,
         -pre_diag_dec_day,
         -pre_dec_op_day,
         -pt_comorbid,
         -pre_img_finding,
         -pre_img_finding_cbd,
         -op_anaes)


# pointblank::scan_data(appdata, sections = "OV")


allvars = appdata %>%
  select(-contains("_day")) %>% 
  finalfit::extract_labels() %>%
  select(vname, vfill) %$%
  setNames(as.list(vname), vfill)



updated_date = format(Sys.time(), format = "%d-%B %Y")

usethis::use_data(appdata, overwrite = TRUE)
usethis::use_data(allvars, overwrite = TRUE)
usethis::use_data(updated_date, overwrite = TRUE)
}