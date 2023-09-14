## code to prepare `appdata` dataset goes here
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

appdata = patient_data_orig %>% 
  select(-redcap_data_access_group) %>% 
  mutate(ALL = factor("ALL")) %>% 
  mutate(age.groups = case_when(age_years < 18 ~ "Missing",
                                between(age_years, 18, 30) ~ "18-30",
                                between(age_years, 31, 50) ~ "31-50",
                                between(age_years, 51, 70) ~ "51-70",
                                between(age_years, 71, 120) ~ "71+",
                                TRUE ~ "Missing") %>% 
           ff_label("Age"),
         admission_prior = parse_number(admission_prior),
         admission_prior.groups = case_when(admission_prior == 0 ~ "None",
                                            between(admission_prior, 1, 1) ~ "1",
                                            between(admission_prior, 2, 3) ~ "2-3",
                                            between(admission_prior, 5, 10) ~ "5-10",
                                            between(admission_prior, 11, 10000) ~ "10+",
                                            TRUE ~ "Missing") %>% 
           fct_relevel("None", "1", "2-3", "5-10", "10+") %>% 
           ff_label("Prior admissions"),
         post30_los.groups = case_when(post30_los == 0 ~ "0",
                                       between(post30_los, 1, 1) ~ "1",
                                       between(post30_los, 2, 3) ~ "2-3",
                                       between(post30_los, 5, 10) ~ "5-10",
                                       between(post30_los, 11, 10000) ~ "10+",
                                       TRUE ~ "Missing") %>%
           fct_relevel("0", "1", "2-3", "5-10", "10+") %>% 
           ff_label("Length of Stay (days)")
         )

# appdata %>% 
#   #filter(admission_prior < 10) %>% 
#   ggplot(aes(post30_los.groups)) +
#   geom_bar()

# %>% #select_if(is.factor) %>% 
#mutate_if(is.factor, fct_explicit_na, na_level = "Missing") %>% 
# 
# mutate(mortality2 = fct_collapse(mortality, 
#                                  "Died" = c("[b] Died in hospital, within 30 days of surgery",
#                                             "[c] Died outside of hospital, within 30 days of surgery")))


#%>% 
#  modify(function(.x){levels(.x)=str_trunc(levels(.x),25)})

# levels(appdata$diagnosis_timing) = str_trunc(levels(appdata$diagnosis_timing), width = 25)


allvars = appdata %>%
  select(-record_id, -age_years, -admission_prior, -post30_los) %>% 
  select(-contains("_day")) %>% 
  finalfit::extract_labels() %>%
  select(vname, vfill) %$%
  setNames(as.list(vname), vfill)


usethis::use_data(appdata, overwrite = TRUE)
usethis::use_data(allvars, overwrite = TRUE)
