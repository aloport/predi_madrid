library(tidyverse)
library(summarytools)

library(haven)


madsur_raw <- read_sav('/Users/ortega/Dropbox/My Mac (pwiortega-mac.local)/Downloads/UNZUES_218856/UNZUES_218856.sav')


madsur_clean <- madsur_raw %>% 
select(gender=SEXO, age=EDADR, edu=EDUCACION, political_interest= P1,
         partisanship_pp= P2_1, partisanship_ps= P2_2, partisanship_mm= P2_3,
         partisanship_up= P2_4, partisanship_vx= P2_5,partisanship_cs= P2_6,
         partisanship_no= "P2_NO#1", partisanship_one= P2R_1, partisanship_two= P2R_2,
       partisanship_three= P2R_3,
         polarisation= P3, likely_government= P4, likely_pp= P5, likely_ps= P5_1,
         likely_cs= P6, likely_mm= P7_3, likely_up= P7_4, likely_vx= P7_5,
         substantive_coalition= P8A, abstract_coalition= P8B) %>% 
  mutate(partisanship_one= as.numeric(partisanship_one)) %>% 
  mutate(partisanship_no= as.numeric(partisanship_no)) %>% 
  mutate(partisanship_two= as.numeric(partisanship_two)) %>% 
  mutate(partisanship_three= as.numeric(partisanship_three)) %>% 
  mutate_at(.vars= vars(partisanship_pp:partisanship_three),  funs(case_when(
    is.na(.)  ~ 0,
    TRUE ~ .
  ))) %>% 
  mutate_at(.vars= vars(partisanship_one:partisanship_three),  funs(case_when(
    .== 1 ~ "pp",
    .== 2 ~ "ps",
    .== 3 ~ "mm",
    .== 4 ~ "up",
    .== 5 ~ "vx",
    .== 6 ~ "cs"
  )))

test <- madsur_clean %>% count(partisanship_one, partisanship_two, partisanship_three)

madsur_clean  
  model <- lm(polarisation ~ gender + age + edu + political_interest + partisanship_one, data = madsur_clean)
summary(model)
  

model_gov <- lm(likely_government ~ gender + age + edu + political_interest + partisanship_one, data = madsur_clean)
summary(model_gov)


polarised_who <- madsur_clean %>% 
  group_by(partisanship_one) %>% 
  summarise(across( .cols = c("polarisation", "political_interest"), .fns = list(Mean = mean, SD = sd))) %>% 
   drop_na(partisanship_one) %>% mutate(polarisation_Mean= 10-polarisation_Mean, 
                                        political_interest_Mean= 10-political_interest_Mean)
