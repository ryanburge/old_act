library(interactions)

## Attendance ####

regg <- full %>% 
  filter(religion != 9) %>% 
  filter(religion != 10) %>% 
  filter(religion != 11) %>% 
  filter(attend <= 6) %>% 
  mutate(att = 7 - attend) %>% 
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(male = case_when(gender == 1 ~ 1, TRUE ~0 )) %>% 
  mutate(age2 = frcode(age >= 65 ~ "65+",
                       age >= 36 & age <= 64 ~ "36-64",
                       age >= 18 & age <= 35 ~ "18-35")) 
  

reg1 <- lm(acts ~ att*age2*pid2 + interest + educ + income + race + male, data = regg)

gg <- interact_plot(reg1, pred= att, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 

gg +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(panel.spacing = unit(2, "lines")) +
  theme(legend.position = c(.3, .2)) +
  labs(x = "", y = "Pr(# of Political Acts)", title = "Interaction of Church Attendance and Age on Political Activity", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/interact_att_pid2_age2.png", type = "cairo-png", width = 9)

## Interest #### 


regg <- full %>% 
  filter(attend <= 6) %>%
  filter(interest <= 4) %>% 
  mutate(interest =  5 - interest) %>% 
  mutate(att = 7 - attend) %>% 
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(age2 = frcode(age >= 65 ~ "65+",
                       age >= 36 & age <= 64 ~ "36-64",
                       age >= 18 & age <= 35 ~ "18-35")) 


reg1 <- lm(acts ~ interest*age2*pid2 + attend + educ + income + race, data = regg)

gg <- interact_plot(reg1, pred= interest, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 

gg +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(panel.spacing = unit(2, "lines")) +
  theme(legend.position = c(.3, .2)) +
  labs(x = "", y = "Pr(# of Political Acts)", title = "Interaction of Church Attendance and Age on Political Activity", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/interact_att_pid2_age2.png", type = "cairo-png", width = 9)


## Education #### 

regg <- full %>% 
  mutate(att = 7 - attend) %>% 
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  mutate(white = case_when(race == 1 ~ 1, TRUE ~ 0)) %>% 
  mutate(male = case_when(gender == 1 ~ 1, TRUE ~0 )) %>% 
  mutate(age2 = frcode(age >= 65 ~ "65+",
                       age >= 36 & age <= 64 ~ "36-64",
                       age >= 18 & age <= 35 ~ "18-35")) 


reg1 <- lm(acts ~ educ*age2*pid2 + interest + att + income + race + male, data = regg)

gg <- interact_plot(reg1, pred= educ, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 

gg +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("No HS", "HS Grad", "Some\nCollege", "2 Yr.", "4 Yr.", "Grad.")) +
  theme(panel.spacing = unit(2, "lines")) +
  theme(legend.position = c(.3, .2)) +
  labs(x = "", y = "Pr(# of Political Acts)", title = "Interaction of Education and Age on Political Activity", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/interact_ed_pid2_age2.png", type = "cairo-png", width = 9)
