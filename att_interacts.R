
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
  mutate(age2 = frcode(age >= 18 & age <= 35 ~ "18-35",
                       age >= 36 & age <= 64 ~ "36-64",
                       age >= 65 ~ "65+")) 


reg_m <- glm(meet ~ att*age2*pid2 + interest + educ + income + race + male, data = regg, family = "binomial")
reg_v <- glm(work ~ att*age2*pid2 + interest + educ + income + race + male, data = regg, family = "binomial")
reg_d <- glm(donate ~ att*age2*pid2 + interest + educ + income + race + male, data = regg, family = "binomial")
reg_s <- glm(sign ~ att*age2*pid2 + interest + educ + income + race + male, data = regg, family = "binomial")


gg_m <- interact_plot(reg_m, pred= att, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 
gg_v <- interact_plot(reg_v, pred= att, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 
gg_d <- interact_plot(reg_d, pred= att, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 
gg_s <- interact_plot(reg_s, pred= att, modx = age2, mod2 = pid2, mod2.labels = c("Democrat", "Republican"), int.width = .76, interval = TRUE) 


one <- gg_m +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "", y = "Pr(Likelihood)", title = "Interaction of Church Attendance and Age on Political Activity", subtitle = "Political Meeting", caption = "") +
  ggsave("D://old_act/images/meet_int_att.png", type = "cairo-png", width = 9)

two <- gg_v +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "", y = "Pr(Likelihood)", title = "", subtitle = "Work for Candidate", caption = "") +
  ggsave("D://old_act/images/vol_int_att.png", type = "cairo-png", width = 9)

three <- gg_d +
  theme_gg("Muli") +
  color4_3() +
  fill4_3() +
  # theme(legend.position = c(1.05, -.135)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "", y = "Pr(Likelihood)", title = "", subtitle = "Donate to Candidate", caption = "") +
  ggsave("D://old_act/images/donate_int_att.png", type = "cairo-png", width = 9)

four <- gg_s +
  theme_gg("Muli", legend = TRUE) +
  color4_3() +
  fill4_3() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c("Never", "", "Yearly", "", "Weekly", "")) +
  theme(panel.spacing = unit(2, "lines")) +
  labs(x = "", y = "Pr(Likelihood)", title = "", subtitle = "Display Sign", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/sign_int_att.png", type = "cairo-png", width = 9)

library(patchwork) 

all <- (one | two) / (three | four)

ggsave("D://old_act/images/all_four_acts.png", type = "cairo-png", all, width = 10, height = 8)