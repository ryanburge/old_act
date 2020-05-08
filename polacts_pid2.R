
graph <- full %>% 
  mutate(pid2 = frcode(pid7 == 1 | pid7 == 2 | pid7 == 3 ~ "Democrat",
                       pid7 == 5 | pid7 == 6 | pid7 == 7 ~ "Republican")) %>% 
  group_by(year, pid2, age) %>% 
  mean_ci(acts) %>% 
  na.omit()

graph %>% 
  ggplot(., aes(x = age, y = mean, color = pid2, group = pid2)) +
  geom_point(size = .25, alpha = .4) +
  smooth() +
  facet_wrap(~year) +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3")) +
  theme_gg("Muli", legend = TRUE) + 
  labs(x = "", y = "Mean Number of Activities", title = "Political Activites by Age and Partisanship", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/polact_age_year.png", type = "cairo-png")

