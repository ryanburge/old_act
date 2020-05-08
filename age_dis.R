
graph <- full %>% 
  mutate(age2 = frcode(age >= 18 & age <= 35 ~ "18-35",
                       age >= 36 & age <= 44 ~ "36-44",
                       age >= 45 & age <= 54 ~ "45-54",
                       age >= 55 & age <= 64 ~ "55-64", 
                       age >= 65 & age <= 74 ~ "65-74",
                       age >= 75             ~ "75+")) %>% 
  group_by(year) %>% 
  ct(age2, wt = weight, show_na = FALSE)


graph %>% 
  ggplot(., aes(x = 1, y = pct, fill = fct_rev(age2))) +
  geom_col(color = "black") + 
  coord_flip() +
  facet_wrap(~ year, ncol =1, strip.position = "left") +
  scale_fill_d3() + 
  theme_gg("Muli") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  theme(strip.text.y = element_text(angle = 180)) +
  guides(fill = guide_legend(reverse=T, nrow = 1)) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank()) +
  geom_text(aes(label = ifelse(pct >.05, paste0(pct*100, '%'), '')), position = position_stack(vjust = 0.5), size = 4, family = "font", color = "black") +
  labs(x = "", y = "", title = "Age Distribution of the CCES", subtitle = "", caption = "Data: CCES 2018") +
  ggsave("D://old_act/images/age_distribution.png", width = 9, height = 6)

