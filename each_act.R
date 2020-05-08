

fun <- function(df, var, name){
  
  df %>% 
    filter(age >= 65) %>% 
    group_by(year) %>% 
    mean_ci({{var}}) %>% 
    mutate(type = name)
  
}

aaa1 <- full %>% fun(meet, "Attend Political Meeting")
aaa2 <- full %>% fun(sign, "Display Yard Sign")
aaa3 <- full %>% fun(work, "Work for Campaign")
aaa4 <- full %>% fun(donate, "Donate Money")

graph <- bind_df("aaa")

graph %>% 
  ggplot(., aes(x = year, y = mean, fill = type)) +
  geom_col(color = "black") +
  facet_wrap(~ type) +
  theme_gg("Muli") +
  error_bar() +
  fill4_1() +
  y_pct() +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  lab_bar(top = FALSE, type = mean, pos = .03, sz = 4) +
  labs(x = "", y = "Percent Engaging in Activity", title = "Political Activites by Year", subtitle = "Among Those 65 and Over", caption = "Data: CCES 2008-2018") +
  ggsave("D://old_act/images/each_act.png", type = "cairo-png", width = 7)