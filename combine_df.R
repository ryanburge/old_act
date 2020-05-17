

reg08 <- cces08 %>% 
  mutate(age = 2008 - V207) %>% 
  mutate(pid7 = CC307a) %>% 
  mutate(meet = car::recode(CC415_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC415_3, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC415_4, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC415_6, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = V217, interest = V244, educ = V213, race = V211, income = V246, weight = V201, gender = V208, religion = V219) %>% 
  mutate(year = 2008) %>% 
  as_tibble()


reg10 <- cces10 %>% 
  mutate(age = 2010 - V207) %>% 
  mutate(pid7 = V212c) %>% 
  mutate(meet = car::recode(CC417a_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC417a_2, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC417a_3, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC417a_4, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = V217, interest = V244, educ = V213, race = V211, income = V246, weight = V101, gender = V208, religion = V219) %>% 
  mutate(year = 2010) %>% 
  as_tibble()

reg12 <- cces12 %>% 
  mutate(age = 2012 - birthyr) %>% 
  mutate(meet = car::recode(CC417a_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC417a_2, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC417a_3, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC417a_4, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = pew_churatd, interest = newsint, educ, race, income = faminc, weight = weight_vv, gender, religion = religpew) %>% 
  mutate(year = 2012) %>% 
  as_tibble()

reg14 <- cces14 %>% 
  mutate(age = 2014 - birthyr) %>% 
  mutate(meet = car::recode(CC417a_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC417a_2, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC417a_3, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC417a_4, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = pew_churatd, interest = newsint, educ, race, income = faminc, weight = weight, gender, religion = religpew) %>% 
  mutate(year = 2014) %>% 
  as_tibble()

reg16 <- cces16 %>% 
  mutate(age = 2016 - birthyr) %>% 
  mutate(meet = car::recode(CC16_417a_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC16_417a_2, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC16_417a_3, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC16_417a_4, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = pew_churatd, interest = newsint, educ, race, income = faminc, weight = commonweight_vv, gender, religion = religpew) %>% 
  mutate(year = 2016) %>% 
  as_tibble()

reg18 <- cces18 %>% 
  mutate(age = 2018 - birthyr) %>% 
  mutate(meet = car::recode(CC18_417a_1, "1=1; 2=0")) %>% 
  mutate(sign = car::recode(CC18_417a_2, "1=1; 2=0")) %>% 
  mutate(work = car::recode(CC18_417a_3, "1=1; 2=0")) %>% 
  mutate(donate = car::recode(CC18_417a_6, "1=1; 2=0")) %>% 
  mutate(acts = meet + sign + work + donate) %>% 
  select(meet, sign, work, donate, age, pid7, acts, attend = pew_churatd, interest = newsint, educ, race, income = faminc_new, weight = commonweight, gender, religion = religpew) %>% 
  mutate(year = 2018) %>% 
  as_tibble() 

full <- bind_rows(reg08, reg10, reg12, reg14, reg16, reg18)
