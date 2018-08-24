library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)


welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)

class(welfare$code_job)
table(welfare$code_job)

library(readxl)

list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)

head(list_job)

dim(list_job)

welfare = left_join(welfare, list_job, id = "code_job")

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)

job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()



