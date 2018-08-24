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

#웹 크롤링 - 공항 이용자수 지도에 표현###########################
install.packages("rvest")
library(rvest)
library(ggplot2)
library(dplyr)
install.packages("xml2")
library(xml2)
library(ggmap)
html.airports = read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic")
#위키피디아에서 airports by passenger 로 검색하고 이 링크를 복사



df = html_table(html_nodes(html.airports,"table")[[1]],fill = TRUE)

head(df)
View(df)

# 전처리 시작

#6번째 열의 이름을 total로 변경하기
colnames(df)[6] = "total"

#total의 필요없는 부분 제거하기

df$total

#ex)
df$total = gsub('\\[[0-9]+\\]', '', df$total)

#gsub 함수 - 특정 문자를 바꾸는 기능

df$total
df$total = gsub(',','',df$total)
#쉼표를 제거하라!

df$total
#but, 여전히 문자로 인식되고 있음


#total 열을 숫자로 변환하기
df$total = as.numeric(df$total)
df$total


#공항들의 위도와 경도값 알아오기

#ggmap 패키지를 실행시켜야함
gc = geocode(df$Airport) #위도 경도 스스로 찾게 해줌

gc

df = cbind(df, gc) #묶어주기


df


#세계 지도 불러오기

world = map_data("world")


#지도 위에 그리기
ggplot(df, aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey75", color = "grey70") +
  geom_point(color = "dark red", alpha = .25, aes(size = total)) +
  geom_point(color = "dark red", alpha = .75, shape = 1, aes(size = total)) +
  theme(legend.position = 'none')

View(df)
