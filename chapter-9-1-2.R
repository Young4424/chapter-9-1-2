library(foreign)
library(dplyr)
install.packages("dplyr")
library(ggplot2)
library(readxl)

raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

welfare = raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)



welfare = rename(welfare,
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

#데이터 분석 절차
#1. 변수 검토 및 전처리, 2. 변수 간 관계 분석

#09-2. 성별에 따른 월급 차이
#- 성별에 따라 월급이 다를까?

#1. 변수 검토 및 전처리 - 성별, 월급
class(welfare$sex)
table(welfare$sex)

#이상치 확인
table(welfare$sex)
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)

table(is.na(welfare$sex)) #결측치 확

#성별에 항목 이름 부여
welfare$sex = ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

#월급 변수 검토 및 전처리
#1. 변수 검토하기
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)

#x축이 길다?
qplot(welfare$income) + xlim(0,1000)

# 2. 전처리
#이상치 확인
summary(welfare$income)

#이상치 결측 처리
welfare$income = ifelse(welfare$income %in% c(0,9999),NA, welfare$income)

#결측치 확인
table(is.na(welfare$income))




#2. 변수 간 관계 분석 - 성별 월급 평균표 만들기, 그래프 만들기

#성별에 따른 월급 차이 분석하기
# 1. 성별 월급 평균표 만들기
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income
#2. 그래프 만들기
ggplot(data = sex_income, aes(x = sex, y = mean_income)) +
  geom_col()

#09-3. 나이와 월급의 관계 - 몇살 떄 월급을 가장 많이 받을까?

#1. 변수 검토 및 전처리 - 나이 , 월급
#(1) 변수 검토하기
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#(2) 전처리
#이상치 확인
summary(welfare$birth)
table(is.na(welfare$birth))

welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

#(3) 파생 변수 만들기
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)


#2. 변수 간 관계 분석 - 나이에 따른 월급 평균표 만들기, 그래프 만들기

#(1) 나이에 따른 월급 평균표 만들기
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>%
  summarise(mean_income = mean(income))

head(age_income)

#(2) 그래프 만들기
ggplot(data = age_income, aes(x = age, y = mean_income)) +
  geom_line()

#09-4 연령대에 따른 월급 차이이 - "어떤 연령대의 월급이 많을까?"

#1. 변수 검토 및 전처리 - 연령대, 월급
#(1) 파생 변수 만들기 - 연령대
welfare = welfare %>% 
  mutate(ageg = ifelse(age < 30, "young",
                       ifelse(age <= 59, "middle", "old")))

table(welfare$ageg)
qplot(welfare$ageg)




#2. 변수 간 관계 분석- 연령대별 월급 평균표 만들기, 그래프 만들기
#(1) 연령대별 월급 평균표 만들기

ageg_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))

ageg_income

#(2) 그래프 만들기
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) +
  geom_col()

#09-5. 연령대 및 성별 월급 차이 - "성별 월급 차이는 연령대별로 다를까?"

#1. 변수 검토 및 전처리 - 연령대 , 성별 , 월급
#(1) 연령대 및 성별 월급 평균표 만들기
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg, sex) %>%  #연령대 및 성별로 그룹 묶기
  summarise(mean_income = mean(income))

sex_income




#2. 변수 간 관계 분석 - 연령대 및, 성별 월급 평균표 만들기,그래프 만들기

#fill : 채워라 하는 것
ggplot(data = sex_income, aes(x =ageg, y = mean_income, fill = sex)) +
  geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))
#그래프의 순서 정하기 + 분리시킴

#성별 연령별 월급 평균표 만들기
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>%  # 성별, 연령별로 그룹!
  summarise(mean_income = mean(income))

head(sex_age)

#(2) 그래프 만들기
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) +
  geom_line()

#09-6 직업별 월급차이 - "어떤 직업이 월급을 가장 많이 받을까?"
#1. 변수 검토 및 전처리 - 직업, 월급
#(1) 변수 검토하기
class(welfare$code_job)
table(welfare$code_job)

#(2) 전처리
#직업분류코드 목록 불러오기
library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)

#welfare에 직업명 결합
welfare = left_join(welfare, list_job, id = "code_job")
#왼쪽으로 붙혀주자 = left_join

welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job, job) %>% 
  head(10)





#2. 변수 간 관계 분석 - 직업별 월급 평균표 만들기, 그래프 만들기
#(1) 직업별 월급 평균표 만들기
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

head(job_income)

#(2) 상위 10개 추출
top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

#(3) 그래프 만들기
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() #옆으로 뒤집어라!

#(4) 하위 10위 추출
bottom10 = job_income %>% 
  arrange(mean_income) %>% 
  head(10)

bottom10

#(5) 그래프 그리기
ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income))+
  geom_col() +
  coord_flip() +
  ylim(0, 850)


#09-7 성별 직업 빈도 - 성별로 어떤 직업이 가장 많을까?"
#1. 변수 검토 및 전처리 - 성별, 직업

#(1) 성별 직업 빈도 상위 10개 추출
#남성
job_male = welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% #종사자 수 표기 
  arrange(desc(n)) %>% 
  head(10)

job_male

#여성
job_female = welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female


#2. 변수 간 관계 분석 - 성별 직업 빈도표 만들기 , 그래프 만들기

#(1) 그래프 만들기
ggplot(data = job_male, aes(x = reorder(job,n), y = n)) +
  geom_col() +
  coord_flip()

#여성
ggplot(data = job_female, aes(x = reorder(job,n), y = n)) +
  geom_col() +
  coord_flip()

#09-8 종교 유무에 따른 이혼율 - "종교가 있는 사람들이 이혼을 덜할까?"

#1. 변수 검토 및 전처리 - 종교, 혼인상태
#(1) 변수 검토하기
class(welfare$religion)
table(welfare$religion)

#(2) 전처리
#종교 유무 이름 부여
welfare$religion = ifelse(welfare$religion == 1,"yes","no")
table(welfare$religion)

qplot(welfare$religion)

#혼인 상태 변수 검토, 전처리
#(1) 변수 검토
class(welfare$marriage)
table(welfare$marriage)

#(2) 이혼 여부 변수 만들기
welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorce",NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)


#2. 변수 간 관계 분석- 종교 유무에 따른 이혼율 표 만들기, 그래프 만들기

#(1) 종교 유무에 따른 이혼율 표 만들기
religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))
#round 함수 = 반올림 함수, 소수점 이하 한자리에서 반올림!
religion_marriage


#count() 활용
religion_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100,1))

#(2) 이혼율 표 만들기
divorce = religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

divorce

#(3) 그래프 만들기
ggplot(data = divorce, aes(x = religion, y = pct)) +
  geom_col()
# 5%이상의 차이가 유의마한 차이라고 인정함

###연령대 및 종교 유무에 따른 이혼율 분석하기
#(1) 연령대별 이혼율 표 만들기
ageg_marriage = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

ageg_marriage

#(2) 초년 제외, 이혼 추출
ageg_divorce = ageg_marriage %>% 
  filter(ageg != "young" & group_marriage == "divorce") %>% 
  select(ageg, pct)

ageg_divorce


#그래프 만들기
ggplot(data = ageg_divorce, aes(x = ageg, y = pct)) +
  geom_col()

#(3) 연령대, 종교유무 결혼상태별 비율표

ageg_religion_marriage = welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100,1))

ageg_religion_marriage


df_divorce = ageg_religion_marriage %>% 
  filter(group_marriage == "divorce") %>% 
  select(ageg, religion, pct)

df_divorce

#(4) 연령대 및 종교 유무에 따른 이혼율 그래프
ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) +
  geom_col(position = "dodge")

#09-9 지역별 연령대 비율
#(1) 변수 검토 - 지역, 연령대
class(welfare$code_region)
table(welfare$code_region)

list_region = data.frame(code_region = c(1:7),
                         region = c("서울",
                                    "수도권(인천/경기",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))

list_region                                         

#(2) welfare에 지역명 변수 추가
welfare = left_join(welfare, list_region, id = "code_region")

welfare %>% 
  select(code_region, region) %>% 
  head

#2. 지역별 연령대 비율표 만들기

region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 2))

head(region_ageg)

#(2) 그래프 만들기
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# (3) 막대 정렬하기
#노년층 비율 내림차순 정령
list_order_old = region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

list_order_old 

