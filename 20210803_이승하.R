### 데이터 확인 및 현황 파악 ###

# 최초 작성 : 2021-07-17(토)
# 추가 작성 : 2021-07-24(토)
#      수정 : 2021-07-26(월)

# Encoding : UTF-8
# (Rstudio "File" - "Reopen with Encoding" - "UTF-8")



# settings ----------------------------------------------------------------


### options ###
options(scipen = 20)


### Working Directory 설정 ###
# 데이터(csv 파일) 있는 폴더로 각자 설정
# setwd("C:\\Users\\memoon\\Desktop\\빅데이터아카데미\\멘토링 예시코드 - 3조 문믿음")


### install, load and attach packages ###
# user-defined function

install.packages("dlookr")

myfcn_pkgInstallAttachLoad <- function(pkgs_attach, pkgs_load) {
  
  # install packages
  for (tmp_pkg in c(pkgs_attach, pkgs_load)) {
    
    if (!requireNamespace(package = tmp_pkg, quietly = TRUE)) {
      install.packages(pkgs = tmp_pkg)
    }
  }
  
  # attach packages
  for (tmp_pkg in pkgs_attach) {
    
    library(package = tmp_pkg, character.only = TRUE)
  }
  
  # load packages
  for (tmp_pkg in setdiff(pkgs_load, loadedNamespaces())) {
    
    loadNamespace(tmp_pkg)
  }
  
  return(invisible(NULL))
}

# 인터넷 연결 필요(CRAN 접속)
myfcn_pkgInstallAttachLoad(
  
  pkgs_attach = c("magrittr", "tidyverse", "glmnet", "randomForest", "dlookr", "ggplot"),
  pkgs_load = c("readr", "pillar", "psych", "Hmisc", "skimr",
                "corrplot", "VIM", "DMwR2", "mice", "MLmetrics")
)



# load datasets -----------------------------------------------------------
# 데이터 출처 : DACON 주차수요 예측 AI 경진대회 
# [https://dacon.io/competitions/official/235745/data]

# file encoding 확인
readr::guess_encoding(file = "train.csv")

# 현재 working directory 내에 .csv로 끝나는 파일 목록 저장
files_csv <- list.files(path = getwd(), pattern = ".csv$")


### csv 파일별로 data.frame 만들어서 list의 element로 저장 ###

# list 만들기
csv_list <- vector(mode = 'list', length = length(files_csv))

# 이름 설정 (`stringr::word`)
names(csv_list) <- word(files_csv, start = 1, end = 1, sep = "\\.")

# csv 이름별로 반복문
for (tmp_file in files_csv) {
  # tmp_file <- files_csv[1]
  
  tmp_name <- word(tmp_file, 1, 1, sep = "\\.")
  
  csv_list[[tmp_name]] <- read.csv(
    tmp_file,
    header = TRUE,
    fileEncoding = "UTF-8",
    stringsAsFactors = FALSE
  )
  
  rm(list = ls(pattern = "^tmp"))
}



# check training & test data ----------------------------------------------


### 데이터 현황 파악 시작할때 쓸 수 있는 함수 예시들 ###
# 패키지, 함수는 취향껏 사용!!!
base::summary  (csv_list$train)
utils::str     (csv_list$train)
pillar::glimpse(csv_list$train)
psych::describe(csv_list$train)
Hmisc::describe(csv_list$train)
skimr::skim    (csv_list$train)


### 임대보증금, 임대료를 numeric으로 변경 ###
# "", "-" 존재
sort(unique(csv_list$train$임대보증금))
sort(unique(csv_list$train$임대료))
sort(unique(csv_list$test $임대보증금))
sort(unique(csv_list$test $임대보증금))

# "", "-" NA로 변경
tr_df <- csv_list$train %>% mutate(
  임대보증금 = as.double(ifelse(임대보증금 %in% c("", "-"), NA, 임대보증금)),
  임대료     = as.double(ifelse(임대료     %in% c("", "-"), NA, 임대료    ))
)
te_df <- csv_list$test  %>% mutate(
  임대보증금 = as.double(ifelse(임대보증금 %in% c("", "-"), NA, 임대보증금)),
  임대료     = as.double(ifelse(임대료     %in% c("", "-"), NA, 임대료    ))
)


### 변수명 긴 것들 이름 바꾸기
tr_df <- rename(tr_df,
                c("버스정류장수" = "도보.10분거리.내.버스정류장.수",
                  "지하철역수" = "도보.10분거리.내.지하철역.수.환승노선.수.반영."))

te_df <- rename(te_df,
                c("버스정류장수" = "도보.10분거리.내.버스정류장.수",
                  "지하철역수" = "도보.10분거리.내.지하철역.수.환승노선.수.반영."))


head(tr_df)
head(te_df)


# # (참고) as.double과 as.numeric은 같음
# #        is.double과 is.numeric은 다름
# print(as.double)
# print(as.numeric)
# identical(as.double, as.numeric)
# print(is.double)
# print(is.numeric)
# identical(is.double, is.numeric)
# # "Details" 참고 => 날짜 관련 class들 처리 방식이 다름...
# help("is.numeric")

# training, test set column 확인
# => Y변수(등록차량수)는 training set에만 존재!
intersect(names(tr_df), names(te_df))
setdiff  (names(tr_df), names(te_df))

### 단지코드별로 unique한 column 확인 ###
# 단지 코드별로 등록 차량수는 모두 같음 (Y값 예측 단위는 단지 코드임)
tr_df %>%
  distinct(단지코드, 등록차량수) %>% 
  count(단지코드) %>% 
  count(n)

# 예시 : 단지 코드 내에 2개 이상의 값이 column 존재 => row 여러개
# modeling시는 단지 코드별로 row 1개가 나오도록 처리해줘야 함
tr_df %>% filter(단지코드 == "C2644")

# 단지 코드별로 값이 1개(또는 NA만 있으면 0개)인 column은??
num_uniq_int <- tr_df %>% 
  # 단지 코드별로 데이터 summary
  group_by(단지코드) %>% 
  # 모든 column에 대해, # column별로 unique한 값 중 NA를 제외한 개수 세기
  # (참고) ~ (x) : 익명 함수 (function(x) return(x))
  #                R Package `purrr` syntax
  # (참고) `data.table:::uniqueN` 사용 가능
  summarise(across(everything(), ~ length(na.omit(unique(.))))) %>% 
  # 단지 코드 제외하고
  select(-단지코드) %>% 
  # column별로 합 계산
  apply(2, sum) 

num_uniq_int

# 단지 코드 unique한 개수(423) 이하인 column은 단지 코드별로 값이 유일한 column
cols_uniq <- names(num_uniq_int)[num_uniq_int <= length(unique(tr_df$단지코드))]
cols_uniq <- c("단지코드", cols_uniq)

# unique한 column만 따로 저장
tr_uniq_df <- tr_df %>% 
  select(all_of(cols_uniq)) %>% 
  distinct(across(everything()))

tr_uniq_df

# test set에서도 확인
# unique한 단지 코드 수
length(unique(te_df$단지코드))
# `te_df`에 있는 `cols_uniq` column이 unique한지 확인
te_df %>% 
  select(any_of(cols_uniq)) %>% 
  distinct(across(everything())) %>% 
  nrow()
# unique한 column만 따로 저장
te_uniq_df <- te_df %>% 
  select(any_of(cols_uniq)) %>% 
  distinct(across(everything()))

# unique하지 않은 column들은 어떤 게 있을까?
tr_df %>% 
  select(-all_of(cols_uniq)) %>% 
  head()

# (주의) 공가수는 (아마도) 전용면적 상관없이 단지 전체 중복 들어간 듯
#        판단 근거 : 단지코드별로 공가수 값이 unique함
"공가수" %in% cols_uniq


### character column을 factor로 변경 (modeling 활용 위해서) ###
# character column 저장
cols_chr <- c("임대건물구분", "지역", "공급유형", "자격유형")

# 분포 확인 : column별 빈도 수
tr_df %>%
  select(all_of(cols_chr)) %>% 
  map(table)

# 분포 확인 : 조합별 빈도 수
tr_df %>% 
  select(all_of(cols_chr)) %>% 
  table() %>%
  as.data.frame() %>% 
  filter(Freq > 0) %>% 
  arrange(Freq)

# factor로 변경
# (참고) magrittr::`%<>%`
# a %<>% b  <=>  a <- a %>% b
# tr_df <- tr_df %>% mutate(across(cols_chr, as.factor))
tr_df %<>% mutate(across(cols_chr, as.factor))
te_df %<>% mutate(across(cols_chr, as.factor))

# (참고) `factor`함수 대신 `as.factor` 사용시 ordered = FALSE로 들어감
# i.e. 명목변수(norminal variable)
skimr::skim(tr_df)



# unique하지 않은 column 활용 방안?? --------------------------------------
# (예시) 전용면적, 전용면적별세대수 
# (주의) 예시에서 다루지 않은 변수들 활용 시 예측 정확도 올라갈 가능성 있음.


summary(tr_df)

### 전용면적 파생변수 생성 1 : 단지별 면적 총합 변수 생성 ###
# `전용면적`이 같은 경우 세대수 합쳐서 사용
# (주의) 이렇게 하면 unique하지 않은 column 정보를 무시한 파생변수 생성
#        => (예) `임대건물구분`(아파트 xor 상가)까지 고려한 합계는??
tr_area_total_df <- tr_df %>% 
  group_by(단지코드, 전용면적) %>% 
  summarise(전용면적별세대수 = sum(전용면적별세대수), .groups = 'rowwise') %>% 
  summarise(총면적 = 전용면적 * 전용면적별세대수, .groups = 'drop') %>% 
  group_by(단지코드) %>% 
  summarise(총면적 = sum(총면적))

# 총면적 변수 추가
tr_uniq_df %<>% inner_join(tr_area_total_df, by = "단지코드")

tr_uniq_df

### 전용면적 파생변수 생성 2 : 전용면적 구간별 빈도수 집계 ###
# (주의) 구간을 나누는 기준, 구간 개수 등은 정하기 나름
#        정답은 없음, modeling을 어떻게 할지에 따라 맞춰서 고민할 필요 있음음
#        (예1) 구간 내 빈도수가 비슷하게 quantile 값을 사용
#              (`Hmisc::cut2`)
#        (예2) 구간 width 고정해서(예 : 10) 구간 끊어서 사용
#              (주의) outlier(상한, 하한) 주의
#              (`base::round`, `graphics::hist`)
#        (예3) Y변수를 예측하는 의사결정나무를 만들어 구간 사용
#              (discretization using decision tree : `rpart::rpart`)

# 예 2-2 : 10 단위로 구간 끊기
#          10 단위로 끊으면 X변수 행렬이 sparse matrix
#          => 어떤 modeling을 쓸 때 적절할까??
tr_area_freq_df <- tr_df %>%
  # 1의 자리에서 반올림
  mutate(전용면적 = round(전용면적, -1)) %>%
  # 상한 100으로 제한
  mutate(전용면적 = pmin(전용면적, 100)) %>%
  # 하한 10으로 제한
  mutate(전용면적 = pmax(전용면적, 10)) %>% 
  group_by(단지코드, 전용면적) %>% 
  summarise(전용면적별세대수 = sum(전용면적별세대수), .groups = 'drop') %>% 
  # pivoting
  pivot_wider(
    
    id_cols = 단지코드,
    
    names_from = 전용면적,
    # zero padding
    names_glue = "전용면적_{str_pad(전용면적, 3, 'left', '0')}",
    
    values_from = 전용면적별세대수,
    values_fill = 0
  ) %>% 
  # column명 정렬
  select(all_of(sort(names(.))))

# 총면적 변수 추가
tr_uniq_df %<>% inner_join(tr_area_freq_df, by = "단지코드")


tr_uniq_df


# Plotting ----------------------------------------------------------------

### 변수 목록 저장 ###
# (주의) 예시에서는 unique한 변수만 사용 
#        unique하지 않은 변수들 보려면,
#        변수별로 중복값 제거해서 단지코드별로 보는 방법 존재

cols_num <- tr_uniq_df %>% 
  select(where(is.numeric)) %>%
  names() %>% 
  # Y변수는 제외
  setdiff("등록차량수")

cols_num

cols_fct <- tr_uniq_df %>% 
  select(where(is.factor)) %>%
  names()


view(tr_uniq_df)

### dlookr 패키지를 통하여 데이터 확인
# 지하철역수와 버스정류장수에 결측치가 존재함
diagnose(tr_uniq_df)

## 결측치 확인
# 지하철역수는 주로, 지하철역이 없거나(경상남도, 충청남도) 적은 곳(대전)에서 발생
# 0으로 대체 가능
tr_uniq_df %>%
  filter(is.na(지하철역수)) %>% group_by(지역) %>% summarise(count =n())
tr_uniq_df$지하철역수[is.na(tr_uniq_df$지하철역수)] <- 0

# 버스정류장수는 결측치가 1개 밖에 없음
# 0으로 대체
tr_uniq_df %>%
  filter(is.na(버스정류장수)) %>% group_by(지역) %>% summarise(count =n())
tr_uniq_df$버스정류장수[is.na(tr_uniq_df$버스정류장수)] <- 0


### column별 분포 그림 저장 ###
# working directory 내에 그림 저장할 폴더 생성
dir_distn <- file.path(getwd(), "변수별_분포그림")
if (!dir.exists(dir_distn)) dir.create(dir_distn)

for (tmp_name in cols_num) {
  # tmp_name <- cols_num[1]
  
  tmp_tr <- tr_uniq_df[[tmp_name]]
  
  png(
    filename = paste0(dir_distn, "/", "training_", tmp_name, ".png"),
    width = 1000, height = 500, res = 110
  )
  par(mfrow = c(1, 2))
  
  hist   (tmp_tr, main = paste0(tmp_name, " (training)"), xlab = "", las = 1)
  boxplot(tmp_tr, main = paste0(tmp_name, " (training)"), xlab = "", las = 1)
  
  dev.off()
}


### correlation ###
# X, Y 둘다 연속형일 때 : Pearson correlation coefficient
tr_uniq_df %>% 
  select(등록차량수, all_of(cols_num)) %>% 
  # (X, Y) 둘 다 문제 없는 pair들만 이용해 상관계수 계산
  cor(use = 'pairwise.complete.obs') %>% 
  corrplot::corrplot.mixed(tl.pos = 'd')


# 지하철역수에 따른 등록 차량 수
# 지하철역수가 적을수록 단지내 주차면수가 적고, 등록차량수가 적음을 확인 가능
ggplot() +
  geom_point(mapping=aes(x=단지내주차면수, y=등록차량수, color=factor(지하철역수), shape=factor(지하철역수)), data=tr_uniq_df) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()

# 버스 정류장수가 1~20까지 있고, 이를 확인하기 위해서 구간별로 나눔?
# hist 그래프를 그려보면 주로 0~5 구간에 몰려있고, 10이상은 극히 드묾을 알 수 있음
# 0 => 1구간, 1~3 => 2구간, 4~7 => 3구간, 8~10 => 4구간, 11~20 => 5구간 이런 식으로??
hist(tr_uniq_df$버스정류장수)
tr_uniq_df %>% group_by(버스정류장수) %>% summarise(count=n())

tr_uniq_df_temp <- tr_uniq_df

tr_uniq_df_temp$버스구간 = ifelse(tr_uniq_df_temp$버스정류장수 < 2, 0,
                        ifelse(tr_uniq_df_temp$버스정류장수 >= 2 & tr_uniq_df_temp$버스정류장수 < 4, 1,
                           ifelse(tr_uniq_df_temp$버스정류장수 >= 4 & tr_uniq_df_temp$버스정류장수 < 6, 2,
                              ifelse(tr_uniq_df_temp$버스정류장수 >= 6 & tr_uniq_df_temp$버스정류장수 < 10, 3, 4))))


ggplot() +
  geom_point(mapping=aes(x=단지내주차면수, y=등록차량수, color=factor(버스구간), shape=factor(버스구간)), data=tr_uniq_df_temp) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()


# 변수들 사이 상관계수 큼 => PCA(Principal Component Analysis; 주성분분석) 수행??



# 가장 상관계수 큰 `단지내주차면수` 변수와 Y변수 산점도
# base plot
# magrittr::`%$%` (Exposition pipe)
tr_uniq_df %$% plot(단지내주차면수, 등록차량수, pch = 20)
tr_uniq_df %$% cor.test(단지내주차면수, 등록차량수)
fit_lm <- lm(등록차량수 ~ 단지내주차면수, data = tr_uniq_df)
abline(a = fit_lm$coefficients[1], b = fit_lm$coefficients[2], col = "blue")
# ggplot
ggplot(tr_uniq_df, aes(단지내주차면수, 등록차량수)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  theme_bw()


### X : 이산형(discrete), Y : 연속형(continuous)일 때 ###
# (참고) 주차 대수 : 정수(integer) 1, 2, 3, ..., 
# (방법 1) Y를 이산형으로 보고 분석 : (예) Poisson Regression
# (방법 2) Y를 연속형으로 보고 분석 : (예) linear regression, random forest
# => 값 자체가 정수로 나오지 않으면 적당한 rule 추가??

# X : binary variable
tr_df %>% 
  distinct(단지코드, 임대건물구분, 등록차량수) %$%
  boxplot(등록차량수 ~ 임대건물구분, las = 1)
# 평균차 검정 : t.test
tr_df %>% 
  distinct(단지코드, 임대건물구분, 등록차량수) %$%
  t.test(등록차량수 ~ 임대건물구분)

# 3개 이상 범주
tr_df %>% 
  distinct(단지코드, 지역, 등록차량수) %$%
  boxplot(등록차량수 ~ 지역, las = 1)
# 평균차 검정 : anova
tr_df %>% 
  distinct(단지코드, 지역, 등록차량수) %$%
  lm(등록차량수 ~ 지역) %>% 
  anova()



# 결측치 처리 -------------------------------------------------------------
# 결측치 처리 역시 정답이 없음
# 여러 방법 중 상황에 맞는 방법을 조합해서 사용해야 함

### 결측치 분포 확인 ###


# `VIM::aggr` : 결측치 조합별 분포 확인 
# 그림 표시 위해 column명 긴 경우 임시로 변경
# tr_df %>% 
#   rename(
#     면적별세대수 = 전용면적별세대수,
#     주차면수 = 단지내주차면수
#   ) %>%
#   VIM::aggr(cex.axis = 0.6, numbers = TRUE, sortVars = TRUE)
# tr_uniq_df %>% 
#   rename(
#     주차면수 = 단지내주차면수
#   ) %>%
#   VIM::aggr(cex.axis = 0.6, numbers = TRUE, sortVars = TRUE)


# ### (방법 0) 외부 데이터 수집 또는 배경 지식을 통해 결측값 채우기 ###
# # 가능하면...
# 
# 
# ### (방법 1) row(observation) 제외 또는 column(variable) 제외 ###
# # 1-1. 결측치 column 제거 
# tr_uniq_df$도보.10분거리.내.버스정류장.수 <- NULL
# # 1-2. 결측치 row 제거
# tr_uniq_df %<>% drop_na()
# 
# 
# ### (방법 2) 결측치를 단일값으로 처리 ###
# # 결측치 0으로 대체
# tr_uniq_df$도보.10분거리.내.버스정류장.수[is.na(tr_uniq_df$도보.10분거리.내.버스정류장.수)] <- 0
# 
# # 결측치를 column의 중간값(최빈값, 평균값, ...)으로 대체
# median_bus <- median(tr_uniq_df$도보.10분거리.내.버스정류장.수, na.rm = TRUE)
# tr_uniq_df$도보.10분거리.내.버스정류장.수[is.na(tr_uniq_df$도보.10분거리.내.버스정류장.수)] <- median_bus
# 
# 
# ### (방법 3) 결측치 값을 다른 x변수로 예측 ###
# # (주의) KNN과 같은 거리 기반 방법 사용시 차원의 저주(Curse of Dimensionality)를 조심해야 함
# # 3-1. `DMwR2::knnImputation` : KNN
# tr_uniq_df %>% 
#   select(-where(is.character)) %>% 
#   DMwR2::knnImputation(k = 10, scale = TRUE, meth = 'weighAvg')
# # 3-2. `mice::mice` : MCMC(Markov Chain Monte Carlo)
# # (참고) 알고리즘 설명 : https://stefvanbuuren.name/fimd/sec-FCS.html#def:mice
# # m = 5 : 5개의 imputed data 생성
# tr_uniq_df %>% 
#   select(-where(is.character)) %>% 
#   mice::mice(m = 5, seed = 1)



print("=========================== 210731 ===========================")
print("=========================== 210731 ===========================")
print("=========================== 210731 ===========================")



# 분석용 data.frame 정하기  -----------------------------------------------
# (주의) 데이터 split
# 현재 데이터 : train.csv, test.csv
# test.csv는 Y정보(등록차량수) 없음
# 분석을 위해서는 train.csv 자체를 나눠야함 (train, validation, test)
# 전체 데이터 : train.csv로 고정
# 전체 데이터 내에서 training, test set 


### 상가/아파트별로 전용면적 변수 구분 ###
area_freq_list <- tr_df %>%
  # 1의 자리에서 반올림
  mutate(전용면적 = round(전용면적, -1)) %>%
  # 상한 100으로 제한
  mutate(전용면적 = pmin(전용면적, 100)) %>%
  # 하한 10으로 제한
  mutate(전용면적 = pmax(전용면적, 10)) %>%
  split(.$임대건물구분) %>% 
  map(~ {
    x <- .
    
    x %>% 
      group_by(단지코드, 전용면적) %>% 
      summarise(전용면적별세대수 = sum(전용면적별세대수), .groups = 'drop') %>% 
      # pivoting
      pivot_wider(
        
        id_cols = 단지코드,
        
        names_from = 전용면적,
        # zero padding
        names_glue = "전용면적_{str_pad(전용면적, 3, 'left', '0')}",
        
        values_from = 전용면적별세대수,
        values_fill = 0
      ) %>% 
      # column명 정렬
      select(all_of(sort(names(.))))
  })

area_freq_list

### 방법 1 : row(observation) 단위 : 단지코드 ###
# 상가 / 아파트 구분
# (주의) 하나의 단지코드에 상가, 아파트 동시에 있는 경우 존재

# 상가, 아파트 동시 사용 column
cols_method1 <- c(
  
  # Y변수 
  "등록차량수",
  
  # 결측치 X : 5개
  "단지코드", "총세대수", "지역", "공가수", "등록차량수" ,
  # 결측치 O : 2개
  "지하철역수",
  "버스정류장수"
)

cols_method1

# 상가
method1_shop_df <- tr_df %>% 
  filter(임대건물구분 == "상가") %>%
  select(all_of(cols_method1)) %>% 
  mutate(
    지하철역수   = ifelse(is.na(지하철역수),   0, 지하철역수),
    버스정류장수 = ifelse(is.na(버스정류장수), 0, 버스정류장수)
  ) %>% 
  distinct(across(everything())) %>% 
  inner_join(area_freq_list$상가, by = "단지코드") %>% 
  select(단지코드, 등록차량수, everything())

method1_shop_df

# 아파트
method1_apt_df <- tr_df %>% 
  filter(임대건물구분 == "아파트") %>%
  select(all_of(cols_method1), 임대보증금, 임대료, 전용면적별세대수) %>% 
  mutate(
    지하철역수   = ifelse(is.na(지하철역수),   0, 지하철역수),
    버스정류장수 = ifelse(is.na(버스정류장수), 0, 버스정류장수)
  ) %>%  
  group_split(단지코드) %>% 
  map_df(~ {
    x <- .
    y <- x %>% drop_na(임대보증금, 임대료)
    
    mean_deposit <- sum(y$임대보증금 * y$전용면적별세대수) / sum(y$전용면적별세대수)
    mean_rent    <- sum(y$임대료     * y$전용면적별세대수) / sum(y$전용면적별세대수)
    
    y$임대보증금 <- mean_deposit
    y$임대료     <- mean_rent
    
    if (nrow(y) > 0) {
      
      return(y)
    } else {
      
      x %>% mutate(임대보증금 = NA, 임대료 = NA) %>% return()
    }
  }) %>% 
  mutate(
    임대보증금   = ifelse(is.na(임대보증금), median(임대보증금, na.rm = TRUE), 임대보증금),
    임대료       = ifelse(is.na(임대료),     median(임대료    , na.rm = TRUE),     임대료)
  ) %>% 
  select(-전용면적별세대수) %>% 
  distinct(across(everything())) %>% 
  inner_join(area_freq_list$아파트, by = "단지코드") %>% 
  select(단지코드, 등록차량수, everything()) %>% 
  as.data.frame()

method1_apt_df 

# # 단지코드 중복 확인
# nrow(method1_shop_df) == length(unique(method1_shop_df$단지코드))
# nrow(method1_apt_df ) == length(unique(method1_apt_df $단지코드))


# 지역별 비율 고려 training, test index
# table(method1_shop_df$지역)
# table(method1_apt_df $지역)

set.seed(1)
code_shop_tr <- method1_shop_df %>% 
  select(단지코드, 지역) %>% 
  group_by(지역) %>%
  slice_sample(prop = 0.8) %>%
  pull(단지코드)
code_shop_te <- setdiff(method1_shop_df$단지코드, code_shop_tr)
code_shop_tr
code_shop_te
# code_shop_tr %in% method1_apt_df$단지코드
# code_shop_te %in% method1_apt_df$단지코드


set.seed(2)
code_apt_tr <- method1_apt_df %>% 
  filter(!단지코드 %in% method1_shop_df$단지코드) %>% 
  select(단지코드, 지역) %>% 
  group_by(지역) %>% 
  slice_sample(prop = 0.8) %>%
  pull(단지코드)
code_apt_te <- method1_apt_df %>% 
  filter(!단지코드 %in% method1_shop_df$단지코드) %$% 
  setdiff(단지코드, code_apt_tr)

code_apt_tr <- c(code_apt_tr, code_shop_tr)
code_apt_te <- c(code_apt_te, code_shop_te)


method1_shop_df %<>% 
  mutate(tr_te = ifelse(단지코드 %in% code_shop_tr, "training", "test")) %>% 
  select(tr_te, everything())

method1_apt_df  %<>% 
  mutate(tr_te = ifelse(단지코드 %in% code_apt_tr,  "training", "test")) %>% 
  select(tr_te, everything())


# ### 결과 저장 ###
# # csv 파일 저장
# write.csv(
#   x = method1_shop_df,
#   file = "method1_shop_df.csv",
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )
# write.csv(
#   x = method1_apt_df,
#   file = "method1_apt_df.csv",
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )
# 
# saveRDS(object = method1_shop_df, file = "method1_shop_df.rds")
# saveRDS(object = method1_apt_df,  file = "method1_apt_df.rds")
# 
# 
# # (참고) CSV 파일 읽어오기
# tmp_1_shop_df <- read.csv("method1_shop_df.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# tmp_1_apt_df  <- read.csv("method1_apt_df.csv",  header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# # (참고) RDS 파일 읽어오기
# tmp_1_shop_df <- readRDS("method1_shop_df.rds")
# tmp_1_apt_df  <- readRDS("method1_apt_df.rds")



# modeling : 7/31(토) 진행 내용 -------------------------------------------

# 변수명 저장
name_y  <- "등록차량수"
names_x <- setdiff(names(method1_apt_df), c("tr_te", "단지코드", name_y))

# formula 저장
formula_y_x <- formula(paste0(name_y, " ~ ", paste0(names_x, collapse = " + ")))

# training, test set 분리 
apt_tr_df <- method1_apt_df %>% filter(tr_te == "training")
apt_te_df <- method1_apt_df %>% filter(tr_te == "test")


# intersect(apt_tr_df$단지코드, apt_te_df$단지코드)


### 선형 회귀 ###
# 일반적인 선형 회귀
# 오차의 제곱합을 최소로 하는 beta(회귀계수, 가중치)
fit_lm <- lm(formula = formula_y_x, data = apt_tr_df)
summary(fit_lm)

# 선형 회귀분석에서만 가능
par(mfrow = c(2, 2))
plot(fit_lm)
par(mfrow = c(1, 1))


### Elastic Net ###
# 벌점화 회귀(Peneralized Regression) : 회귀계수(가중치)에 penalty를 부여
# training set에 모형이 과적합(over-fitting)되는 현상을 방지

# training set이 데이터의 모든 pattern을 그대로 설명하면 best
# but noise가 존재

# 목표 : training set'만을' 잘 설명하는 게 아니라
# (X, Y) 사이의 일반적인 pattern을 설명하는 모형!

# `glmnet::cv.glmnet`
# cross-validation을 통해 lambda 결정
# alpha = 0 : Ridge, alpha = 1 : LASSO (0 <= alpha <= 1)

# LASSO
# factor 변수들 encoding(dummy variable)
apt_tr_x_mat <- 
  cbind(
    
    apt_tr_df %>% 
      select(all_of(names_x)) %>% 
      select(-지역) %>% 
      as.matrix(),
    
    model.matrix(
      object = 단지코드 ~ 지역,
      data = apt_tr_df
    )[, -1]
  )
apt_te_x_mat <- 
  cbind(
    
    apt_te_df %>% 
      select(all_of(names_x)) %>% 
      select(-지역) %>% 
      as.matrix(),
    
    model.matrix(
      object = 단지코드 ~ 지역,
      data = apt_te_df
    )[, -1]
  )


set.seed(3)
fit_lasso <- cv.glmnet(
  x = apt_tr_x_mat,
  y = apt_tr_df$등록차량수,
  # y = apt_tr_df[[name_y]],
  alpha = 1,
  nfolds = 10
)
coef(fit_lasso)


### Random Forest ###
set.seed(4)
fit_rf <- randomForest(
  formula = formula_y_x,
  data = apt_tr_df
)
fit_rf$importance
# plot(fit_rf)
# plot(fit_rf$importance)
varImpPlot(fit_rf)


### 예측값 ###
# training
yreal_tr <- apt_tr_df[[name_y]]
ypred_tr_lm    <- predict(fit_lm,    apt_tr_df)
ypred_tr_lasso <- predict(fit_lasso, apt_tr_x_mat) %>% as.double()
ypred_tr_rf    <- predict(fit_rf,    apt_tr_df)
# test
yreal_te <- apt_te_df[[name_y]]
ypred_te_lm    <- predict(fit_lm,    apt_te_df)
ypred_te_lasso <- predict(fit_lasso, apt_te_x_mat) %>% as.double()
ypred_te_rf    <- predict(fit_rf,    apt_te_df)


### 예측오차 ###
# (주의) 사용 함수에 따라 예측값, 실제값 순서 주의!

# 잔차 = 실제값 - 예측값

### training set ###
# MSE : 평균 제곱 오차
# RMSE (Root Mean Squared Error; 평균 제곱근 오차)
MLmetrics::RMSE(ypred_tr_lm,    yreal_tr)
MLmetrics::RMSE(ypred_tr_lasso, yreal_tr)
MLmetrics::RMSE(ypred_tr_rf,    yreal_tr)
# MAPE (Mean Absolute Percentage Error; 평균 절대 비 오차)
MLmetrics::MAPE(ypred_tr_lm,    yreal_tr)
MLmetrics::MAPE(ypred_tr_lasso, yreal_tr)
MLmetrics::MAPE(ypred_tr_rf,    yreal_tr)
# R-squared
MLmetrics::R2_Score(ypred_tr_lm,    yreal_tr)
MLmetrics::R2_Score(ypred_tr_lasso, yreal_tr)
MLmetrics::R2_Score(ypred_tr_rf,    yreal_tr)

### test set ###
# RMSE (Root Mean Squared Error; 평균 제곱근 오차)
MLmetrics::RMSE(ypred_te_lm,    yreal_te)
MLmetrics::RMSE(ypred_te_lasso, yreal_te)
MLmetrics::RMSE(ypred_te_rf,    yreal_te)
# MAPE (Mean Absolute Percentage Error; 평균 절대 비 오차)
MLmetrics::MAPE(ypred_te_lm,    yreal_te)
MLmetrics::MAPE(ypred_te_lasso, yreal_te)
MLmetrics::MAPE(ypred_te_rf,    yreal_te)
# R-squared
MLmetrics::R2_Score(ypred_te_lm,    yreal_te)
MLmetrics::R2_Score(ypred_te_lasso, yreal_te)
MLmetrics::R2_Score(ypred_te_rf,    yreal_te)


### 실제값 vs 예측값 그림 ###
# (주의) 현재 test set의 y정보 없음
#        test set 제출 전에 결과 확인하고 싶으면 validation set 별도로 필요!!!

# training set 실제값 vs 예측값 그림
# x축, y축 범위
lims_tr <- range(c(yreal_tr, ypred_tr_lm, ypred_tr_lasso, ypred_tr_rf))
# lm
par(pty = 's')
plot(
  ypred_tr_lm, yreal_tr, xlim = lims_tr, ylim = lims_tr,
  xlab = "선형 회귀 예측값", ylab = "실제값", main = "training lm 실제값 vs 예측값"
)
abline(a = 0, b = 1, col = "red")
# LASSO
par(pty = 's')
plot(ypred_tr_lasso, yreal_tr, xlim = lims_tr, ylim = lims_tr)
abline(a = 0, b = 1, col = "red")
# Random Forest
par(pty = 's')
plot(ypred_tr_rf, yreal_tr ,xlim = lims_tr, ylim = lims_tr)
abline(a = 0, b = 1, col = "red")



# test set 실제값 vs 예측값 그림
# x축, y축 범위
lims_te <- range(c(yreal_te, ypred_te_lm, ypred_te_lasso, ypred_te_rf))
# lm
par(pty = 's')
plot(
  ypred_te_lm, yreal_te, xlim = lims_te, ylim = lims_te,
  xlab = "선형 회귀 예측값", ylab = "실제값", main = "training lm 실제값 vs 예측값"
)
abline(a = 0, b = 1, col = "red")
# LASSO
par(pty = 's')
plot(ypred_te_lasso, yreal_te, xlim = lims_te, ylim = lims_te)
abline(a = 0, b = 1, col = "red")
# Random Forest
par(pty = 's')
plot(ypred_te_rf, yreal_te ,xlim = lims_te, ylim = lims_te)
abline(a = 0, b = 1, col = "red")


data.frame(
  단지코드 = apt_te_df$단지코드,
  잔차_rf = yreal_te - ypred_te_rf
)



# End of File -------------------------------------------------------------

# EOF
