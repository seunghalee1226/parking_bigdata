### 데이터 확인 및 현황 파악 ###

# 최초 작성 : 2021-07-17(토)
# 추가 작성 : 2021-07-24(토)
#      수정 : 2021-07-26(월)

# Encoding : UTF-8
# (Rstudio "File" - "Reopen with Encoding" - "UTF-8")



# settings ----------------------------------------------------------------


### options ###
options(scipen = 20)

getwd()
setwd("C:\\parking_bigdata")
### Working Directory 설정 ###
# 데이터(csv 파일) 있는 폴더로 각자 설정
# setwd("C:\\Users\\memoon\\Desktop\\빅데이터아카데미\\멘토링 예시코드 - 3조 문믿음")


### install, load and attach packages ###
# user-defined function

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
  
  pkgs_attach = c("magrittr", "tidyverse", "glmnet", "randomForest", "dlookr", 
                  "ggplot2", "broom", "dplyr", "MASS", "rms", "xgboost",
                  "recipes", "ipred", "gbm"),
  pkgs_load = c("readr", "pillar", "psych", "Hmisc", "skimr", "ggplot2",
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
  dplyr::select(-단지코드) %>% 
  # column별로 합 계산
  apply(2, sum) 

num_uniq_int

# 단지 코드 unique한 개수(423) 이하인 column은 단지 코드별로 값이 유일한 column
cols_uniq <- names(num_uniq_int)[num_uniq_int <= length(unique(tr_df$단지코드))]
cols_uniq <- c("단지코드", cols_uniq)

# unique한 column만 따로 저장
tr_uniq_df <- tr_df %>% 
  dplyr::select(all_of(cols_uniq)) %>% 
  distinct(across(everything()))

tr_uniq_df

# test set에서도 확인
# unique한 단지 코드 수
length(unique(te_df$단지코드))
# `te_df`에 있는 `cols_uniq` column이 unique한지 확인
te_df %>% 
  dplyr::select(any_of(cols_uniq)) %>% 
  distinct(across(everything())) %>% 
  nrow()
# unique한 column만 따로 저장
te_uniq_df <- te_df %>% 
  dplyr::select(any_of(cols_uniq)) %>% 
  distinct(across(everything()))

# unique하지 않은 column들은 어떤 게 있을까?
tr_df %>% 
  dplyr::select(-all_of(cols_uniq)) %>% 
  head()

# (주의) 공가수는 (아마도) 전용면적 상관없이 단지 전체 중복 들어간 듯
#        판단 근거 : 단지코드별로 공가수 값이 unique함
"공가수" %in% cols_uniq


### character column을 factor로 변경 (modeling 활용 위해서) ###
# character column 저장
cols_chr <- c("임대건물구분", "지역", "공급유형", "자격유형")

# 분포 확인 : column별 빈도 수
tr_df %>%
  dplyr::select(all_of(cols_chr)) %>% 
  map(table)

# 분포 확인 : 조합별 빈도 수
tr_df %>% 
  dplyr::select(all_of(cols_chr)) %>% 
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
  dplyr::select(all_of(sort(names(.))))

# 총면적 변수 추가
tr_uniq_df %<>% inner_join(tr_area_freq_df, by = "단지코드")


tr_uniq_df


# Plotting ----------------------------------------------------------------

### 변수 목록 저장 ###
# (주의) 예시에서는 unique한 변수만 사용 
#        unique하지 않은 변수들 보려면,
#        변수별로 중복값 제거해서 단지코드별로 보는 방법 존재

cols_num <- tr_uniq_df %>% 
  dplyr::select(where(is.numeric)) %>%
  names() %>% 
  # Y변수는 제외
  setdiff("등록차량수")

cols_num

cols_fct <- tr_uniq_df %>% 
  dplyr::select(where(is.factor)) %>%
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
  dplyr::select(등록차량수, all_of(cols_num)) %>% 
  # (X, Y) 둘 다 문제 없는 pair들만 이용해 상관계수 계산
  cor(use = 'pairwise.complete.obs') %>% 
  corrplot::corrplot.mixed(tl.pos = 'd')


### EDA
# 데이터 위생 정검
# 범주형 변수별 수준 갯수
tr_uniq_df %>% select_if(is.factor) %>%
  summarise_all(nlevels) %>% gather(variable, cnt)

# 범주형 변수별 요약 통계량
tr_uniq_df %>%
  select_if(is.factor) %>% skim()

# 범주형 변수
cat_var  <- tr_uniq_df %>%
  select_if(is.factor) %>%
  colnames()

## 연속형 변수
tr_uniq_df %>%
  dplyr::select(cont_var , 등록차량수) %>%
  dplyr::group_by(등록차량수) %>%
  skim()


tr_uniq_df_temp <- tr_uniq_df


# 새로운 변수 생성? 세대당 주차면수 = 단지내주차면수 / 총세대수
tr_uniq_df_temp$세대당주차면수 <- tr_uniq_df_temp$단지내주차면수/tr_uniq_df_temp$총세대수 %>% round(2)

# 실거주세대당 등록차량수  = 등록차량수 / (총세대 - 공가수)
tr_uniq_df_temp$실거주당주차등록 <- tr_uniq_df_temp$등록차량수/ (tr_uniq_df_temp$총세대수 - tr_uniq_df_temp$공가수)


# 버스 구간 정하기
# 1이하 : 0, 2~3 : 1, 4~5 : 2, 6~9 : 3, 10~ : 4
hist(tr_uniq_df_temp$버스정류장수)
tr_uniq_df_temp %>% group_by(버스정류장수) %>% summarise(count=n())
tr_uniq_df_temp$버스구간 = ifelse(tr_uniq_df_temp$버스정류장수 < 2, 0,
                              ifelse(tr_uniq_df_temp$버스정류장수 >= 2 & tr_uniq_df_temp$버스정류장수 < 4, 1,
                                     ifelse(tr_uniq_df_temp$버스정류장수 >= 4 & tr_uniq_df_temp$버스정류장수 < 6, 2,
                                            ifelse(tr_uniq_df_temp$버스정류장수 >= 6 & tr_uniq_df_temp$버스정류장수 < 9, 3, 4))))


# 대중교통수로 합치기 (버스 + 지하철)
tr_uniq_df_temp$대중교통수 <- tr_uniq_df_temp$지하철역수 + tr_uniq_df_temp$버스정류장수
hist(tr_uniq_df_temp$대중교통수)
tr_uniq_df_temp %>% group_by(대중교통수) %>% summarise(count=n())

tr_uniq_df_temp$대중교통구간 = ifelse(tr_uniq_df_temp$대중교통수 < 2, 0,
                              ifelse(tr_uniq_df_temp$대중교통수 >= 2 & tr_uniq_df_temp$대중교통수 < 4, 1,
                                     ifelse(tr_uniq_df_temp$대중교통수 >= 4 & tr_uniq_df_temp$대중교통수 < 6, 2,
                                            ifelse(tr_uniq_df_temp$대중교통수 >= 6 & tr_uniq_df_temp$대중교통수 < 9, 3, 4))))



# 수도권만 따로 확인
# 수도권 분류
tr_uniq_df_metr <- rbind(subset(tr_uniq_df_temp, 지역=="서울특별시"), subset(tr_uniq_df_temp, 지역=="경기도"))

# 지하철이 있는 광역시만 따로 확인
# 부산(4), 대구(3), 대전(1), 광주(1)
tr_uniq_df_mega <- rbind(subset(tr_uniq_df_temp, 지역=="부산광역시"), subset(tr_uniq_df_temp, 지역=="대구광역시"),
                         subset(tr_uniq_df_temp, 지역=="대전광역시"), subset(tr_uniq_df_temp, 지역=="광주광역시"))

# 그 외 도시 분류
tr_uniq_df_city <- rbind(subset(tr_uniq_df_temp, 지역=="강원도"), subset(tr_uniq_df_temp, 지역=="경상남도"),
                         subset(tr_uniq_df_temp, 지역=="경상북도"), subset(tr_uniq_df_temp, 지역=="세종특별자치시"),
                         subset(tr_uniq_df_temp, 지역=="울산광역시"), subset(tr_uniq_df_temp, 지역=="전라남도"),
                         subset(tr_uniq_df_temp, 지역=="전라북도"), subset(tr_uniq_df_temp, 지역=="제주특별자치도"),
                         subset(tr_uniq_df_temp, 지역=="충청남도"), subset(tr_uniq_df_temp, 지역=="충청북도"))


### 가설 1 : 주변에 지하철이 있으면 주차 등록에 영향이 없을 것이다
### 가설 2 : 주변에 버스가 있으면 주차등록은 감소할 것이다
### 가설 3 : 대중교통은 주차등록과 연관이 있다


# 지하철
# 수도권의 경우 주변에 지하철이 있으면 실거주당 주차등록대수가 1.2이하
ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(지하철역수), shape=factor(지하철역수)), data=tr_uniq_df_metr) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()

# 지하철이 있는 광역시의 경우 관계가 없음
ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(지하철역수), shape=factor(지하철역수)), data=tr_uniq_df_mega) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()


# 버스
# 수도권의 경우 실구주당 주차등록수가 1.5정도? 버스와 관계성을 딱히 관계를 발견하지 못하겠음
ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(버스구간), shape=factor(버스구간)), data=tr_uniq_df_metr) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()

ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(버스구간), shape=factor(버스구간)), data=tr_uniq_df_mega) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()


ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(버스구간), shape=factor(버스구간)), data=tr_uniq_df_city) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()


# 대중교통
# 전혀... 해석 추가 필요할 듯...
ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(대중교통구간), shape=factor(대중교통구간)), data=tr_uniq_df_metr) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()

ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(대중교통구간), shape=factor(대중교통구간)), data=tr_uniq_df_mega) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()


ggplot() +
  geom_point(mapping=aes(x=세대당주차면수, y=실거주당주차등록, color=factor(대중교통구간), shape=factor(대중교통구간)), data=tr_uniq_df_city) +
  geom_smooth(method='lm', formula = y ~ x) +
  theme_bw()



## 예측 모형 사전 검토 
# 전용면적들에 대한 다중 공산성이 높음?
tr_uniq_df_lm <- lm(등록차량수 ~., data = tr_uniq_df[c(cols_num, "등록차량수")])
tr_uniq_df_vif <- rms::vif(tr_uniq_df_lm)
tr_uniq_df_vif %>% as.data.frame() %>%
  rownames_to_column(var="변수명") %>%
  rename(VIF = ".") %>%
  arrange(-VIF)


### PCA 진행 
# 일반적으로는 전체 변화량의 90% 가량을 설명하는 주성분을 선택,
# 여기서는 분산의 누적합계를 볼 때 PC9까지 선택을 볼 수 있음
data_pca_results <- prcomp(tr_uniq_df[cols_num], scale = TRUE)
summary(data_pca_results)
# screeplot으로 확인을 하나, 그래프가 완만해지는 부분이 없음??
# 각 개체에 대해 첫번째, 두번째 주성분 점수 및 행렬도(biplot) 그리기
# 가까운 거리와 방향일수록 변수들의 상관성이 높아짐
#
factoextra::fviz_eig(data_pca_results)
# screeplot(data_pca_results, main="", col="blue", type="lines", pch=1, npcs=length(data_pca_results$sdev))
biplot(data_pca_results)

pca_dim_12 <- factoextra::fviz_pca_var(data_pca_results, axes = c(1, 2),
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE     # Avoid text overlapping
)

pca_dim_13 <- factoextra::fviz_pca_var(data_pca_results, axes = c(1, 3), 
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE     # Avoid text overlapping
)

pca_dim_23 <- factoextra::fviz_pca_var(data_pca_results, axes = c(2, 3), 
                              col.var = "contrib", # Color by contributions to the PC
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE     # Avoid text overlapping
)

gridExtra::grid.arrange(pca_dim_12, pca_dim_13, pca_dim_23, nrow=1 )
data_pca_results$rotation[, 1:5] %>% round(2)
data_pca_results

pca_results <- data_pca_results$x[, 1:9] %>% as_tibble()
pca_results



################## 회귀 분석 비교
### 주성분 회귀 모형
pca_df <- tr_uniq_df %>% dplyr::select(등록차량수) %>% 
  bind_cols(pca_results)

pca_lm <- lm(등록차량수 ~., data=pca_df)
summary(pca_lm)

pca_broom <- pca_lm %>% broom::glance() %>% gather(통계량, 통계수치) %>% 
  mutate(모형 = "주성분 회귀") %>%
  dplyr::select(모형, everything())
pca_broom


tr_uniq_df_1 <- tr_uniq_df
tr_uniq_df_1$pre_pca <- predict(pca_lm)

# 잔차분석
# p-value : 0
durbinWatsonTest(pca_lm)

# Q-Q plot
e_pca <- tr_uniq_df_1$pre_pca - tr_uniq_df_1$등록차량수
qqnorm(e_pca); qqline(e_pca, col = "red")

# 정규성
# X-squared = 645.67, df = 2, p-value < 0.00000000000000022
jarque.bera.test(resid(pca_lm))

# 등분산성 산차 산점도
tr_uniq_df_1$e_pca            <- e_pca
ggplot(tr_uniq_df_1, aes(pre_pca, e_pca)) + 
  geom_point(color = "gray20", size = 2) +
  geom_smooth(method = "loess") + 
  labs(x = "Predicted", y = "잔차") +
  geom_hline(yintercept = 0, color = "red") + theme_classic() +
  ggtitle("주성분 회귀 잔차산점도")

# 등분산성 Breusch-Pagan Test
# BP = 57.812, df = 9, p-value = 0.000000003532
bptest(pca_lm)

# 모형선택 기준: AIC 5652.898
AIC(pca_lm)
# 후진 소거법 : BIC 5697.419
BIC(pca_lm)


### 다중 선형회귀 모형
lm_broom <- tr_uniq_df_lm %>% broom::glance() %>%
  gather(통계량, 통계수치) %>%
  mutate(모형 = "다중회귀") %>%
  dplyr::select(모형, everything())

lm_broom
summary(lm_broom)

### 변수선택 다중 선형회귀 모델
aic_lm <- stepAIC(tr_uniq_df_lm, trace=0)
aic_lm
summary(aic_lm)


tr_uniq_df_2 <- tr_uniq_df
tr_uniq_df_2$pre_aic <- predict(aic_lm)

# 잔차분석
# p-value : 0.038
durbinWatsonTest(aic_lm)

# Q-Q plot
e_aic <- tr_uniq_df_2$pre_aic - tr_uniq_df_2$등록차량수
qqnorm(e_aic); qqline(e_aic, col = "red")

# 정규성
# XX-squared = 272.06, df = 2, p-value < 0.00000000000000022
jarque.bera.test(resid(aic_lm))

# 등분산성 산차 산점도
tr_uniq_df_2$e_aic <- e_aic
ggplot(tr_uniq_df_2, aes(pre_aic, e_aic)) + 
  geom_point(color = "gray20", size = 2) +
  geom_smooth(method = "loess") + 
  labs(x = "Predicted", y = "잔차") +
  geom_hline(yintercept = 0, color = "red") + theme_classic() +
  ggtitle("변수선택 회귀 잔차산점도")

# 등분산성 Breusch-Pagan Test
# BP = 78.04, df = 12, p-value = 0.000000000009748
bptest(aic_lm)

# 모형선택 기준: AIC 5545.411
AIC(aic_lm)
# 후진 소거법 : BIC 5602.075
BIC(aic_lm)


### 다중회귀 모델
parsi_fm <- as.formula(summary(aic_lm)$call)
parsi_lm <- lm(parsi_fm, data=tr_uniq_df)
summary(parsi_lm)

parsi_broom <- parsi_lm %>%  broom::glance() %>%
  gather(통계량, 통계수치) %>%
  mutate(모형 = "변수선택 회귀") %>%
  dplyr::select(모형, everything())


tr_uniq_df_3 <- tr_uniq_df
tr_uniq_df_3$pre_parsi <- predict(parsi_lm)

# 잔차분석
# p-value : 0.03
durbinWatsonTest(parsi_lm)

# Q-Q plot
e_parsi <- tr_uniq_df_3$pre_parsi - tr_uniq_df_3$등록차량수
qqnorm(e_parsi); qqline(e_parsi, col = "red")

# 정규성
# X-squared = 272.06, df = 2, p-value < 0.00000000000000022
jarque.bera.test(resid(parsi_lm))

# 등분산성 산차 산점도
tr_uniq_df_3$e_parsi <- e_parsi
ggplot(tr_uniq_df_3, aes(pre_parsi, e_parsi)) + 
  geom_point(color = "gray20", size = 2) +
  geom_smooth(method = "loess") + 
  labs(x = "Predicted", y = "잔차") +
  geom_hline(yintercept = 0, color = "red") + theme_classic() +
  ggtitle("변수선택 회귀 잔차산점도")

# 등분산성 Breusch-Pagan Test
# BP = 78.04, df = 12, p-value = 0.000000000009748
bptest(parsi_lm)

# 모형선택 기준: AIC 5545.411
AIC(parsi_lm)
# 후진 소거법 : BIC 5602.075
BIC(parsi_lm)


### 회귀 모형 비교
parking_model <- bind_rows(pca_broom, lm_broom) %>% bind_rows(parsi_broom)

parking_model %>%
  filter(통계량 == "adj.r.squared") %>%
  ggplot(aes(x=모형, y=통계수치)) +
  geom_bar(stat="identity", width=0.3, fill="blue") +
  coord_flip() +
  labs(y="조정결정계수(Adjusted R Squared)", x="모형") +
  theme_minimal(base_family = "NanumGothic") +
  geom_text(aes(label=round(통계수치, 2)), position=position_dodge(width=1), vjust=-0.0, hjust=-0.1)








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
      dplyr::select(all_of(sort(names(.))))
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
  dplyr::select(all_of(cols_method1)) %>% 
  mutate(
    지하철역수   = ifelse(is.na(지하철역수),   0, 지하철역수),
    버스정류장수 = ifelse(is.na(버스정류장수), 0, 버스정류장수)
  ) %>% 
  distinct(across(everything())) %>% 
  inner_join(area_freq_list$상가, by = "단지코드") %>% 
  dplyr::select(단지코드, 등록차량수, everything())

method1_shop_df

# 아파트
method1_apt_df <- tr_df %>% 
  filter(임대건물구분 == "아파트") %>%
  dplyr::select(all_of(cols_method1), 임대보증금, 임대료, 전용면적별세대수) %>% 
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
  dplyr::select(-전용면적별세대수) %>% 
  distinct(across(everything())) %>% 
  inner_join(area_freq_list$아파트, by = "단지코드") %>% 
  dplyr::select(단지코드, 등록차량수, everything()) %>% 
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
  dplyr::select(단지코드, 지역) %>% 
  group_by(지역) %>%
  slice_sample(prop = 0.8) %>%
  pull(단지코드)

code_shop_tr
code_shop_te <- setdiff(method1_shop_df$단지코드, code_shop_tr)
code_shop_tr
code_shop_te
# code_shop_tr %in% method1_apt_df$단지코드
# code_shop_te %in% method1_apt_df$단지코드


set.seed(2)
code_apt_tr <- method1_apt_df %>% 
  filter(!단지코드 %in% method1_shop_df$단지코드) %>% 
  dplyr::select(단지코드, 지역) %>% 
  group_by(지역) %>% 
  slice_sample(prop = 0.8) %>%
  pull(단지코드)
code_apt_te <- method1_apt_df %>% 
  filter(!단지코드 %in% method1_shop_df$단지코드) %$% 
  setdiff(단지코드, code_apt_tr)

code_apt_tr <- c(code_apt_tr, code_shop_tr)
code_apt_te <- c(code_apt_te, code_shop_te)

code_apt_tr
code_apt_te

method1_shop_df %<>% 
  mutate(tr_te = ifelse(단지코드 %in% code_shop_tr, "training", "test")) %>% 
  dplyr::select(tr_te, everything())

method1_apt_df  %<>% 
  mutate(tr_te = ifelse(단지코드 %in% code_apt_tr,  "training", "test")) %>% 
  dplyr::select(tr_te, everything())


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



#### 모델링
## 참고 : https://statkclee.github.io/ml/ml-pm-continuous.html
### 데이터 분할 및 결합

shop_df <- method1_shop_df %>% dplyr::select(-c(단지코드))
apt_df <- method1_shop_df %>% dplyr::select(-c(단지코드))


# 
# age_gender <- csv_list$age_gender_info
# names(age_gender)
# 
# shop_df_1 <- merge(x=shod_df, y=age_gender, by="지역")
# apt_df_1 <- merge(x=apt_df, y=age_gender, by="지역")
# 

names(apt_df)
# 정규 분포 적합?

apt_normal_g <- apt_df %>% 
  ggplot(aes(x=등록차량수)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(apt_df$등록차량수), sd = sd(apt_df$등록차량수)), 
                lwd = 2, col = 'red') +
  theme_bw(base_family="NanumGothic") +
  labs(title="아파트 등록차량수 분포", x="등록차량수", y="확률밀도(density)") +
  scale_x_continuous(labels=scales::comma)

apt_log_g <- apt_df %>% 
    ggplot(aes(x=log(등록차량수))) +
    geom_histogram(aes(y = ..density..)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(log(apt_df$등록차량수)), sd = sd(log(apt_df$등록차량수))), 
                  lwd = 2, col = 'red') +
    theme_bw(base_family="NanumGothic") +
    labs(title="아파트 등록차량수 분포(로그)", x="등록차량수", y="") +
    scale_x_continuous(labels=scales::comma)

### caret 예측모형 개발

## 아파트
# train, test 모델 분리
apt_tr_df <- apt_df %>% filter(tr_te == "training") %>% dplyr::select(-c(tr_te))
apt_te_df <- apt_df %>% filter(tr_te == "test") %>% dplyr::select(-c(tr_te))

names(apt_tr_df)

y_var <- "등록차량수"
x_var <- setdiff(names(apt_tr_df), c(y_var))
apt_fmla <- as.formula(paste(y_var, "~", paste(x_var, collapse="+")))
apt_fmla

# 0. 교체 타당도 제어 조건
apt_ml_control <- caret::trainControl(method = "repeatedcv",
                                      number = 5,
                                      repeats = 1,
                                      verboseIter = FALSE) 

### 1. 기본 모형 적합
## 아파트
# rmse = 81.1358
apt_lm <- lm(apt_fmla, data = apt_df)
summary(apt_lm)
apt_df_new <- apt_df
apt_df_new$pred <- predict(apt_lm)

# 잔차분석
# p-value : 0.44
durbinWatsonTest(apt_lm)

# Q-Q plot
e_apt_lm <- apt_df_new$pred - apt_df_new$등록차량수
qqnorm(e_apt_lm); qqline(e_apt_lm, col = "red")

# 정규성
# X-squared = 33.028, df = 2, p-value = 0.00000006732
jarque.bera.test(resid(apt_lm))

# 등분산성 산차 산점도
apt_df_new$e_apt_lm <- e_apt_lm
ggplot(apt_df_new, aes(pred, e_apt_lm)) + 
  geom_point(color = "gray20", size = 2) +
  geom_smooth(method = "loess") + 
  labs(x = "Predicted", y = "잔차") +
  geom_hline(yintercept = 0, color = "red") + theme_classic() +
  ggtitle("변수선택 회귀 잔차산점도")

# 등분산성 Breusch-Pagan Test
# BP = 22.278, df = 18, p-value = 0.2199
bptest(apt_lm)

# 모형선택 기준: AIC 5423.7941
AIC(apt_lm)
# 후진 소거법 : BIC 453.7243
BIC(apt_lm)



## 상가
# rmse = 81.1358
shop_lm <- lm(apt_fmla, data = shop_df)
summary(shop_lm)
shop_df_new <- shop_df
shop_df_new$pred <- predict(shop_lm)

# 잔차분석
# p-value : 0.488
durbinWatsonTest(shop_lm)

# Q-Q plot
e_shop_lm <- shop_df_new$pred - shop_df_new$등록차량수
qqnorm(e_shop_lm); qqline(e_shop_lm, col = "red")

# 정규성
# X-squared = 33.028, df = 2, p-value = 0.00000006732
jarque.bera.test(resid(shop_lm))

# 등분산성 산차 산점도
shop_df_new$e_shop_lm <- e_shop_lm
ggplot(shop_df_new, aes(pred, e_shop_lm)) + 
  geom_point(color = "gray20", size = 2) +
  geom_smooth(method = "loess") + 
  labs(x = "Predicted", y = "잔차") +
  geom_hline(yintercept = 0, color = "red") + theme_classic() +
  ggtitle("변수선택 회귀 잔차산점도")

# 등분산성 Breusch-Pagan Test
# BP = 22.278, df = 18, p-value = 0.2199
bptest(shop_lm)

# 모형선택 기준: AIC 423.7941
AIC(shop_lm)
# 후진 소거법 : BIC 453.7243
BIC(shop_lm)






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
      dplyr::select(all_of(names_x)) %>% 
      dplyr::select(-지역) %>% 
      as.matrix(),
    
    model.matrix(
      object = 단지코드 ~ 지역,
      data = apt_tr_df
    )[, -1]
  )
apt_te_x_mat <- 
  cbind(
    
    apt_te_df %>% 
      dplyr::select(all_of(names_x)) %>% 
      dplyr::select(-지역) %>% 
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

