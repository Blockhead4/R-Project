library(rJava)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#install.packages("stringi")
#install.packages("devtools")
#devtools::install_github("cardiomoon/kormaps2014")
#devtools::install_github("cardiomoon/Kormaps")
#library(kormaps2014)
library(gridExtra)
library(tibble)
library(maps)
library(ggiraphExtra)
require(Kormaps)
require(tmap)
setwd("D:/workspace-Jwp/R/R-Project/통계 빅데이터 공모전_2019")
options(digits=2)

# 18년도 자치구별 0~2세 인구 분포
data <- read.csv("data/서울시_주민등록인구_2016.txt", header=T, sep="\t")
#View(data18)

sdata <- data %>%
  filter(행정동 != c('합계', '소계'))

sdata2 <- sdata[, c(1:8)]

head(sdata2)
sdata2$합계 <- as.numeric(sdata2$합계)
sdata2$X0세 <- as.numeric(sdata2$X0세)
sdata2$X1세 <- as.numeric(sdata2$X1세)
sdata2$X2세 <- as.numeric(sdata2$X2세)
sdata2$X3세 <- as.numeric(sdata2$X3세)
str(sdata2)

gu <- sdata2 %>%
  group_by(자치구) %>%
  summarise_each(funs(mean), X0세, X1세, X2세, X3세) %>%
  arrange(desc(X0세, X1세, X2세, X3세))
gu

gu_all <- gu %>%
  mutate("영세아" = X0세+X1세+X2세+X3세) %>%
  arrange(desc(영세아))
gu_all

#head(sdata2)
head(sdata3)


#ggplot(sdata2, aes(자치구, X0세, fill=자치구)) +  geom_col()
#ggplot(sdata3, aes(자치구, 영세아, fill=자치구)) +  geom_col()


# 동별 분포
dong <- sdata2 %>%
  group_by(자치구, 행정동) %>%
  summarise_each(funs(mean), X0세, X1세, X2세, X3세) %>%
  arrange(자치구, 행정동, desc(X0세, X1세, X2세, X3세))
dong

dong_all <- dong %>%
  mutate("영세아" = X0세+X1세+X2세+X3세) %>%
  arrange(desc(영세아))
dong_all

ggplot(dong, aes(자치구, X0세, fill=자치구)) +  geom_col()
ggplot(dong_all, aes(자치구, 영세아, fill=자치구)) +  geom_col()
