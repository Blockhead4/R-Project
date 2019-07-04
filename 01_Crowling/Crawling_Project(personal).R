# NAVER 영화 ('스파이더맨 : 파프롬 홈') 일반인 리뷰 크롤링
library(rvest)
library(stringr)
library(dplyr)
library(xlsx)
library(rJava)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
useSejongDic()
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

ifr_base_url <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='
pages <- c(1:336)

points <- c()
for(i in 1:length(pages)) {
  url <- paste0(ifr_base_url, pages[i])
  html <- read_html(url)
  
  html %>%
    html_node('div.score_result') %>%
    html_nodes('li') -> lis
  
  score <- c()
  writer <- c()
  review <- c()
  time <- c()
  for (li in lis) {
    score <- c(score, html_node(li, '.star_score') %>% html_text('em') %>% trim())
    li %>%
      html_node('.score_reple') %>%
      html_text('p') %>%
      trim() -> tmp
    idx <- str_locate(tmp, "\r")
    review <- c(review, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    writer <- c(writer, str_sub(tmp, 1, idx[1]-1))
    tmp <- trim(str_sub(tmp, idx[1], -1))
    idx <- str_locate(tmp, "\r")
    time <- c(time, str_sub(tmp, 1, idx[1]-1))
    #print(time)
  }
  
  result <- data.frame(score=score, review=review, writer=writer, time=time)
  points <- rbind.data.frame(points, result)
}
points

write.xlsx(points, "D:/workspace-Jwp/R/R-Project/01_Crowling/data/points.xlsx")


# WordCloud 
setwd("D:/workspace-Jwp/R/R-Project/01_Crowling")
head(points$review)
reply <- points$review

write.csv(reply, "data/reply.csv", row.names = F, col.names = F)
words <- readLines("data/reply.csv")

words <- sapply(words, extractNoun, USE.NAMES = F)
cdata <- unlist(words)

words <- str_replace_all(cdata, "[^[:alpha:]]", "")
words <- gsub(" ", "", words)
words <- gsub("\\d+", "", words)
words <- Filter(function(x) {nchar(x) >= 2}, words)
head(words2, 300)  

write(unlist(words2), "data/reply.txt")
rev <- read.table("data/reply.txt")
rev

wordcount <- table(rev)
head(sort(wordcount, decreasing = T), 100)

words <- gsub("영화", "", words)
words <- gsub("진짜", "", words)
words <- gsub("생각", "", words)
words <- gsub("ㅠㅠ", "", words)
words <- gsub("이번", "", words)
words <- gsub("뭔가", "", words)
words <- gsub("이후", "", words)
words <- gsub("스파이더맨", "", words)

write(unlist(words), "data/reply.txt")
rev <- read.table("data/reply.txt")

wordcount <- table(rev)
head(sort(wordcount, decreasing = T), 100)

windows()
windowsFonts(font=windowsFont("맑은 고딕"))

palete <- brewer.pal(10, 'Paired')
wordcloud(names(wordcount), freq=wordcount, scale = c(4,0.4), rot.per = 0.1,
          min.freq = 3, random.order = F, random.color = T, colors = palete, family="font")
#legend(0.1, 0.92, 'NAVER 영화 \'스파이더맨\' 평점 댓글 WordCloud', cex=1, fill=NA, border=NA,
#       bg='navy', text.col='green', text.font=2, box.col='white')


# Star Rating Change over time
# Change by Date
head(points)
star_date <- select(points, score, time)
star_date

tmp <- strptime(star_date$time, "%Y.%m.%d %H:%M")
tmp <- paste0(tmp$mon+1, "월 ", tmp$mday, "일")
star_date$time <- tmp
star_date$score <- factor(star_date$score, levels=c(10:1))
levels(star_date$score)

star_date_bar <- star_date %>%
  group_by(time, score) %>%
  summarise(score_freq = n()) %>%
  arrange(time, score)
star_date_bar
  
star_date_bar2 <- star_date_bar %>%  
  mutate(pct = score_freq/sum(score_freq)*100) %>%
  mutate(ylabel_b = str_c(score, "점 (", format(pct, digits=2), "%)")) %>%
  arrange(desc(score)) %>%
  mutate(ypos = cumsum(score_freq) - 0.5*score_freq) %>%
  arrange(time, desc(score))
star_date_bar2

library(extrafont)
windowsFonts(myfont = "맑은 고딕")
theme_update(text=element_text(family="myfont"))

# Bar Chart
ggplot(data=star_date_bar2, aes(x=time, y=score_freq, fill=score)) + 
  geom_bar(stat="identity") +
  geom_text(aes(y=ypos, label=ylabel_b), cex=3, color="black", family="myfont") +
  theme_light() +
  xlab("날짜(일)") +
  ylab("평점(1~10점)") +
  ggtitle("2019 [스파이더맨 : 파 프롬 홈] \n 평점 빈도") +
  theme(plot.title=element_text(size=rel(2), face="bold", hjust=0.5, family="myfont")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=rel(1.2), family="myfont"),
        axis.title.y = element_text(size=rel(1.2), family="myfont"))

ggplot(data=star_date_bar2, aes(x=time, y=score_freq, fill=score)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(y=rep(seq(360, 900, 60), 3), label=ylabel_b), cex=4, color="navy", family="myfont") +
  theme_light() +
  xlab("날짜(일)") +
  ylab("평점(1~10점)") +
  ggtitle("2019 [스파이더맨 : 파 프롬 홈] \n 평점 빈도") +
  theme(plot.title=element_text(size=rel(2), face="bold", hjust=0.5, family="myfont")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=rel(1.2), family="myfont"),
        axis.title.y = element_text(size=rel(1.2), family="myfont"))

star_date$score <- as.character(star_date$score)
star_date$score <- as.numeric(star_date$score)

star_date_line <- star_date %>%
  group_by(time) %>%
  summarise(avg_score = mean(score))
star_date_line

# Line Chart
ggplot(star_date_line, aes(x=time, y=avg_score, color="red", group=1)) + 
  geom_line(color="red", size=1) +
  #geom_text(aes(y=rep(seq(360, 900, 60), 3), label=ylabel_b), cex=4, color="navy", family="myfont") +
  theme_light() +
  xlab("날짜(일)") +
  ylab("평점(1~10점)") +
  ggtitle("2019 [스파이더맨 : 파 프롬 홈] \n 날짜에 따른 평균 평점 변화") +
  theme(plot.title=element_text(size=rel(2), face="bold", hjust=0.5, family="myfont")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=rel(1.2), family="myfont"),
        axis.title.y = element_text(size=rel(1.2), family="myfont"))

# Change Hourly
# Line Chart(1시간 단위)
star_date2 <- select(points, score, time)
tmp <- strptime(star_date2$time, "%Y.%m.%d %H:%M")
tmp <- paste0(tmp$mon+1, "월 ", tmp$mday, "일 ", tmp$hour, "시")
star_date2$time <- tmp
star_date2$score <- as.character(star_date2$score)
star_date2$score <- as.numeric(star_date2$score)
star_date2

star_hour_line1 <- star_date2 %>%
  group_by(time) %>%
  summarise(avg_score = mean(score))
star_hour_line1

ggplot(star_hour_line1, aes(x=time, y=avg_score, group=1)) + 
  geom_line(color="red", lwd=1) +
  theme_light() +
  xlab("시간(단위: 1시간)") +
  ylab("평점(1~10점)") +
  ggtitle("2019 [스파이더맨 : 파 프롬 홈] \n 시간에 따른 평균 평점 변화") +
  theme(plot.title=element_text(size=rel(2), face="bold", hjust=0.5, family="myfont")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=rel(1.2), family="myfont"),
        axis.title.y = element_text(size=rel(1.2), family="myfont"),
        axis.text.x = element_blank())

# Line Chart(4시간 단위)
time_tmp <- rep(NA, length(tmp))
for(i in 1:length(tmp)) {
  ifelse(tmp$hour[i] <= 4, time_tmp[i] <- paste0(tmp[i]$mon+1, "월 ", tmp[i]$mday, "일 ", "1~4시"),
  ifelse(tmp$hour[i] <= 8, time_tmp[i] <- paste0(tmp[i]$mon+1, "월 ", tmp[i]$mday, "일 ", "4~8시"),
  ifelse(tmp$hour[i] <= 12, time_tmp[i] <- paste0(tmp[i]$mon+1, "월 ", tmp[i]$mday, "일 ", "8~12시"),
  ifelse(tmp$hour[i] <= 16, time_tmp[i] <- paste0(tmp[i]$mon+1, "월 ", tmp[i]$mday, "일 ", "12~16시"),
  ifelse(tmp$hour[i] <= 20, time_tmp[i] <- paste0(tmp[i]$mon+1, "월 ", tmp[i]$mday, "일 ", "16~20시"),
  time_tmp[i] <- paste0(tmp$mon[i]+1, "월 ", tmp$mday[i], "일 ", "21~24시"))))))
}
time_tmp

star_date2$time <- time_tmp
star_date2$score <- as.character(star_date2$score)
star_date2$score <- as.numeric(star_date2$score)
star_date2

star_hour_line2 <- star_date2 %>%
  group_by(time) %>%
  summarise(avg_score = mean(score))
star_hour_line2

ggplot(star_hour_line2, aes(x=time, y=avg_score, group=1)) + 
  geom_line(color="red", lwd=1) +
  theme_light() +
  xlab("시간(단위: 4시간)") +
  ylab("평점(1~10점)") +
  ggtitle("2019 [스파이더맨 : 파 프롬 홈] \n 시간에 따른 평균 평점 변화") +
  theme(plot.title=element_text(size=rel(2), face="bold", hjust=0.5, family="myfont")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=rel(1.2), family="myfont"),
        axis.title.y = element_text(size=rel(1.2), family="myfont"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1))
