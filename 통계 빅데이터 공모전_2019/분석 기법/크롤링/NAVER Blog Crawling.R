## https://developers.naver.com/main/ # 네이버 개발자 계정 홈페이지 

urlStr <- "https://openapi.naver.com/v1/search/blog.xml?" # 기본 url 생성 
searchString <- "query=어린이집,맞벌이,육아휴직,경력단절" # 쿼리생성 
searchString <- iconv(searchString, to="UTF-8") # 인코딩 
searchString <- URLencode(searchString)
searchString

etcString <- "&display=100&start=1&sort=sim"

reqUrl <- paste(urlStr, searchString, etcString, sep="")
reqUrl # 요청할 url 생성 

install.packages("httr")
library(httr)

clientid <- "***" # 개인 api id 값
clientSecret <- "***" # 개인 apu secret 값

apiResult <- GET(reqUrl, add_headers("X-Naver-Client-Id"=clientid, 
                                     "X-Naver-Client-Secret"=clientSecret))

apiResult # Status 값이 200이어야 정상. 500 이면 시스템 에러 
str(apiResult)

apiResult$content

result <- rawToChar(apiResult$content)
result
Encoding(result) <- "UTF-8"
result

library(rJava)
library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
useSejongDic()
library(dplyr)
library(stringr)


babymom <- sapply(result,extractNoun,USE.NAMES=F)
babymom <- gsub("<(\\/?)(\\w+)*([^<>]*)>", "", babymom)
babymom <- gsub("[[:punct:]]", "", babymom) # 문장부호 제거
babymom <- gsub("[[:cntrl:]]","",babymom) # 특수문자 제거
babymom <- gsub("[A-z]", "", babymom) # 모든 영문자 제거
babymom <- gsub("[0-9]", "", babymom) # 숫자 제거
babymom <- gsub(" +", "", babymom)
babymom <- gsub("^","", babymom)
babymom <- gsub("ㅋ","", babymom)
babymom <- gsub("ㅎ","", babymom)
babymom <- gsub("ㅜ","", babymom)
babymom <- gsub("naver","",babymom)
babymom <- gsub("blog","",babymom)
babymom <- gsub("https","",babymom)
babymom <- gsub("link","",babymom)
babymom <- gsub("title","",babymom)
babymom <- gsub("com","",babymom)
babymom <- Filter(function(x) {nchar(x)>=2 & nchar(x)<=4}, babymom)
babymom <- unlist(babymom)
wordcount <- table(babymom)
head(sort(wordcount,decreasing=T),100)
require(wordcloud2)
wordcloud2(wordcount,size=5,col="random-dark",rotateRatio=0.5,
           backgroundColor="white",shape="circle")







## Association Analysis
# 출처1 : http://blog.naver.com/PostView.nhn?blogId=nonamed0000&logNo=220959156052&categoryNo=24&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=postView
# 출처2 : http://blog.naver.com/PostView.nhn?blogId=nonamed0000&logNo=220965696087&parentCategoryNo=&categoryNo=24&viewDate=&isShowPopularPosts=false&from=postView
install.packages("tm")
install.packages("arules")
library(tm)
library(arules) 

require(stringr)
ass.result <- result
ass.result <- str_split(ass.result, "</title>")

str(ass.result)

ass.babymom <- c()
for(i in 1:length(ass.result[[1]])) {
  ass.babymom <- c(ass.babymom, ass.result[[1]][i])
}
head(ass.babymom,10)

ass.babymom <- gsub("<(\\/?)(\\w+)*([^<>]*)>","",ass.babymom)
ass.babymom <- gsub("[[:punct:]]","",ass.babymom) # 문장부호 제거
ass.babymom <- gsub("[[:cntrl:]]","",ass.babymom) # 특수문자 제거
ass.babymom <- gsub("[A-z]","",ass.babymom) # 모든 영문자 제거
ass.babymom <- gsub("[0-9]","",ass.babymom) # 숫자 제거
ass.babymom <- gsub("^","",ass.babymom)
ass.babymom <- gsub("ㅋ","",ass.babymom)
ass.babymom <- gsub("ㅎ","",ass.babymom)
ass.babymom <- gsub("ㅜ","",ass.babymom)

require(KoNLP)
lword <- Map(extractNoun,ass.babymom) 
lword <- unique(lword) # 중복제거1(전체 대상)
lword <- sapply(lword, unique) # 중복제거2(줄 단위 대상) 
lword[1:5] # 추출 단어 확인


# 1) 길이가 2~4 사이의 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
# 2) Filter(f,x) -> filter1() 함수를 적용하여 x 벡터 단위 필터링 
filter2 <- function(x){
  Filter(filter1, x)
}
# 3) 줄 단어 대상 필터링
lword <- sapply(lword, filter2)
lword # 추출 단어 확인(길이 1개 단어 삭제됨)


wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran 

# 트랜잭션 내용 보기 -> 각 트랜잭션의 단어 보기
inspect(wordtran)  

# 동일한 단어끼리 교차테이블 작성 
wordtable <- crossTable(wordtran) # 교차표 작성


## 지지도, 신뢰도, 향상도

# 1. 지지도(support) : 전체자료에서 A를 구매한 후 B를 구매하는 거래 비율  
# A->B 지지도 식  
# -> A와 B를 포함한 거래수 / 전체 거래수 
# -> n(A, B) : 두 항목(A,B)이 동시에 포함되는 거래수 
# -> n : 전체 거래 수 

# 2. 신뢰도(confidence) : A가 포함된 거래 중에서 B를 포함한 거래의 비율(조건부 확률) 
# A->B 신뢰도 식 
# -> A와 B를 포함한 거래수 / A를 포함한 거래수 

# 3. 향상도(Lift) : 하위 항목들이 독립에서 얼마나 벗어나는지의 정도를 측정한 값 
# 향상도 식 
# -> 신뢰도 / B가 포함될 거래율 
# 분자와 분모가 동일한 경우 : Lift == 1, A와 B가 독립(상관없음) 
# 분자와 분모가 동일한 경우 : Lift != 1, x와 y가 독립이 아닌 경우(상관있음)

tranrules <- apriori(wordtran, parameter=list(supp=0.4, conf=0.05, minlen=2)) 
inspect(tranrules) # 연관규칙 생성 결과 보기

library(arulesViz) # rules값 대상 그래프를 그리는 패키지
# plot(tranrules, method="graph", control=list(type="items"))
plot(tranrules, method="graph")

# (1) 데이터 구조 변경 : 연관규칙 결과 -> 행렬구조 변경(matrix 또는 data.frame) 
rules <- labels(tranrules, ruleSep=" ")  
# 문자열로 묶인 연관단어를 행렬구조 변경 
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
# 행 단위로 묶어서 matrix로 반환
rulemat <- do.call("rbind", rules)
# (2) 연관어 시각화를 위한 igraph 패키지 설치
library(igraph)   
# (3) edgelist보기 - 연관단어를 정점 형태의 목록 제공 
# ruleg <- graph.edgelist(rulemat[c(6:40),], directed=F) # [1,]~[5,] "{}" 제외
ruleg <- graph.edgelist(rulemat, directed=F)
ruleg
# (4) edgelist 시각화
plot.igraph(ruleg, vertex.label=V(ruleg)$name,
            vertex.label.cex=1.2, vertex.label.color='black', 
            vertex.size=20, vertex.color='gray', vertex.frame.color='blue')







## Sentimental Analysis
# 출처 : https://github.com/park1200656/KnuSentiLex (군산대 감성사전)
# site : https://goo-eungs.tistory.com/22 
# 서울연구원 : https://www.si.re.kr/search/node?keys=%EC%B6%9C%EC%82%B0&type%5B%5D=infographics&ptimes=total&created%5Bmin%5D=&created%5Bmax%5D=

library(plyr)
library(stringr)

setwd("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링")

positive <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/positive.txt",
                      encoding="EUC-KR")
positive <- positive[-1]
negative <- readLines("D:/Workplace/StatisticsKorea_Big_Data_Project/크롤링/군산대 감성사전/negative.txt",
                      encoding="EUC-KR")
negative <- negative[-1]

require(plyr)

sentimental <- function(sentences,positive,negative) {
  
  scores <- laply(sentences,function(sentence,positive,negative) {
    sentence <- gsub('[[:punct:]]','',sentence) # 문장부호 제거
    sentence <- gsub('[[:cntrl:]]','',sentence) # 특수문자 제거
    sentence <- gsub('\\d+','',sentence) # 숫자 제거
    
    word.list <- str_split(sentence,'\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개이상)
    words <- unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    
    pos.matches <- match(words,positive) # words의 단어를 positive에서 matching
    neg.matches <- match(words,negative)
    
    pos.matches <- !is.na(pos.matches) # NA 제거, 위치(숫자)만 추출
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches) # 긍정 - 부정
    return(score)
  }, positive, negative)

  scores.df <- data.frame(score=scores,text=sentences)
  return(scores.df)
}

sen.result <- sentimental(babymom,positive,negative)
sen.result$color[sen.result$score >= 1] <- "blue"
sen.result$color[sen.result$score == 0] <- "green"
sen.result$color[sen.result$score < 0] <- "red"

table(sen.result$color)

sen.result$remark[sen.result$score >= 1] <- "긍정"
sen.result$remark[sen.result$score == 0] <- "중립"
sen.result$remark[sen.result$score < 0] <- "부정"

sentiment_result <- table(sen.result$remark)

pie(sentiment_result,main="감성분석 결과",col=c("blue","red","green"),radius=0.8)

library(ggplot2)
library(dplyr)

table(sen.result$remark)

remark <- c("긍정","부정","중립")
count <- c(39,23,2003)
result1 <- data.frame(remark=remark,count=count)
result1 <- result1 %>%
  mutate(pct=round(count/sum(count)*100,2)) %>%
  mutate(ylabel=paste0("약",pct,"%")) %>%
  mutate(ypos=cumsum(pct)-0.5*pct)
result1$remark <- factor(result1$remark,levels=rev(as.character(result1$remark)))
result1
  
ggplot(result1,aes(x="",y=pct,fill=remark)) +
  geom_bar(stat="identity", width=1) +  
  geom_text(aes(y=ypos,label=ylabel),color="black") +
  coord_polar(theta="y",start=0) +
  xlab("") + ylab("") +
  ggtitle("육아 관련 네이버 블로그 감성분석") +
  theme(plot.title=element_text(color="black",size=16,face="bold",hjust=0.5))









    
    









