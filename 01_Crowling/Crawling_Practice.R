# Crawling Practice
library(rvest)
library(dplyr)
library(stringr)
library(xlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# 1. 위키피디아 '신은총' 검색 결과 크롤링
url_n <- 'https://ko.wikipedia.org/w/index.php?search=machine+learning&title=%ED%8A%B9%EC%88%98%3A%EA%B2%80%EC%83%89&go=%EB%B3%B4%EA%B8%B0&ns0=1'
html <- read_html(url_n)
html

data <- html_nodes(html, 'div.searchresult')
data <- html_text(data)
data
length(data)

search_result <- data.frame(no = c(1:length(data)), result=data)
View(search_result)

# 2. 서울특별시 빅데이터 캠퍼스 공무전 분석결과 크롤링
base_url <- 'https://bigdata.seoul.go.kr/noti/selectPageListNoti.do?r_id=P440&bbs_seq=&sch_type=&sch_text=&currentPage='
page = c(1:5)

anlysis <- c()
for(i in 1:length(page)) {
  url <- paste0(base_url, page[i])
  
  html <- read_html(url)
  html
  html %>%
    html_node('.board_list') %>%
    html_nodes('td') %>%
    html_text('.title') -> dataset
  
  table_data <- trim(dataset)
  
  no <- c()
  title <- c()
  date <- c()
  view <- c()
  for(i in 1:(length(table_data)/4)*4) {
    no <- c(no, table_data[i-3])
    title <- c(title, table_data[i-2])
    date <- c(date, table_data[i-1])
    view <- c(view, table_data[i])
  }
  df <- data.frame(no = no, title = title, date = date, view = view)
  anlysis <- rbind.data.frame(anlysis, df)
}
anlysis

# write.xlsx(anlysis, "D:/workspace-Jwp/R/R-Project/01_Crowling/anlysis.xlsx")

# 3. 성격테스트 결과 크롤링
kinds <- c('intj', 'intp','entj','entp','infj','infp','enfj','enfp','istj','isfj','estj','esfj','istp','isfp','estp','esfp')

url <- 'https://www.16personalities.com/ko/%EC%84%B1%EA%B2%A9%EC%9C%A0%ED%98%95-'
urls <- c()
for(i in kinds){
  urls <- c(urls,paste0(url,i))
}

fcontent <- c()
for (i in urls){
  content <- read_html(i)
  content <- html_node(content,'article')
  content <- html_text(content)
  content <- gsub("[\r\n\t]", "", content)
  fcontent <- trim(content)
}
head(fcontent)

# 4. stack over flow 크롤링
url <- 'https://stackoverflow.com/questions?tab=newest&page='
urls <- c()

for(i in 1:10){
  urls <- c(urls,paste0(url,i))
}

fcontent <- c()
for (i in urls){
  print(i)
  content <- read_html(i)
  
  content <- html_nodes(content,'.flush-left')
  content <- html_nodes(content,'h3')
  content <- html_text(content,'a')
  fcontent <- c(fcontent, content)
}
fcontent

# 5. 청와대 6.21 ~ 7.2 청원 내용 크롤링
num_page <- c()
for (i in 581000:581184){
  num_page<-c(num_page,i)
  
}

urls <- c()
for (i in num_page){
  url <- paste0('https://www1.president.go.kr/petitions/',toString(i))
  urls <- c(urls,url)
}

fcontent <- c()
for (i in urls){
  html <- read_html(i)
  html %>%
    html_node('.petitionsView_write') %>%
    html_nodes('div') -> divs
  divs <- html_text(divs)
  
  divs <- gsub("[\r\n\t]", "", divs)
  fcontent <- c(fcontent,trim(divs))
}
head(fcontent)
