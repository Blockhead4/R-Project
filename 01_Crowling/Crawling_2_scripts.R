# NAVER 영화('알라딘') 일반인 리뷰 크롤링
library(rvest)
library(stringr)
library(dplyr)
library(xlsx)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

url_base <- 'https://movie.naver.com'
start_url <- '/movie/bi/mi/point.nhn?code=163788#tab'
url <- paste0(url_base, start_url, encoding="euc-kr")
html <- read_html(url)
html %>%
  html_node('iframe.ifr') %>%
  html_attr('src') -> url2

ifr_url <- paste0(url_base, url2) 
html2 <- read_html(ifr_url)
html2 %>%
  html_node('div.score_result') %>%
  html_nodes('li') -> lis

score <- c()
review <- c()
writer <- c()
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

review = data.frame(score=score, review=review, writer=writer, time=time)
review


# NAVER 영화 ('스파이더맨 : 파프롬 홈') 일반인 리뷰 크롤링
ifr_base_url <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='
pages <- c(1:242)

reviews <- c()
for(i in 1:length(pages)) {
  url_all <- paste0(ifr_base_url, pages[i])
  html <- read_html(url_all)
  
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
  reviews <- rbind.data.frame(reviews, result)
}
reviews

# write.xlsx(reviews, "D:/workspace-Jwp/R/R-Project/01_Crowling/reviews.xlsx")


