# 한빛아카데미 도서 크롤링
install.packages('rvest')
library(rvest)
library(dplyr)
library(stringr)
library(xlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page <- 'page='
category <- '&cate_cd='
num_cate <- c('004008', '004008', '004003', '004004', '004005', '004006',
              '005005', '005001', '005002', '005003', '005004')
tails <- '&srt=p_pub_date'

url <- c()
for(i in 1:length(num_cate)) {
  for(j in 1:6) {
    url <- c(url, paste0(base_url, page, j, category, num_cate[i], tails))
  }
}

computer_books <- c()
for(i in 1:length(url)) {
  html <- read_html(url[i])
  
  #book_list <- html_node(html, '.sub_book_list_area')
  #lis <- html_nodes(book_list, 'li')
  #lis
  
  # dplyr 이용
  html %>%
    html_node('.sub_book_list_area') %>%
    html_nodes('li') -> lis
  
  price <- c()
  title <- c()
  writer <- c()
  for(li in lis) {
    pr <- html_node(li, 'span.price') %>% html_text()
    pr <- gsub("\\\\", "", pr)
    price <- c(price, pr)
    title <- c(title, html_node(li, '.book_tit') %>% html_text())
    writer <- c(writer, html_node(li, '.book_writer') %>% html_text())
    #cat(title, writer, price, '\n')
  }
  books <- data.frame(title=title, writer=writer, price=price)
  computer_books <- rbind.data.frame(computer_books, books)
}
computer_books

# write.xlsx(computer_books, "D:/workspace-Jwp/R/R-Project/01_Crowling/computer_books.xlsx")
