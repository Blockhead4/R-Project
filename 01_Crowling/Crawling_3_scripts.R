# 네이버 자동 로그인 및 메일 크롤링
install.packages("RSelenium")
library(RSelenium)
library(rvest)
library(stringr)
library(xlsx)

remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open()

remDr$navigate("https://nid.naver.com/nidlogin.login") #접속할 사이트 입력한다.

#DOM-->문서 전체의 element간의 계층모델로 가짐

#findElement() 함수를 이용하여 특정 위치를 지정할 수 있습니다. 
txt_id<-remDr$findElement(using="id",value="loginId") #id="~~~~"의 형식이 나타나는데, 차례대로 입력하면 된다.
txt_id<-remDr$findElement(using="css selector", "#id")
txt_pw<-remDr$findElement(using="id",value="pw")
login_btn<-remDr$findElement(using="class",value="btn_global") #로그인 버튼도 id/pw와 비슷하게 class='~~~~"입력

txt_id$setElementAttribute("value","block*****") # ★에 아이디 입력
txt_pw$setElementAttribute("value","******") # ★에 비밀번호 입력
login_btn$clickElement()

remDr$navigate("https://mail.naver.com/")
mail_texts <- remDr$findElement(using="id", value="list_for_view")
                                # (using = 'css selector', "subject")
mail_texts
mail_texts <- mail_texts$getElementText()
tmp <- str_split(mail_texts, '\n') %>% .[[1]]
tmp

sender <- c()
subject <- c()
time <- c()
for (i in 1:20) {
  sender <- c(sender, tmp[3*i-2])
  subject <- c(subject, tmp[3*i-1])
  time <- c(time, tmp[3*i])
}
df_mail <- data.frame(sender=sender, subject=subject, time=time)
df_mail
remDr$close()
