# JSON 파일로부터 읽어서 데이터프레임 만들기
library(jsonlite)

setwd('D:/workspace-Jwp/R/R-Project/02_JSON')

# person.json 파일로부터 읽기
wiki_person <- fromJSON('person.json')
str(wiki_person)
class(wiki_person)

# sample.json
data <- fromJSON('sample.json')
str(data)

data <- as.data.frame(data)
names(data) <- c('id', 'like', 'share', 'comment', 'unique', 'msg', 'time')
data$like <- as.numeric(as.character(data$like))

# CSV 파일로 저장
write.csv(data, 'data.csv')
read.csv('data.csv')

# Data Frame을 JSON 파일로 저장
json_data <- toJSON(data)
write(json_data, 'data.json')
prettify(json_data)