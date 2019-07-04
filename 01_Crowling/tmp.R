# Change Hourly
# 1시간 단위 Line Chart
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

# 4시간 단위 Line Chart
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
