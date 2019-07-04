# Change Hourly
star_date2 <- select(points, score, time)
tmp <- strptime(star_date2$time, "%Y.%m.%d %H:%M")
tmp <- paste0(tmp$mon+1, "월 ", tmp$mday, "일 ", tmp$hour, "시")
star_date2$time <- tmp
star_date2$score <- as.character(star_date2$score)
star_date2$score <- as.numeric(star_date2$score)
star_date2

star_hour_line <- star_date2 %>%
  group_by(time) %>%
  summarise(avg_score = mean(score))
star_hour_line  

  

ggplot(star_date2, aes(x=time, y=score, color="red", group=1)) + geom_line()
