pacman::p_load(tidyverse, lubridate, ggplot2, chron)

feeding <- read_csv('raw_data/feeding.csv') %>% 
  mutate(duration = as.numeric(difftime(end,start))) %>% 
  mutate(start_time_am_pm = paste(start, start_am_pm)) %>% 
  mutate(end_time_am_pm = paste(end, end_am_pm)) %>% 
  mutate(hour = hour(start)) %>% 
  mutate(hour_am_pm = paste(hour, start_am_pm)) %>% 
  arrange(date) %>% 
  mutate(
      duration = as.POSIXct(strptime(end_time_am_pm, "%I:%M:%S %p"), tz="") -
                 as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="")
    ) %>% 
  mutate(
    duration = case_when(
      duration < 0 ~ 1440 + duration,
      TRUE ~ duration
    )
  ) %>% 
  mutate(
    mil_start_time = times(format(strptime(start_time_am_pm, "%I:%M:%S %p"), "%H:%M:%S")),
    mil_end_time = times(format(strptime(end_time_am_pm, "%I:%M:%S %p"), "%H:%M:%S"))
  ) %>% 
  arrange(date, mil_start_time) %>% 
  mutate(time_since_last_feeding =
           (as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") -
              as.POSIXct(strptime(lag(end_time_am_pm), "%I:%M:%S %p"), tz="")) / 60
  ) %>% 
  mutate(time_since_last_feeding = case_when(
    time_since_last_feeding < 0 ~ time_since_last_feeding + 1440,
    TRUE ~ time_since_last_feeding
  )) %>% 
  mutate(week = week(date))

feeding[is.na(feeding)] <- 0

# todo; break it up per week
feeding %>% 
  ggplot(aes(x = hour_am_pm)) +
  geom_histogram(stat_bin = 30, stat = "count") +
  scale_x_discrete(limits=c(
    "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM",
    "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
    "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM",
    "8 PM", "9 PM", "10 PM", "11 PM"
  )) +
  labs(x = 'Hour of the day')

feeding %>% 
  ggplot(aes(x = hour_am_pm)) +
    geom_histogram(stat_bin = 30, stat = "count") +
    scale_x_discrete(limits=c(
      "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM",
      "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
      "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM",
      "8 PM", "9 PM", "10 PM", "11 PM"
    )) +
    labs(x = 'Hour of the day') +
    facet_grid(week ~ .)
  

table(feeding$date)

feeding %>% 
  filter(side == 'R') %>% 
  summarize(mean_duration = mean(duration))

feeding %>% 
  filter(side == 'L') %>% 
  summarize(mean_duration = mean(duration))

# feeding %>% 
#   group_by(date) %>% 
#   summarize(count = sum(n())) %>% 
#   ggplot(aes(x = date, y = count)) +
#     geom_line()

feeding_summary <- feeding %>% 
  group_by(date) %>% 
  summarize(duration_sum = as.numeric(sum(duration) / 60))

feeding %>%
  mutate(time_since_last_feeding =
      (as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") -
      as.POSIXct(strptime(lag(end_time_am_pm), "%I:%M:%S %p"), tz="")) / 60
  ) %>%
  mutate(time_since_last_feeding = case_when(
    time_since_last_feeding < 0 ~ time_since_last_feeding + 1440,
    TRUE ~ time_since_last_feeding
  ))

previous <- feeding[1,]
group_number <- 1

for (row in 1:nrow(feeding)) {
  
  time_since_last_feeding <- feeding[row, "time_since_last_feeding"]
  
  if (time_since_last_feeding < 70) {
    feeding[row, "group_number"] <- group_number
    feeding[row, "recliner_minutes"] <- time_since_last_feeding
  } else {
    group_number <- group_number + 1
    feeding[row, "group_number"] <- group_number
    feeding[row, "recliner_minutes"] <- 0
  }
}

feeding %>% 
  filter(date > '2019-12-3') %>% 
  group_by(date, group_number) %>% 
  summarize(
      group_duration = as.numeric(sum(duration)),
      total_recliner_minutes = as.numeric(sum(recliner_minutes))
    ) %>% 
    group_by(date) %>% 
    summarize(total_duration = sum(group_duration) + sum(total_recliner_minutes)) %>% 
    mutate(total_recliner_hours = total_duration / 60) %>% 
    ggplot() +
    geom_line(aes(x = date, y = total_recliner_hours, color = 'total_recliner_hours'), size = 1.1) +
    theme_minimal() +
    labs(y = "Hours in recliner") +
    scale_color_manual(values = c("skyblue")) +
    guides(color=FALSE)

mean_duration <- mean(feeding_summary$duration_sum)

feeding_summary %>%
  ggplot() +
  geom_line(aes(x = date, y = duration_sum)) +
  geom_hline(yintercept = mean_duration, linetype="dashed")
 