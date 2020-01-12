pacman::p_load(tidyverse, lubridate, ggplot2)

createSleepingDataset <- function() {
  read_csv('raw_data/sleeping.csv') %>% 
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
    mutate(duration = duration / 60)
}

sleeping <- createSleepingDataset()

total_sleep_by_date <- sleeping %>% 
  group_by(date) %>% 
  arrange(desc(date)) %>% 
  summarize(total_sleep = sum(duration))

max_sleep <- sleeping %>% 
  group_by(date) %>% 
  arrange(desc(date)) %>% 
  summarize(max_sleep = max(duration))

max_sleep_mean <- mean(max_sleep$max_sleep)

max_sleep %>% 
ggplot() +
  geom_line(aes(x = date, y = max_sleep, color = 'total_sleep'), size = 1.1) +
  scale_color_manual(values = c("firebrick")) +
  labs(y = "Max sleep") +
  guides(color=FALSE) +
  geom_hline(yintercept = max_sleep_mean, linetype="dashed")
  

mean_sleep <- as.numeric(mean(total_sleep_by_date$total_sleep))

sleeping %>% 
  group_by(date) %>% 
  summarize(total_sleep = sum(duration)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = total_sleep, color = 'total_sleep'), size = 1.1) +
  theme_minimal() +
  labs(y = "Sleep hours") +
  scale_color_manual(values = c("skyblue")) +
  guides(color=FALSE) +
  geom_hline(yintercept = mean_sleep, linetype="dashed")
  
# sleeping %>% arrange(desc(duration)) %>% View()
