pacman::p_load(tidyverse, lubridate, ggplot2)

createSleepingDataset <- function() {
  sleeping <- read_csv('raw_data/sleeping.csv') %>%
    mutate(start_time_am_pm = paste(crib_start, start_am_pm)) %>%
    mutate(start_time_am_pm = paste(start, start_am_pm)) %>%
    mutate(end_time_am_pm = paste(end, end_am_pm)) %>%
    mutate(hour = hour(start)) %>%
    mutate(hour_am_pm = paste(hour, start_am_pm)) %>%
    arrange(date) %>%
    mutate(
      duration = (as.POSIXct(strptime(end_time_am_pm, "%I:%M:%S %p"), tz="") -
        as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="")) / 60
    ) %>%
    mutate(
      duration = case_when(
        duration < 0 ~ 1440 + duration,
        TRUE ~ duration
      )  
    ) %>% 
    mutate(n = row_number()) %>%
    arrange(n) %>%
    mutate(time_awake = ((as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") - as.POSIXct(strptime(lag(end_time_am_pm), "%I:%M:%S %p"), tz="")))) %>% 
    mutate(
      time_awake = case_when(
        time_awake < 0 ~ 1440 + time_awake,
        TRUE ~ time_awake
      )  
    ) %>% 
    mutate(time_awake = time_awake / 60) %>% 
    mutate(
      duration = case_when(
        duration < 0 ~ 1440 + duration,
        TRUE ~ duration
      )  
    )
  
  return(sleeping)
}

sleeping <- createSleepingDataset()

total_sleep_by_date <- sleeping %>% 
  group_by(date) %>% 
  arrange(desc(date)) %>% 
  summarize(total_sleep = abs(sum(duration)) / 60)

crib <- sleeping %>% 
  filter(!is.na(crib_start)) %>%
  mutate(in_crib_am_pm = paste(crib_start, start_am_pm)) %>%
  mutate(in_crib = (as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="")) - (as.POSIXct(strptime(in_crib_am_pm, "%I:%M:%S %p"), tz=""))) %>% 
  mutate(in_crib = as.integer(in_crib) / 60) %>% 
  mutate(
    in_crib = case_when(
      in_crib < 0 ~ 720 + in_crib,
      TRUE ~ in_crib
    )
  )

crib %>% 
  filter(as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") > as.POSIXct(strptime('06:00:00 AM', "%I:%M:%S %p"), tz="")) %>% 
  filter(as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") < as.POSIXct(strptime('07:00:00 PM', "%I:%M:%S %p"), tz="")) %>% 
  ggplot(aes(x = time_awake, y = in_crib)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

crib %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  summarize(total_crib = sum(in_crib)) %>% 
    ggplot() +
    geom_line(aes(x = date, y = (total_crib / 60), color = 'total_sleep'), size = 1.1) +
    labs(y = "Hours in crib before sleep") +
    guides(color=FALSE)

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
  mutate(end_time_am_pm = as.POSIXct(strptime(end_time_am_pm, "%I:%M:%S %p"), tz="")) %>% 
  mutate(wake_up_hour = hour(end_time_am_pm)) %>%
  select(wake_up_hour) %>% 
  filter(wake_up_hour >= 10) %>% 
  filter(wake_up_hour <= 15) %>% 
  table()

# crib %>% 
#   group_by(date) %>% 
#   arrange(desc(date)) %>% 
#   summarize(total_crib = abs(sum(in_crib, na.rm=T)) / 60) %>% 
#   ggplot() +
#   geom_line(aes(x = date, y = total_crib, color = 'total_crib'), size = 1.1) +
#   theme_minimal()

sleeping %>% 
  group_by(date) %>% 
  summarize(total_sleep = abs(sum(duration)) / 60) %>% 
  arrange(date) %>% 
  ggplot() +
  geom_line(aes(x = date, y = total_sleep, color = 'total_sleep'), size = 1.1) +
  theme_minimal() +
  labs(y = "Sleep hours") +
  scale_color_manual(values = c("skyblue")) +
  guides(color=FALSE) +
  geom_hline(yintercept = mean_sleep, linetype="dashed")


sleeping %>% 
  filter(date > "2019-12-10") %>% 
  group_by(date) %>% 
  mutate(id = as.numeric(date)) %>%
  ungroup() %>% 
  mutate(group = case_when(
    as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") < as.POSIXct(strptime("06:00:00 AM", "%I:%M:%S %p"), tz="") ~ id - 1,
    T ~ id
  )) %>%
  group_by(group) %>%
  select(start_time_am_pm, id, group, duration, date) %>% 
  arrange(group) %>% 
  summarize(total_sleep = abs(sum(duration)) / 60, date = first(date)) %>%
  ggplot() +
  geom_line(aes(x = date, y = total_sleep, color = 'total_sleep'), size = 1.1) +
  theme_minimal() +
  labs(y = "Sleep hours") +
  scale_color_manual(values = c("skyblue")) +
  guides(color=FALSE) +
  geom_hline(yintercept = mean_sleep, linetype="dashed")
