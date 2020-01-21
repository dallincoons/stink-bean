pacman::p_load(tidyverse, lubridate)

diapers <- read_csv('raw_data/diapers.csv') %>% 
  mutate(hour = hour(time)) %>% 
  mutate(hour_am_pm = paste(hour, am_pm)) %>% 
  arrange(date) %>% 
  mutate(week = week(date))

diapers %>% 
  ggplot(aes(x = hour_am_pm)) +
  geom_histogram(stat_bin = 30, stat = "count") +
  scale_x_discrete(limits=c(
    "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM",
    "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM",
    "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM",
    "8 PM", "9 PM", "10 PM", "11 PM"
  )) + facet_grid(week ~ .)

table(diapers$date)

diapers_summary <- diapers %>%  
  group_by(date) %>% 
  summarize(count = sum(n()))

diapers_mean <- mean(diapers_summary$count)




poop <- diapers %>% filter(stool == 1) %>% 
  group_by(date) %>% 
  summarize(count = sum(n()))

poop_mean <- mean(poop$count)

poop %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  geom_hline(yintercept = poop_mean, linetype="dashed") +
  labs(title = 'Poopy diapers')

pee <- diapers %>% filter(urine == 1) %>% 
  group_by(date) %>% 
  summarize(count = sum(n()))

pee_mean <- mean(pee$count)

pee %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  geom_hline(yintercept = pee_mean, linetype="dashed") +
  labs(title = 'Pee')

 diapers_summary %>% 
  ggplot(aes(x = date, y = count)) +
  geom_line() +
  geom_hline(yintercept = diapers_mean, linetype="dashed") +
  labs(title = 'Diapers')
