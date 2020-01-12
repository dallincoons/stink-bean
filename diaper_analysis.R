pacman::p_load(tidyverse, lubridate, ggplot2, chron)

source('./sleeping_analysis.R')

sleeping <- createSleepingDataset() %>% 
  filter(start_am_pm == 'PM') %>%
  filter(duration > 3) %>% 
  filter(as.POSIXct(strptime(start_time_am_pm, "%I:%M:%S %p"), tz="") > as.POSIXct(strptime("05:00:00 PM", "%I:%M:%S %p"))) %>% 
  arrange(desc(duration))

diapers <- read_csv('raw_data/diapers.csv') %>% 
  filter(am_pm == "PM") %>% 
  filter(date %in% sleeping$date) %>% 
  arrange(date)
