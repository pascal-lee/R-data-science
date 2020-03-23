require(magrittr)
require(lubridate)
require(tidyverse)
require(gridExtra)
require(kableExtra)

url <-'https://raw.githubusercontent.com/BlankerL/DXY-COVID-19-Data/master/csv/DXYOverall.csv'
filename <-'./data/DXYOverall.csv'

download.file(url, filename)
data.raw<-read.csv(filename)
## select colums
data.raw %<>% select(c(updateTime, curedCount, deadCount,  deadCount,
                       currentConfirmedCount, confirmedCount, suspectedCount,
                       #seriousCount,
                       curedIncr, deadIncr, confirmedIncr, suspectedIncr
                       #curentConfirmedIntr,
                       #seriousIncr
                       )) 

## data preparation
data.raw %<>% mutate(date = date(updateTime)) 
## sort by timestamp
data<-tbl_df(data.raw) %>% 
  group_by(date) %>%
  top_n(1, updateTime)

##sort by date ascendingly and remove updateTime
data %<>% arrange(data) %>% select(-updateTime) 
min.date<-min(data.raw$date)
max.date<-max(data.raw$date)
max.date.txt<-max.date %>% format('%d %B %Y')

