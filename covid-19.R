require(magrittr)
require(lubridate)
require(tidyverse)
require(gridExtra)
require(kableExtra)
require(ggforce)
require(shiny)
require(leaflet)
require(devtools)
require(xts)
require(lubridate)
#install.packages("httpuv", dependencies = TRUE, INSTALL_opts = '--no-lock')


## source data file
filenams<-c('time_series_covid19_confirmed_global.csv',
            'time_series_covid19_deaths_global.csv',
            'time_series_covid19_recovered_global.csv')
url.path <-paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                  "master/csse_covid_19_data/csse_covid_19_time_series")
url.path <-paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                  'master/csse_covid_19_data/csse_covid_19_time_series/')

## Dwonload data                  
download<-function(filenams){
  url<-file.path(url.path,filenams)
  dest<-file.path('./data',filenams)
  download.file(url, dest)
}

bin<-lapply(filenams,download)


#load data into R

raw.data.confirmed<-read.csv('./data/time_series_covid19_confirmed_global.csv')
raw.data.deaths<-read.csv('./data/time_series_covid19_deaths_global.csv')
raw.data.recovered<-read.csv('./data/time_series_covid19_recovered_global.csv')

dim(raw.data.confirmed)

raw.data.confirmed[1:10, 1:10] %>% 
  kable('latex',booktabs=T, caption = 'Raw Data (Confirmed, Frist Columns only)') %>% 
  kable_styling(font_size = 6, latex_options = c('striped','hold_postion', 'repeat_header'))


n.col<-ncol(raw.data.confirmed)
##get dates from colum names
dates<-names(raw.data.confirmed)[5:n.col] %>% 
  substr(2,8) %>% 
  mdy()
range(dates)

min.date<-min(dates)
max.date<-max(dates)
max.date.txt<-max.date %>% format('%d %B %Y')

## select last column, which is the number of lates confirmed cases

## data cleaning and transformation
cleanData<-function(data){
  ## remove some columns
  data%<>% select(-c(Province.State, Lat,Long)) %>% 
    rename(country=Country.Region)
  ##convert from wide to long format
  data%<>%gather(key=date, value=count, -country)
  ## convert from character to date
  data%<>%mutate(date=date %>% substr(2,8) %>% mdy())
  data%<>%group_by(country, date) %>% summarise(count=sum(count, na.rm = T)) %>% 
    as.data.frame()
  return(data)
}
## clean the three datasets
data.confirmed<-raw.data.confirmed %>% cleanData() %>% rename(confirmed=count)
data.deaths<-raw.data.deaths %>% cleanData() %>% rename(deaths=count)
data.recovered<-raw.data.recovered %>% cleanData() %>% rename(recovered=count)

## merge above 3 dataset into one, by country and date
data<-data.confirmed %>% merge(data.deaths) %>% merge(data.recovered)

## countries/region with confirmed cases, excl, cruise ships
countries<-data %>% pull(country) %>% setdiff('Cruise Ships')

## first 10 records when it first broke out in china
data %>% filter(country=='China') %>% head(10) %>% 
  kable('latex',booktabs=T, caption = 'Raw Data (with first 10 Columns only)',
        format.args = list(big.mark=',')) %>% 
  kable_styling(latex_options = c('striped','hold_postion','repeat_header'))

#counts for the whole world

data.world<-data %>% group_by(date) %>% 
  summarise(country='World',
            confirmed=sum(confirmed),
            deaths=sum(deaths),
            recovered=sum(recovered))
data %<>%rbind(data.world)

## current confirmed cases
data%<>%mutate(current.confirmed=confirmed-deaths-recovered)



## Daily Increase and Death Rates

## sort by country and date
data%<>%arrange(country, date)


## daily increase
## set NA to the increaase on day1
n<-nrow(data)
day1<-min(data$date)
data%<>%mutate(new.confirmed = ifelse(date == day1, NA, confirmed-lag(confirmed, n=1)),
               new.deaths=ifelse(date == day1, NA, deaths-lag(deaths, n =1)),
               new.recovered=ifelse(date == day1, NA, recovered-lag(recovered, n =1)))

## change negative number of new cases to zero
data%<>%mutate(new.confirmed = ifelse(new.confirmed < 0 , 0, new.confirmed ),
               new.deaths=ifelse(new.deaths < 0,0, new.deaths),
               new.recovered=ifelse(new.recovered < 0, 0, new.recovered)
)
#  death rate, recover rate
data %<>%mutate(rate.upper = (100*deaths/(deaths + recovered)) %>% 
                  round(1))
data %<>%mutate(rate.lower = (100*deaths/confirmed) %>% 
                  round(1))
data %<>%mutate(rate.daily = (100*new.deaths/(new.deaths + new.recovered)) %>% 
                  round(1))


# convert from wide to long format, for drawing area plots
data.long<-data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>% 
  gather(key=type, value=count, -c(country,date))
##set factor levels to show them in a desirable order
data.long %<>%mutate(type=recode_factor(type, confirmed= 'Total Confirmed',
                                        current.confirmed='Current Confirmed',
                                        recovered = 'Recovered',
                                        deaths='Deaths')) 

rates.long <-data %>% select(c(country, date, rate.upper, rate.lower, rate.daily)) %>% 
  gather(key=type, value=count, -c(country, date))

rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily',
                                          rate.lower='Lower bound',
                                          rate.upper='Upper bound')) 

## Number of cases
world.long<-data.long %>% filter(country == 'World')

plot1<-world.long %>% filter(type != 'Total Confirmed') %>%
  ggplot(aes(x=date, y=count))+
  geom_area(aes(fill=type), alpha=0.5)+
  labs(title = paste0('Number of Cases Worldwide - ', max.date.txt))+
  scale_fill_manual(values = c('red','green','black'))+
  theme(legend.title = element_blank(), legend.position = 'bottom',
        plot.title = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot2<-world.long %>% 
  ggplot(aes(x=date, y=count))+
  geom_line(aes(color = type))+
  labs(title = paste0('Number of Cases Worldwide (log scale) - ', max.date.txt ))+
  scale_color_manual(values = c('purple', 'red', 'green', 'black'))+
  theme(legend.title = element_blank(), legend.position = 'bottom',
        plot.title = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(trans = 'log10')

grid.arrange(plot1, plot2, ncol=2)

## Current Confirmed Cases
data.world<-data %>% filter(country == 'World')
n<-nrow(data.world)
## current confiremd and daily new confiremd
plot11<-ggplot(data.world, aes(x=date, y=current.confirmed))+
  geom_point()+geom_smooth()+
  xlab('') + ylab('Count') + labs(title='Current Confiremd Cases')+
  theme(axis.text.x = element_text(angle=45, hjust =1))
plot21<-ggplot(data.world, aes(x=date, y=new.confirmed))+
  geom_point()+geom_smooth()+
  xlab('')+ylab('Count') + labs(title='Daily New Confirmed Cases')+
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot12<-ggplot(data.world, aes(x=date, y=current.confirmed))+
  geom_point()+geom_smooth()+
  xlab('') + ylab('Count') + labs(title='Current Confiremd Cases (Log scale)')+
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  scale_y_continuous(trans = 'log10')
plot22<-ggplot(data.world, aes(x=date, y=new.confirmed))+
  geom_point()+geom_smooth()+
  xlab('')+ylab('Count') + labs(title='Daily New Confirmed Cases(Log scale)')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(trans = 'log10')

grid.arrange(plot11, plot12, plot21, plot22,ncol=2)

head(world.long,n=100)



## Word map

x<-raw.data.confirmed
x$confirmed<-x[,ncol(x)]

x%<>% select(c(Country.Region, Province.State,Lat, Long, confirmed)) %>% 
  mutate(txt=paste0(Country.Region,'-', Province.State,' :',confirmed))
m <- leaflet(width = 1200, height = 800) %>% addTiles()
# circle marker(units in pexels)
m%<>%addCircleMarkers(x$Long,x$Lat, radius = 2+log2(x$confirmed), stroke = F,
                      color = 'red',
                      fillOpacity = 0.3 ,
                      popup = x$txt)

m

## Total 
## Visualization
# Ranking by confirmed cases
data.latest<-data %>% filter( date == max(date)) %>% 
  select(country, confirmed,new.confirmed, current.confirmed,
         recovered,deaths,new.deaths) %>% 
  mutate(ranking=dense_rank(desc(confirmed)))

k.top = 20

top.countries <- data.latest %>% filter(ranking <= k.top +1) %>% 
  arrange(ranking) %>% pull(country) %>% as.character()

top.countries %>% setdiff('World') %>% print()  

#add others
top.countries%<>%c('Others')
top.countries

#put all other country in a single group of Others
df<-data.latest %>% filter(!is.na(country)) %>% 
  mutate(country=ifelse(ranking <=  k.top+1, as.character(country), 'Others')) %>% 
  mutate(country=country %>% factor(levels = c(top.countries)))

df%<>%group_by(country) %>% 
  summarise(confirmed=sum(confirmed), new.confirmed=sum(new.confirmed),
            current.confirmed=sum(current.confirmed), recovered=sum(recovered),
            deaths=sum(deaths),new.deaths=sum(new.deaths)) %>% 
  mutate(death.rate=(100*deaths/confirmed) %>% round(1))
df%<>%select(c(country, confirmed, deaths, death.rate,
               new.confirmed,new.deaths, current.confirmed))

#convert wide to long format for area drawing
df.long<-df %>% filter(country != 'World') %>% 
  gather(key = type, value = count, -country)

#set factor levels to show them with proper text and a desirable order
df.long %<>%mutate(type = recode_factor(type,
                                        confirmed = 'Total Confirmed',
                                        deaths = 'Total Deaths',
                                        death.rate = 'Deaths  Rate (%)',
                                        new.confirmed = 'New Comfirmed (Compared with one day before)',
                                        new.deaths = 'New Deaths (Compared with one day before)',
                                        current.confirmed = 'Current Confirmed'))


# bar chart
df.long %>% ggplot(aes(x=country, y= count, group=country, fill=country))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label = count, y = count ), size = 2, vjust = 0)+
  xlab('')+ylab('')+
  labs(title = paste0('Top 20 countries with Most Confirmed Cases - ',max.date.txt))+
  scale_fill_discrete(name='Country',labels= df$country)+
  theme(legend.title = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 11),
        axis.text =  element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust =1))+
  facet_wrap(~type, ncol = 1, scales = 'free_y')

## convert from wide to long format,
data.long<-data %>% filter(country %in% top.countries) %>% 
  select(c(country, date, rate.lower, rate.upper, rate.daily)) %>% 
  mutate(country=factor(country, levels = top.countries)) %>% 
  gather(key = type, value = count, -c(country, date))

#set factor levels to show them in a desirable order
data.long %<>%mutate(type = recode_factor(type,
                                          rate.daily='Daily',
                                          rate.lower= 'Lower bound',
                                          rate.upper = 'Upper bound')) 
## Countries vs. Rate
k.top =10
top.countries <- data.latest %>% filter(ranking <= k.top +1) %>% 
  arrange(ranking) %>% pull(country) %>% as.character()

data.rate.long<-data %>%  
  filter(country %in% top.countries) %>% 
  filter(country != 'World') %>% 
  select(c(country, date, confirmed)) %>% 
  mutate(country=factor(country, levels = top.countries))

str(data.rate.long)

rate_plot1<-ggplot(data.rate.long, aes(x=date, y=confirmed))+
  geom_line(aes(color=country))+
  xlab('') + ylab('Count') + labs(title='Top 10 Confirmed')+
  theme(axis.text.x = element_text(angle=45, hjust =1))
rate_plot1
rate_plot2<-ggplot(data.rate.long, aes(x=date, y=confirmed))+
  geom_line(aes(color=country))+
  xlab('') + ylab('Count') + labs(title='Top 10 Confirmed(Log scale)')+
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  scale_y_continuous(trans = 'log10')

grid.arrange(rate_plot1,rate_plot2,ncol=1)

## Weekly increaseing rate
weekly_data<-data %>%
  filter(country %in% top.countries) %>%
  filter(country != 'World') %>% 
  mutate(weeks=week(date)) %>% 
  group_by(country, weeks) %>% 
  summarise(weekly_new.confirmed=sum(new.confirmed,na.rm=T))

rate_plot11<-ggplot(weekly_data, aes(x=weeks, y=weekly_new.confirmed))+
  geom_line(aes(color=country,linetype=country))+
  geom_point(aes(shape=country))+
  scale_shape_manual(values=1:nlevels(weekly_data$country))+
  xlab('Week') + ylab('Count') + labs(title='Top 10 Weekly New Confirmed')+
  theme(axis.text.x = element_text(angle=45, hjust =1))
rate_plot11
rate_plot22<-ggplot(weekly_data, aes(x=weeks, y=weekly_new.confirmed))+
  geom_line(aes(color=country,linetype=country))+
  geom_point(aes(shape=country))+
  scale_shape_manual(values=1:nlevels(weekly_data$country))+
  xlab('Week') + ylab('Count') + labs(title='Top 10 Weekly New Confirmed(Log Scale)')+
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  scale_y_continuous(trans = 'log10')
rate_plot22
grid.arrange(rate_plot11,rate_plot22,ncol=1)

tail(weekly_data)

data %>% select(c(date, country, new.confirmed)) %>% 
  filter(country %in% top.countries) %>%
  filter(country != 'World') %>% 
  ggplot(aes(x=date, y=new.confirmed))+
  geom_line(aes(color=country))+
  xlab('Date') + ylab('Count') + labs(title='Top 10 Weekly New Confirmed')+
  theme(axis.text.x = element_text(angle=45, hjust =1))

rate_plot11
week("13/4/2020")
m
max.date.txt
## three deaths rate
rate.max <-data.long$count %>%max(na.rm = T)
ggplot(data.long,aes(x=date, y=count, color=type))+
  geom_line()+
  xlab('')+ylab('Deaths Rate(%)')+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5,'cm'),
        axis.text.x = element_text(angle = 45,hjust = 1))+
  ylim(c(0,100))+
  facet_wrap(~country,ncol=4)

# Confirmed vs. Deaths

plot1<-df %>% filter(country != 'World') %>% 
  ggplot(aes(x=confirmed, y=deaths, col=death.rate))+
  geom_text(aes(label=country),size=2.5, check_overlap = T, vjust=-0.8)+
  geom_point()+
  xlab('Total Confirmed')+ylab('Total Deaths')+
  labs(col='Death Rate(%)')+
  scale_color_gradient(low='#56B1F7', high='#132B43')+
  scale_x_log10()+scale_y_log10()
plot1

plot2<-df %>% filter(country != 'World') %>% 
  ggplot(aes(x=new.confirmed, y=new.deaths, col=death.rate)) +
  geom_text(aes(label=country),size=2.5, check_overlap = T, vjust=-0.7)+
  geom_point()+
  xlab('New Confirmed')+ylab('New Death')+
  labs(col='Death Rate(%)')+
  scale_color_gradient(low='#56B1F7', high='#132B43')+
  scale_x_log10()+scale_y_log10()

plot2
grid.arrange(plot1, plot2, ncol=1)


## Comparison across Countries
data.long <-data %>% 
  select(c(country, date, confirmed, remaining.confirmed,
           recovered,deaths)) %>% 
  gather(key=type, value=count, -c(country, date))
# set factor levels to show them in a desirable order
data.long %<>%mutate(type=recode_factor(type, confirmed='Confirmed',
                                        remaining.confirmed='Remaining Confirmed',
                                        recovered='Recovered',
                                        deaths = 'Deaths')) 
## plot; cases by type
df<-data.long %>% filter(country %in% top.countries) %>% 
  mutate(country=country %>% factor(levels=c(top.countries)))

p<-df %>% filter(country != 'World') %>% 
  ggplot(aes(x=date, y=count))+
  xlab('')+ylab('Count')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 11),
        axis.text.x =element_text(angle = 45, hjust = 1) )+
  facet_wrap(~type, ncol=2, scales = 'free_y')
plot1<-p + geom_area(aes(fill=country))+
  labs(title = paste0('Cases around the World - ', max.date.txt))

## line plot and in log scale
linetypes<-rep(c('solid','dashed','dotted'), each=8)
colors <- rep(c('black','blue','red', 'green','orange', 'purple', 'yellow', 'grey'),3)
plot2<-p + geom_line(aes(color=country, linetype = country))+
  scale_linetype_manual(values = linetypes)+
  scale_color_manual(values = colors)+
  labs(title = paste0('Cases around the World - Log Scale - ', max.date.txt)) +
  scale_y_continuous(trans = 'log10')
## show 2 plots together
grid.arrange(plot1, plot2, ncol=1)

# plot:  excluding china data
p<-df %>% filter(!(country %in% c('World','China'))) %>% 
  ggplot(aes(x=date, y=count)) + xlab('')+ylab('Count') +
  theme(legend.title=element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, 'cm'),
        plot.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~type, ncol=2, scales='free_y')
p+geom_area(aes(fill=country))+
  labs(title = paste0('Cases around the World (exl. China) - ', max.date.txt ))
