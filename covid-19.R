require(magrittr)
require(lubridate)
require(tidyverse)
require(gridExtra)
require(kableExtra)
require(ggforce)
require(shiny)
require(leaflet)
require(devtools)
install.packages("httpuv", dependencies = TRUE, INSTALL_opts = '--no-lock')

.
## source data file
filenams<-c('time_series_19-covid-Confirmed.csv',
              'time_series_19-covid-Deaths.csv',
              'time_series_19-covid-Recovered.csv')
url.path <-paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                  'master/csse_covid_19_data/csse_covid_19_time_series/')          

## download data
download<-function(filenams){
  url<-file.path(url.path,filenams)
  dest<-file.path('./data',filenams)
  download.file(url, dest)
}

bin<-lapply(filenams,download)


#load data into R

data.confirmed<-read.csv('./data/time_series_19-covid-Confirmed.csv')
data.deaths<-read.csv('./data/time_series_19-covid-Deaths.csv')
data.recovered<-read.csv('./data/time_series_19-covid-Recovered.csv')

dim(data.confirmed)

data.confirmed[1:10, 1:10] %>% 
  kable('latex',booktabs=T, caption = 'Raw Data (Confirmed, Frist Columns only)') %>% 
  kable_styling(font_size = 6, latex_options = c('striped','hold_postion', 'repeat_header'))


n.col<-ncol(data.confirmed)
##get dates from colum names
dates<-names(data.confirmed)[5:n.col] %>% 
  substr(2,8) %>% 
  mdy()
range(dates)

min.date<-min(dates)
max.date<-max(dates)
max.date.txt<-max.date %>% format('%d %B %Y')

## select last column, which is the number of lates confirmed cases
x<-data.confirmed
x$confirmed<-x[,ncol(x)]
x%<>% select(c(Country.Region, Province.State,Lat, Long, confirmed)) %>% 
  mutate(txt=paste0(Country.Region,'-', Province.State,' :',confirmed))

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
data.confirmed<-data.confirmed %>% cleanData() %>% rename(confirmed=count)
data.deaths<-data.deaths %>% cleanData() %>% rename(deaths=count)
data.recovered<-data.recovered %>% cleanData() %>% rename(recovered=count)

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

## remaining confirmed cases
data%<>%mutate(remaining.confirmed=confirmed-deaths-recovered)


## Daily Increase and Death Rates

## sort by country and date
data%<>%arrange(country, date)

## daily increase
## set NA to the increaaseon day1
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

## Visualization
# Ranking by confirmed cases
data.latest<-data %>% filter( date == max(date)) %>% 
  select(country, confirmed,new.confirmed, remaining.confirmed,
         recovered,deaths,new.deaths) %>% 
  mutate(ranking=dense_rank(desc(confirmed))
         )

k.top = 20

top.counties <- data.latest %>% filter(ranking <= k.top +1) %>% 
  arrange(ranking) %>% pull(country) %>% as.character()
  
top.counties %>% setdiff('World') %>% print()  

#add others
top.counties%<>%c('Others')
top.counties

#put all other country in a single group of Others
df<-data.latest %>% filter(!is.na(country)) %>% 
  mutate(country=ifelse(ranking <=  k.top+1, as.character(country), 'Others')) %>% 
  mutate(country=country %>% factor(levels = c(top.counties)))

df%<>%group_by(country) %>% 
  summarise(confirmed=sum(confirmed), new.confirmed=sum(new.confirmed),
            remaining.confirmed=sum(remaining.confirmed), recovered=sum(recovered),
            deaths=sum(deaths),new.deaths=sum(new.deaths)) %>% 
  mutate(death.rate=(100*deaths/confirmed) %>% round(1))
df%<>%select(c(country, confirmed, deaths, death.rate,
               new.confirmed,new.deaths, remaining.confirmed))

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
                                        remaining.confirmed = 'Remaining Confirmed'))


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
data.long<-data %>% filter(country %in% top.counties) %>% 
  select(c(country, date, rate.lower, rate.upper, rate.daily)) %>% 
  mutate(country=factor(country, levels = top.counties)) %>% 
  gather(key = type, value = count, -c(country, date))

#set factor levels to show them in a desirable order
data.long %<>%mutate(type = recode_factor(type,
                                          rate.daily='Daily',
                                          rate.lower= 'Lower bound',
                                          rate.upper = 'Upper bound')) 
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
df<-data.long %>% filter(country %in% top.counties) %>% 
  mutate(country=country %>% factor(levels=c(top.counties)))

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
