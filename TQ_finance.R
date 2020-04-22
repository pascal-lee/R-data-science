require(tidyverse)
require(tidyquant)

#Getting and Charting Annul Returns

FANG_annual_returns<-FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               type = "arithmetic")
FANG_annual_returns %>% 
  ggplot(aes(x =date, y=yearly.returns, fill=symbol))+
  geom_col()+
  geom_hline(yintercept = 0, color = palette_light()[[1]])+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "FANG: Annual Returns",
       subtitle = "Get annul returns quickly with tq_transmute!",
       y= "Annual Returns", x= "")+
  facet_wrap(~symbol, ncol=2, scales = "free_y")+
  theme_tq()+
  scale_fill_tq()

#Get Daily Log Returns
FANG_daily_log_returns <-FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               type = "log",
               col_rename = "monthly.returns")
FANG_daily_log_returns %>% 
  ggplot(aes(x=monthly.returns, fill= symbol))+
  geom_density(alpha = 0.5)+
  labs(title = "FANG: Charting the Daily Log Returns",
       x = "Monthly Returns", y = "Density")+
  theme_tq()+
  scale_fill_tq()+
  facet_wrap(~symbol, ncol =2)

# Use xts to.period to change periodicity from daily to monthly
FANG %>% 
  group_by(symbol) %>%
  tq_transmute(select = open:volume,
               mutate_fun = to.period,
               period = "month")
# Without periodicity aggregation
FANG_daily<-FANG %>% 
  group_by(symbol)

FANG_daily %>% 
  ggplot(aes(x =date, y=adjusted, color = symbol))+
  geom_line(size = 1)+
  labs(title= "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color ="")+
  facet_wrap(~symbol, ncol=2, scales= "free_y")+
  scale_y_continuous(labels = scales::dollar)+
  theme_tq()+
  scale_color_tq()
str(FANG)
names(FANG)
tail(FANG, n=10)

#With Monthly Periodicity Aggregation
FANG_monthly<-FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = to.period,
               period = "month")
FANG_monthly %>% 
  ggplot(aes(x = date, y = adjusted, color = symbol))+
  geom_line(size = 1)+
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "" )+
  facet_wrap(~symbol, ncol =2, scales="free_y")+
  scale_y_continuous(labels = scales::dollar)+
  theme_tq()+
  scale_color_tq()
# Asset Returns
FANG_returns_monthly<-FANG %>% 
  group_by(symbol) %>% 
  tq_transmute(select =  adjusted,
               mutate_fun = periodReturn,
               period='monthly')
baseline_returns_monthly<-"XLK" %>% 
  tq_get(get = "stock.prices",
         from = "2013-01-01",
         to = "2016-12-31") %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly')
# join the asset returns
returns_joined<-left_join(FANG_returns_monthly,
                          baseline_returns_monthly,
                          by = 'date')
returns_joined

FANG_rolling_corr<-returns_joined %>% 
  tq_transmute_xy(x = monthly.returns.x,
                  y = monthly.returns.y,
                  mutate_fun = runCor,
                  n  =6 ,
                  col_rename="rolling.corr.6")
FANG_rolling_corr %>% 
  ggplot(aes(x = date, y = rolling.corr.6, color = symbol))+
  geom_hline(yintercept = 0, color = palette_light()[[1]])+
  geom_line(size = 1)+
  labs(title = "FANG: Six Month Rolling Correlation to XLK",
       x = "", y = "Correlation", color ="")+
  facet_wrap(~ symbol, ncol = 2)+
  theme_tq()+
  scale_color_tq()
# Moving average convergence divergence
FANG_macd <- FANG %>% 
  group_by(symbol) %>% 
  tq_mutate(select = close,
            mutate_fun = MACD,
            nFast = 12,
            nSlow = 26,
            nSig = 9,
            maType = SMA) %>% 
  mutate(diff = macd -signal) %>% 
  select(-(open:volume))
FANG_macd
#Visualize
FANG_macd %>% 
  filter(date >= as_date("2016-10-01")) %>%
  ggplot(aes(x = date))+
  geom_hline(yintercept = 0, color= palette_light()[[1]])+
  geom_line(aes(y = macd, col = symbol))+
  geom_line(aes( y= signal), color = "blue", linetype =2)+
  geom_bar(aes(y = diff), stat = 'identity', color = palette_light()[[1]])+
  facet_wrap(~symbol, ncol = 2,scale = "free_y")+
  labs(title = "FANG: Moving Average Convergence Divergence",
       y = "MACD", x = "", color ="")+
  theme_tq()+
  scale_color_tq()
  