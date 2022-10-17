library(fpp3)
library(latex2exp)

# global economy

global_economy %>% 
  filter(Country=="Australia") %>% 
  autoplot(GDP/Population)+labs(title="GDP Per Capita",y="$US")

# aus retail
print_retail <- aus_retail %>% 
  filter(Industry=="Newspaper and book retailing") %>% 
  group_by(Industry) %>% 
  index_by(Year=year(Month)) %>% 
  summarise(Turnover=sum(Turnover))

aus_economy <- global_economy %>% filter(Code=="AUS")

print_retail %>% 
  left_join(aus_economy,by="Year") %>%  
  mutate(Adjusted_turnover=Turnover/CPI*100) %>% 
  pivot_longer(c(Turnover,Adjusted_turnover),values_to = "Turnover") %>% 
  mutate(name=factor(name,levels=c("Turnover","Adjusted_turnover"))) %>% 
  ggplot(aes(x=Year,y=Turnover))+
  geom_line()+
  facet_grid(name~.,scales="free_y")+
  labs(title="Turnover: Australia print media industry",y="$AU")

# lambda
lambda <- aus_production %>% 
  features(Gas,features=guerrero) %>% 
  pull(lambda_guerrero)

aus_production %>% autoplot(box_cox(Gas,lambda))+
  labs(y="",
       title=latex2exp::TeX(paste0("Transformed gas production with $\\lambda$=",round(lambda,2)))
       )

# Time Series Component
us_retail_employment = us_employment %>% 
  filter(year(Month)>=1990,Title=="Retail Trade") %>% 
  select(-Series_ID)

autoplot(us_retail_employment,Employed)+
  labs(y="person(thousands)",title="Total Employment in US Retail")

dcmp = us_retail_employment %>% model(stl=STL(Employed))
components(dcmp)

components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(Employed, colour = "gray")+
  geom_line(aes(y=trend),colour="#D55E00")+
  labs(y="Persons (thousand)",title="Total employment in US retail")

components(dcmp) %>% autoplot()

# Seasonal adjusted data
components(dcmp) %>% 
  as_tsibble() %>% 
  autoplot(Employed,colour="gray")+
  geom_line(aes(y=season_adjust), colour="#0072B2")+
  labs(y="Person (thousand)",title="Total Employment in US retail")

# Moving Average
global_economy %>% 
  filter(Country=="Australia") %>% 
  autoplot(Exports) + labs(y="% of GDP",title="Total Australian Export")

aus_exports=global_economy %>% 
  filter(Country=="Australia") %>% 
  mutate(
    `5-MA`=slider::slide_dbl(Exports,mean,.before=2,.after=2,.complete=TRUE)
  )

aus_exports %>% autoplot(Exports)+
  geom_line(aes(y=`5-MA`),colour="#D55E00")+
  labs(y="% GDP",
       title="Total Australian Export")+
  guides(colour=guide_legend(title="series"))

## moving average of moving average
beer <- aus_production %>% filter(year(Quarter)>=1992) %>% 
  select(Quarter,Beer)
beer_ma <-beer %>% 
  mutate(
    `4-MA`=slider::slide_dbl(Beer,mean,.before = 1,.after = 2,.complete = TRUE),
    `2x4-MA`=slider::slide_dbl(`4-MA`,mean,.before = 1,.after = 0,.complete = TRUE)
    
    
  )

us_retail_employment_ma <-us_retail_employment %>% 
  mutate(
    `12-MA`=slider::slide_dbl(Employed,mean,.before=5,.after = 6,.complete = TRUE),
    `2x12-MA`=slider::slide_dbl(`12-MA`,mean,.before = 1,.after=0,.complete = TRUE)
  )

us_retail_employment_ma %>% autoplot(Employed,colour="gray") +
  geom_line(aes(y=`2x12-MA`,colour="#D55E00"))+
  labs(y='Person (thousands)',
       title='Total Employment in US retail')

# decomposition
us_retail_employment %>% 
  model(
    classical_decomposition(Employed,type="additive")
  ) %>% 
  components() %>% 
  autoplot()+
  labs(title="Classical additive decomposition of total US retail employment")

# X-11 and SEATS
library(seasonal)

x11_dcmp <- us_retail_employment %>% 
  model(x11=X_13ARIMA_SEATS(Employed~x11())) %>% 
  components()

autoplot(x11_dcmp)+
  labs(title="Decomposition of total US retail employment using X-11.")

x11_dcmp %>% 
  ggplot(aes(x=Month))+
  geom_line(aes(y=Employed,colour="Data"))+
  geom_line(aes(y=season_adjust,colour="Seasonally Adjusted"))+
  geom_line(aes(y=trend,colour="Trend"))+
  labs(y="Person thousand",
       title="Total employment in US retail")+
  scale_colour_manual(
    values=c("gray","#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

x11_dcmp %>% gg_subseries(seasonal)

# SEATS
seats_dcmp = us_retail_employment %>% 
  model(seats=X_13ARIMA_SEATS(Employed~seats())) %>% 
  components()

autoplot(seats_dcmp) %>% labs(title="Decomposition of total US retail employment using SEATS")
