library(fpp3)

# Exercise
gafa_stock %>% distinct(Symbol)

# plotting for Apple
gafa_stock %>% filter(Symbol=="AAPL") %>% autoplot(Open)+labs(x="Date")
gafa_stock %>% filter(Symbol=="AAPL") %>% autoplot(High)+labs(x="Date")
gafa_stock %>% filter(Symbol=="AAPL") %>% autoplot(Low)+labs(x="Date")
gafa_stock %>% filter(Symbol=="AAPL") %>% autoplot(Volume)+labs(x="Date")

# close price
gafa_stock %>% filter(Symbol=="AAPL") %>% autoplot(Close)+labs(x="Day of Closing Price")
gafa_stock %>% filter(Symbol=="AMZN") %>% autoplot(Close)+labs(x="Day of Closing Price")
gafa_stock %>% filter(Symbol=="FB") %>% autoplot(Close)+labs(x="Day of Closing Price")
gafa_stock %>% filter(Symbol=="GOOG") %>% autoplot(Close)+labs(x="Day of Closing Price")

# with group by clause
gafa_stock %>% group_by(Symbol) %>% autoplot(Close)+labs(x="Day of Closing Price")

# tute
tute1 = readr::read_csv("https://OTexts.com/fpp3/extrafiles/tute1.csv")

mytimeseries = tute1 %>% mutate(Quarter=yearmonth(Quarter)) %>% as_tsibble(index = Quarter)

mytimeseries %>% pivot_longer(-Quarter) %>% ggplot(aes(x=Quarter,y=value,color=name))+
  geom_line()+facet_grid(name~.,scales="free_y")

# US Gas
library(USgas)

us_total = us_total %>% as_tsibble(index=year,key=state)

us_total %>% filter(state %in% c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut","Rhode Island")) %>% 
  autoplot()

# tourism
tourism
class(tourism)

# maximum average trip
tourism %>% select(Region,Purpose,Trips) %>% group_by(Region,Purpose) %>% summarise(avg_trip=mean(Trips)) %>% arrange(desc(avg_trip))

# tsibble seems hard to summarise because index and key will be always there

tourism %>% group_by(Region,Purpose) %>% summarise(Trips=sum(Trips))

# plot
aus_production %>% select(Bricks) %>% autoplot()
pelt %>% select(Lynx) %>% autoplot()
gafa_stock %>% select(Close) %>% autoplot()
vic_elec %>% select(Demand) %>% autoplot()

# arrival
aus_arrivals %>% autoplot()
aus_arrivals %>% gg_season()
aus_arrivals %>% gg_subseries()

# myseries
set.seed(0809)
myseries = aus_retail %>% filter(`Series ID`==sample(aus_retail$`Series ID`,1))

myseries %>% autoplot()
myseries %>% gg_season()
myseries %>% gg_subseries()
myseries %>% gg_lag(Turnover,geom="point")
myseries %>% ACF(Turnover) %>% autoplot()

# plotting from several data
my_us_employment = us_employment %>% filter(Series_ID==sample(us_employment$Series_ID,1))
my_us_employment %>% autoplot()
my_us_employment %>% gg_season()
my_us_employment %>% gg_subseries()
my_us_employment %>% gg_lag(Employed,geom="point")
my_us_employment %>% ACF(Employed) %>% autoplot()

# livestock
head(aus_livestock)
aus_livestock %>% as_tibble() %>% select(Animal) %>% distinct()

aus_livestock %>% filter(Animal=="Pigs") %>% autoplot()
aus_livestock %>% filter(Animal=="Pigs") %>% ACF(Count) %>% autoplot()

# google stock
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

head(dgoog)
dgoog %>% ACF(diff) %>% autoplot() 
