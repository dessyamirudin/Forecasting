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
