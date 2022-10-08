library(fpp3)

# PART 3
vic_elec

vic_elec %>% filter(year(Time)==2014) %>% 
  autoplot(Demand) +
  labs(y="GW",
       title="Half-hourly electricity demand: Victoria")

vic_elec %>% filter(year(Time)==2014) %>% 
  autoplot(Temperature) +
  labs(y="Degrees Celcius",
       title="Half-hourly temperature: Melbourne, Australia")

# scatter plot
vic_elec %>% filter(year(Time)==2014) %>% 
  ggplot(aes(x=Temperature,y=Demand))+
  geom_point()+
  labs(x="Temperature (degress Celcius)",
       y="Electricity demand(GW)")

# tourism
visitors= tourism %>% group_by(State) %>% summarise(Trips=sum(Trips))

visitors %>% ggplot(aes(x=Quarter,y=Trips))+
  geom_line()+
  facet_grid(vars(State),scales="free_y")+
  labs(title="Australian domestic tourism",
       y="Overnight trips('000)")

# pair plot
library(GGally)

visitors %>% pivot_wider(values_from = Trips,names_from = State) %>% 
  GGally::ggpairs(columns=2:9)

# lag scatter plot
aus_production

recent_production = aus_production %>% filter(year(Quarter)>=2000)

recent_production %>% gg_lag(Beer,geom = "point")+
  labs(x="lag(Beer,k)")

# Autocorrelation
recent_production %>% ACF(Beer,lag_max = 9)

recent_production %>% ACF(Beer) %>% autoplot()+labs(title="Australian beer production")

a10 %>% ACF(Cost,lag_max = 48) %>% 
  autoplot()+labs(title="Australian antidiabetic drug sales")

# white noise
set.seed(30)
y=tsibble(sample=1:50,wn=rnorm(50),index=sample)

y %>% autoplot(wn)+labs(title = "White noise",y="")

y %>% ACF(wn) %>% autoplot()+labs(title="White Noise")
