library(fpp3)

# PART 2
melsyd_economy <-ansett %>% filter(Airports=="MEL-SYD",Class=="Economy") %>% 
  mutate(Passengers=Passengers/1000)

autoplot(melsyd_economy,Passengers) +
  labs(title="Ansett airline economy class",
       subtitle = "Melbourne-Sydney",
       y="Passengers('000)")

# note - use data a10 from Part 1
autoplot(a10,Cost)+
  labs(y="$ (million)",
       title="Australian antidiabetic drug sales")

# seasonal plot
a10 %>% gg_season(Cost,labels = "both")+
  labs(y="$ (millions)",
       title="Seasonal plot: Antidiabetic drug sales")

vic_elec %>% gg_season(Demand,period="day")+
  theme(legend.position = "none")+
  labs(y="MWh",title="Electricity demand:Victoria")

vic_elec %>% gg_season(Demand,period="week")+
  theme(legend.position = "none")+
  labs(y="MWh",title="Electricity demand:Victoria")

vic_elec %>% gg_season(Demand,period="year")+
  theme(legend.position = "right")+
  labs(y="MWh",title="Electricity demand:Victoria")

# Subplot
a10 %>% gg_subseries(Cost)+
  labs(y="$ (millions)",
       title="Australian antidiabetic drug sales")

# tourism
holidays = tourism %>% filter(Purpose=="Holiday") %>% 
  group_by(State) %>% summarise(Trips=sum(Trips))

holidays

autoplot(holidays,Trips)+
  labs(y="Overnight trips ('000)",
       title="Australian domestic holidays")

gg_season(holidays,Trips) +
  labs(y="Overnight trips ('000)",
       title="Australian domestic holidays")

# subplot
holidays %>% gg_subseries(Trips)+labs(y="Overnight trips('000)",
                                      title="Australian domestic holidays")
