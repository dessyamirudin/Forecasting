library(fpp3)
library(latex2exp)

# Question 1
global_economy %>% 
  filter(Country %in% c("Albania","Afghanistan")) %>% 
  autoplot(GDP/Population)+labs(title="GDP Per Capita",y="$US")

# need to calculate using loops to find the country where the GDP per Capita is highest

# Question 2
# United States Global Economy
global_economy %>% 
  filter(Country == "United States") %>% 
  autoplot(GDP/Population)+labs(title="GDP Per Capita",y="$US")

aus_livestock %>% filter(Animal == "Bulls, bullocks and steers",State=="Victoria") %>% 
  autoplot(Count)+labs(title="Slaughter Animal Annual")

lambda_a <- aus_livestock %>% filter(Animal == "Bulls, bullocks and steers",State=="Victoria") %>% 
  features(Count,features=guerrero) %>% 
  pull(lambda_guerrero)

aus_livestock %>% filter(Animal == "Bulls, bullocks and steers",State=="Victoria") %>% 
  autoplot(box_cox(Count,lambda_a))+labs(title="Slaughter Animal Annual")

vic_elec %>% autoplot(Demand)

aus_production %>% autoplot(Gas)

# Question 3

# Canadian gas data
canadian_gas %>% autoplot(Volume)

lambda_b <- canadian_gas %>% 
  features(Volume,features=guerrero) %>% 
  pull(lambda_guerrero)

canadian_gas %>% autoplot(box_cox(Volume,lambda_b))

# unable to transform because by 1990 above, the seasonality component varies

# Question 4

set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)

lambda_c <- myseries %>% 
  features(Turnover,features=guerrero) %>% 
  pull(lambda_guerrero)

myseries %>% autoplot(box_cox(Turnover,lambda_c))

# Question 5

# 1
aus_production %>% autoplot(Tobacco)

lambda_t = aus_production %>% features(Gas,features=guerrero) %>% pull(lambda_guerrero)
aus_production %>% autoplot(box_cox(Tobacco,lambda_t))

# 2
ansett %>% data.frame() %>% group_by(Airports) %>% summarize() 

# MEL-SYD 
ansett %>% filter(Airports=="MEL-SYD",Class=="Economy") %>% autoplot(Passengers)

lambda_m = ansett %>% filter(Airports=="MEL-SYD",Class=="Economy") %>% 
  features(Passengers,features = guerrero) %>% pull(lambda_guerrero)

ansett %>% filter(Airports=="MEL-SYD",Class=="Economy") %>% autoplot(box_cox(Passengers,0.01)) # change lambda between 0 to 1 to see effect
ansett %>% filter(Airports=="MEL-SYD",Class=="Economy") %>% autoplot(box_cox(Passengers,lambda_m)) # change lambda between 0 to 1 to see effect
# lambda guerrero seems unable to do the trick to get stable variance.

# 3
pedestrian %>% data.frame() %>% group_by(Sensor) %>% summarise()

# Southern Cross Station
pedestrian %>% filter(Sensor=="Southern Cross Station") %>% autoplot(Count)
pedestrian %>% filter(Sensor=="Southern Cross Station") %>% autoplot(box_cox(Count,3))
lambda_s = pedestrian %>% filter(Sensor=="Southern Cross Station") %>% features(Count,features = guerrero) %>% pull(lambda_guerrero)
pedestrian %>% filter(Sensor=="Southern Cross Station") %>% autoplot(box_cox(Count,lambda_s))
