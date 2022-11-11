library(fpp3)
library(latex2exp)


global_economy %>% 
  filter(Country %in% c("Albania","Afghanistan")) %>% 
  autoplot(GDP/Population)+labs(title="GDP Per Capita",y="$US")

# need to calculate using loops to find the country where the GDP per Capita is highest

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

# Canadian gas data
canadian_gas %>% autoplot(Volume)

lambda_b <- canadian_gas %>% 
  features(Volume,features=guerrero) %>% 
  pull(lambda_guerrero)

canadian_gas %>% autoplot(box_cox(Volume,lambda_b))

# unable to transform because by 1990 above, the seasonality component varies


