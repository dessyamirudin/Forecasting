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

