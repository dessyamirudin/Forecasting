# PART 1
library(fpp3)

y = tsibble(
  Year = 2015:2019,
  Observation = c(123,39,78,52,110),
  index=Year
)

z = tibble(
  Month = c('2019 Jan','2019 Feb','2019 Mar','2019 Apr','2019 May'),
  Observation = c(50,23,34,30,25)
)

# convert z to tsibble
z = z %>% mutate(Month = yearmonth(Month)) %>% as_tsibble(index=Month)

# data olympic running
olympic_running %>% distinct(Sex)

# PBS Data
PBS %>% filter(ATC2=="A01")

PBS %>% filter(ATC2=="A10") %>% select(Month,Concession,Type,Cost)

# index Month, and key still in the dataframe eventhough not selected
PBS %>% filter(ATC2=="A10") %>% select(ATC2,ATC2_desc)

PBS %>% filter(ATC2=="A10") %>% select(Month,Concession,Type,Cost) %>% summarise(TotalC=sum(Cost))

PBS %>% filter(ATC2=="A10") %>% select(Month,Concession,Type,Cost) %>% 
  summarise(TotalC=sum(Cost)) %>% mutate(Cost = TotalC/1e6) -> a10

# prison data
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison %>% 
  mutate(Quarter=yearquarter(Date)) %>% 
  select(-Date) %>% 
  as_tsibble(key=c(State,Gender,Legal,Indigenous),
             index = Quarter)

prison

# PART 2
