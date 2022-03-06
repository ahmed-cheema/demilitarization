library(tidyverse)
library(lubridate)
library(urbnmapr)

setwd("~/HackGC")

fatalEncounters <- read_csv('data/fatalEncounters.csv') # fatal encounters
policeShootings <- read_csv('data/policeShootings.csv') # wapo
receipts <- read_csv('data/1033full.csv') # dla.mil
nv <- read_csv('data/nevadaPDs.csv')

###

tf <- receipts %>%
  mutate(year=year(date)) %>%
  filter(state == "NV") %>%
  left_join(nv,by='agency') %>%
  group_by(county) %>%
  summarise(count=n(),
            cost=sum(cost))

###

counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% 
  mutate(cat_var = paste0("Group ",
                          sample(1:4, nrow(countydata), replace = TRUE)))

county_data <- left_join(counties_sf, county_groups, by = "county_fips") %>%
  filter(state_name == "Nevada") %>%
  rename(county = county_name) %>%
  left_join(tf, by='county') %>%
  replace(is.na(.), 0)

# graph

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = cost),
          color = "#ffffff", size = 0.05) +
  coord_sf(datum = NA)
