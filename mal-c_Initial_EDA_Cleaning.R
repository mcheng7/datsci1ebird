
# set ebd path
auk::auk_set_ebd_path("data/unprocessed", overwrite = TRUE)

sample <- "ebd_US-IL_202008_202009_relSep-2020/ebd_US-IL_202008_202009_relSep-2020.txt"

ebd_dir <- "data/unprocessed"

f_path <- file.path(ebd_dir, "ebd_US-IL_202008_202009_relSep-2020.txt")

big_data <- "data/unprocessed/big_data.txt"

###Note this was the hardest part, figuring out how/what to filter using the auk package!
#lots of arguments/functions, needed to learn and map so the data wasnt too big
n_sample_big <- f_path %>% 
  auk_ebd() %>% 
  auk_complete() %>% 
  auk_filter(file = big_data, overwrite = TRUE) %>% 
  read_ebd() %>% 
  glimpse()

write.csv(n_sample_big, "data/unprocessed/big_sample.csv")
big_csv <- read_csv("data/unprocessed/big_sample.csv")


#writing a function to filter out particular protocols like Historical and Incidental, which aren't applicable to my analysis because they aren't proper collections of data
#possible to negate %in%?

`%notin%` <- negate(`%in%`)

# selecting relevant data to further downsize 

filtered_data <- big_csv %>% 
  filter("protocol_type" %notin% c("Historical", "Incidental")) %>% 
  select(checklist_id, observer_id, common_name, taxonomic_order, observation_date, time_observations_started, duration_minutes, country, country_code, state, state_code, county, county_code, latitude, longitude)

write.csv(filtered_data, "data/processed/filtered_data.csv")

###TALKING ABOUT CHECKLISTS

# Basic questions
# - How many distinct species of birds were there in IL in September? 
filtered_data %>%  distinct(common_name)

#   - How many checklists were submitted?
filtered_data %>% distinct(checklist_id)

# - Number of people submitting checklists?

filtered_data %>% distinct(observer_id)

#   - Most number of checklists by one person?

checklist_data <- filtered_data %>% distinct(observer_id, checklist_id) #getting distinct checklist ID in tandem with observer id

count(checklist_data, observer_id, sort = TRUE)

#1307. in a month. what??

ggplot(checklist_data, mapping = aes(x = observer_id)) +
  geom_bar()

#   - Most number of checklists in one day? 
dates <- filtered_data %>% 
  distinct(observation_date, checklist_id)

count(dates, observation_date, sort = TRUE)

chrono_dates <- arrange(dates, observation_date)

ggplot(chrono_dates, mapping = aes(x = observation_date))+
  geom_bar()+
  labs(title= "Number of checklists submitted by date")

## At what times are the most checklists submitted?

#   - What day of the week? 
#the data is by entry so it nees to be filtered down for individual checklists
time_data <- filtered_data %>% distinct(observation_date, checklist_id, time_observations_started) 
time_data %>% 
  mutate(day_of_week = wday(observation_date, label = TRUE)) %>% 
  group_by(day_of_week) %>% 
  summarize(num_checklists = n()) %>% 
  ggplot(aes(x = day_of_week, y = num_checklists, fill = day_of_week))+
  geom_bar(stat = "identity")+
  labs(title = "eBird checklists submitted by weekday")


#   - What time of day?

time_data %>% 
  mutate(hour_of_day = hour(time_observations_started)) %>% 
  group_by(hour_of_day) %>% 
  summarize(num_checklists = n(), na.rm=TRUE) %>% 
  ggplot(aes(x = hour_of_day, y = num_checklists))+
  geom_point()+
  geom_line()+
  labs(title = "eBird checklists submitted by hour")

# What if we group them for a clearer time of day?

time_data %>% 
  mutate(hour_of_day = hour(time_observations_started)) %>% 
  mutate(time_of_day = ifelse(hour_of_day %in% 0:4, "Night", 
                              ifelse(hour_of_day %in% 5:10, "Morning",
                                     ifelse(hour_of_day %in% 11:16, "Afternoon",
                                            ifelse(hour_of_day %in% 17:21, "Evening","Night"))))) %>% 
  group_by(time_of_day) %>% 
  summarize(num_checklists = n(), na.rm = TRUE) %>% 
  ggplot(aes(x = time_of_day, y = num_checklists))+
  geom_point()

#   - What time of day for each weekday??? (needs facet wrap?)
order <- c("Morning", "Afternoon", "Evening", "Night")

time_data %>% 
  mutate(hour_of_day = hour(time_observations_started)) %>% 
  mutate(time_of_day = ifelse(hour_of_day %in% 0:4, "Night", 
                              ifelse(hour_of_day %in% 5:10, "Morning",
                                     ifelse(hour_of_day %in% 11:16, "Afternoon",
                                            ifelse(hour_of_day %in% 17:21, "Evening","Night"))))) %>% 
  mutate(day_of_week = wday(observation_date, label = TRUE)) %>% 
  group_by(time_of_day, day_of_week) %>% 
  summarize(num_checklists = n(), na.rm = TRUE) %>% 
  ggplot(aes(x = factor(time_of_day, level = order), y = num_checklists))+
  geom_point()+
  facet_wrap(~day_of_week, ncol = 1)

#   What bird is most frequently observed?

head(count(filtered_data, common_name, sort = TRUE))

top_birds <- c("American Goldfinch", "Northwern Cardinal", "American Robin", "Blue Jay", "Mourning Dove", "Downy Woodpecker")


#   - Week by week change?

week1 <- interval(ymd("2020-08-01"), ymd("2020-08-07"))
week2 <- interval(ymd("2020-08-08"), ymd("2020-08-14"))
week3 <- interval(ymd("2020-08-15"), ymd("2020-08-21"))
week4 <- interval(ymd("2020-08-22"), ymd("2020-08-28"))
week5 <- interval(ymd("2020-08-29"), ymd("2020-09-04"))
week6 <- interval(ymd("2020-09-05"), ymd("2020-09-11"))
week7 <- interval(ymd("2020-09-12"), ymd("2020-09-18"))
week8 <- interval(ymd("2020-09-19"), ymd("2020-09-25"))
week9 <- interval(ymd("2020-09-26"), ymd("2020-09-30"))

top_observations <- filtered_data %>% 
  filter(common_name %in% top_birds)


top_observations %>% 
  mutate(week_num = ifelse(observation_date %within% week1, "Week 1",
                           ifelse(observation_date %within% week2, "Week 2",
                                  ifelse(observation_date %within% week3, "Week 3",
                                         ifelse(observation_date %within% week4, "Week 4",
                                                ifelse(observation_date %within% week5, "Week 5",
                                                       ifelse(observation_date %within% week6, "Week 6",
                                                              ifelse(observation_date %within% week7, "Week 7",
                                                                     ifelse(observation_date %within% week1, "Week 8", "Week 9"))))))))) %>% 
  group_by(week_num, common_name) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x = common_name, y = num_observations, color = common_name))+
  geom_point()+
  facet_wrap(~week_num, ncol=1) #easiest to read in ncol = 1


#made cleaner with case_when

top_observations %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) %>% 
  group_by(week_num, common_name) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x = common_name, y = num_observations, color = common_name))+
  geom_point()+
  facet_wrap(~week_num, ncol=1) #easiest to read in ncol = 1


#so we can see that the biggest differences is that there are suddenly a lot more blue jays in weeks 7 and 8, a lot fewer goldfinches and mourning doves as the weather gets colder

top_observations %>% 
  filter(common_name == "Blue Jay") %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) %>%  
  filter(week_num != "Week 9") %>% #removing incomplete week
  group_by(week_num) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x=week_num, y = num_observations)) +
  geom_point()+
  geom_line(aes(group = 1))+
  scale_y_continuous(limit = c(0,2000))+
  labs(title = "Blue Jay Observations by Week", subtitle = "Starting August 1, 2020 through September 25, 2020")
  
#compared to migratory? warblers are migratory

filtered_data %>% 
  filter(taxonomic_order %in% (5577:5987)) %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) %>%  
  filter(week_num != "Week 9") %>% #removing incomplete week
  group_by(week_num) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x=week_num, y = num_observations)) +
  geom_point()+
  geom_line(aes(group = 1))+
  labs(title = "Warbler Observations by Week", subtitle = "Starting August 1, 2020 through September 25, 2020")

#but there are also warblers that are not called warblers, so we consulted the ebird taxonomic order because then they are grouped together

filtered_data %>% 
  filter(taxonomic_order %in% (5577:5987)) %>% 
  group_by(observation_date) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x=observation_date, y = num_observations)) +
  geom_point()+
  geom_line(aes(group = 1))+
  labs(title = "Shorebird Observations by Week", subtitle = "Starting August 1, 2020 through September 25, 2020")


filtered_data %>% 
  filter(taxonomic_order %in% (32667:33029)) %>% 
  mutate(observation_date = ymd(observation_date)) %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) %>% 
  group_by(week_num) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x=observation_date, y = num_observations)) +
  geom_point()+
  geom_line(aes(group = 1))+
  labs(title = "Warbler Observations by Week", subtitle = "Starting August 1, 2020 through September 25, 2020")

filtered_data %>% 
  filter(taxonomic_order %in% (5577:5987)) %>% 
  mutate(observation_date = ymd(observation_date)) %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) %>%  
  filter(week_num != "Week 9") %>% #removing incomplete week
  group_by(week_num) %>% 
  summarize(num_observations = n()) %>% 
  ggplot(aes(x=week_num, y = num_observations)) +
  geom_point()+
  geom_line(aes(group = 1))+
  labs(title = "Shorebird Observations by Week", subtitle = "Starting August 1, 2020 through September 25, 2020")



#Are there rare birds??
#birds that are definitely there all the time: American Robins, Crows
#birds there more in august, less in september: Yellow Warbler and Eastern Kingbird
#birds there less in august, more in september: Ovenbird, waterthrushes
#taxonomic codes for all warblers: 32667 to 33029
#taxonomic codes for shorebirds: 5577 to 5987


#   - Do we see any migratory trends? see: shorebirds and warblers, visible by map?

#maps of counties taken from census

background <- st_read("cb_2018_us_county_20m/cb_2018_us_county_20m.shp", quiet = TRUE) 

background_smol <- background %>% filter(STATEFP == "17") #code for IL is 17

#make a copy of filtered_data
filtered_data_x <- filtered_data
#add weeks?
filtered_data_week <- 
  filtered_data_x %>% 
  mutate(week_num = case_when(
    observation_date %within% week1 ~ "Week 1",
    observation_date %within% week2 ~ "Week 2",
    observation_date %within% week3 ~ "Week 3",
    observation_date %within% week4 ~ "Week 4",
    observation_date %within% week5 ~ "Week 5",
    observation_date %within% week6 ~ "Week 6",
    observation_date %within% week7 ~ "Week 7",
    observation_date %within% week8 ~ "Week 8",
    observation_date %within% week9 ~ "Week 9"
  )) 
#make a month column
filtered_data_week$month <- format(as.Date(filtered_data_week$observation_date,format="%Y-%m-%d"), format = "%m")

#divide it up for specifics!
#Yellow Warblers (not very warbler-pattern following)
yellow_warblers_map <- filtered_data_week %>% 
  filter(common_name=="Yellow Warbler")

coordinates(yellow_warblers_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(yellow_warblers_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_ywarbler = st_as_sf(yellow_warblers_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_ywarbler, color = "gold")+
  facet_wrap( ~week_num)+
  labs(title = "Yellow Warbler Observation Map")

#Wilson's warbler?
wilsons_warbler_map <- filtered_data_week %>% 
  filter(common_name=="Wilson's Warbler")

coordinates(wilsons_warbler_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(wilsons_warbler_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_wwarbler = st_as_sf(wilsons_warbler_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_wwarbler, color = "yellow3")+
  facet_wrap( ~week_num)+
  labs(title = "Wilson's Warbler Observation Map")

#Overall Warblers

all_warblers_map <- filtered_data_week %>% 
  filter(taxonomic_order %in% (32667:33029))
  
coordinates(all_warblers_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(all_warblers_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_warbler = st_as_sf(all_warblers_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_warbler, aes(color= common_name))+
  facet_wrap( ~month)+
  labs(title = "All Warbler Observation Map")


#Spotted Sandpiper

spotted_sandpiper_map <- filtered_data_week %>% 
  filter(common_name=="Spotted Sandpiper")

coordinates(spotted_sandpiper_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(spotted_sandpiper_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_ssandpiper = st_as_sf(spotted_sandpiper_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_ssandpiper, color = "gold3")+
  facet_wrap( ~week_num)+
  labs(title = "Spotted Sandpiper Observation Map")


#other
sp_sandpiper_map <- filtered_data_week %>% 
  filter(common_name=="Semipalmated Sandpiper")

coordinates(sp_sandpiper_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(sp_sandpiper_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_spsandpiper = st_as_sf(sp_sandpiper_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_spsandpiper, color = "gold3")+
  facet_wrap( ~week_num)+
  labs(title = "Semipalmated Sandpiper Observation Map")

#Shorebirds

all_shorebirds_map <- filtered_data_week %>% 
  filter(taxonomic_order %in% (5577:5987))

coordinates(all_shorebirds_map) <- c("longitude", "latitude") #these are the column names, longitude first, then latitude

proj4string(all_shorebirds_map) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

data_shorebirds = st_as_sf(all_shorebirds_map, coords = c("longitude", "latitude"))

ggplot(background_smol) +
  geom_sf()+
  geom_sf(data = data_shorebirds, aes(color= common_name))+
  scale_color_viridis_d()+
  facet_wrap( ~month)+
  labs(title = "All Shorebirds Observation Map")

###Some adjustments to code are made in the EDA because knitr was being picky

