video_game <- read_delim("vgsales.csv")

##dimension
dim(video_game)

##max year
video_game %>% 
  select(Year) %>% 
  filter(Year != "N/A") %>% 
  summarize(max_year = max(Year))

##min year
video_game %>% 
  select(Year) %>% 
  filter(Year != "N/A") %>% 
  summarize(min_year = min(Year))

##types of platforms
video_game %>% 
  select(Platform) %>% 
  reframe(unique_platform = unique(Platform)) %>% 
  print(n = 31)

video_game %>% 
  select(Platform) %>% 
  reframe(diff_platform = n_distinct(Platform))

##ex. different platforms in 2004
video_game %>% 
  select(Platform, Year) %>%
  filter(Year == "2004") %>% 
  reframe(diff_platform_year = n_distinct(Platform))

##group sales and time

video_game %>% 
  select(Platform, Year) %>% 
  group_by(Year)

video_game %>% 
  select(Platform, Global_Sales, Year) %>% 
  group_by(Year)

video_game %>% 
  select(Global_Sales,
         Year) %>% 
  group_by(Year) %>% 
  summarize(global_sales_in_that_year = mean(Global_Sales))

video_game %>% 
  select(Global_Sales,
         Year,
         Publisher) %>% 
  filter(!is.na(Global_Sales),!is.na(Year), !is.na(Publisher)) %>%
  group_by(Publisher, Year) %>% 
  summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
  print(n = 2500)


video_game %>% 
  select(Global_Sales,
         Year,
         Publisher) %>% 
  filter(!is.na(Global_Sales),!is.na(Year), !is.na(Publisher)) %>%
  group_by(Publisher, Year) %>% 
  summarize(global_sales_in_that_year = mean(Global_Sales)) %>% 
  ggplot(aes(x = Year, y = global_sales_in_that_year)) +
  geom_line() +
  labs(x = "Year", y = "Sales in Million")





##average every game that the game platform sold in that year, and do a dropdown menu interface