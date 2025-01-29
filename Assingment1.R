

# Global temprature data from lower troposphere, mid troposphere, tropopause and lower stratosphere



rm(list=ls())

library(tidyverse)
library(zoo)

df_lowertropo <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
df_midtropo <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
df_tropopause <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
df_lowerstrato <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")




#Cleaning up the data

df_lowertropo <- df_lowertropo %>%
  select(Year, Mo, Globe, Land, Ocean) %>% 
  mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>%
  select(Year, Month, Date, Globe, Land, Ocean) %>%  
  mutate_if(is.character, as.numeric) 
  
  
 df_midtropo <- df_midtropo %>% 
   select(Year, Mo, Globe, Land, Ocean) %>% 
   mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
   mutate(Year = year(Date), 
          Month = month(Date)) %>%
   select(Year, Month, Date, Globe, Land, Ocean) %>%  
   mutate_if(is.character, as.numeric) 
  
   
 df_tropopause <- df_tropopause %>% 
   select(Year, Mo, Globe, Land, Ocean) %>% 
   mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
   mutate(Year = year(Date), 
          Month = month(Date)) %>%
   select(Year, Month, Date, Globe, Land, Ocean) %>%  
   mutate_if(is.character, as.numeric) 
  
 
 df_lowerstrato <- df_lowerstrato %>%
   select(Year, Mo, Globe, Land, Ocean) %>% 
   mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
   mutate(Year = year(Date), 
          Month = month(Date)) %>%
   select(Year, Month, Date, Globe, Land, Ocean) %>%  
   mutate_if(is.character, as.numeric) 
   

# Adding location to the datasets
df_lowertropo$Location <- "Lower Troposphere"
df_midtropo$Location <- "Mid Troposphere"
df_tropopause$Location <- "Tropopause"
df_lowerstrato$Location <- "Lower Stratosphere"

   
#Combining the data
 
df_all <- bind_rows(df_lowertropo, df_midtropo, df_tropopause, df_lowerstrato)

#Filtering data from 1980 onwards

df_all <- df_all %>% filter(Year >= 1980)

#Calculating 12-month(right-aligned) moving average

df_all <- df_all %>%
  group_by(Location) %>%
  arrange(Date) %>%
  mutate(MA_12 = rollmean(Globe, 12, align = "right", fill = NA)) %>%
  ungroup()

df_avg <- df_all %>%
  filter(!is.na(MA_12)) %>%
  group_by(Date) %>%
  summarise(Average = mean(MA_12, na.rm = TRUE))

#Plotting the four atomosphere locations and their avreage 

ggplot(df_all, aes(x = Date, y = MA_12, color = Location)) +
  geom_line(size = 1) + 
  geom_line(data = df_avg, aes(x = Date, y = Average), 
            color = "black", size = 1.5, linetype = "dashed") +  
  labs(
    title = "12-month moving average of temperature in four atmospheric levels",
    subtitle = "The black dashed line represents the overall average",
    x = "Year",
    y = "Temperature anomaly (°C)",
    color = "Atmospheric level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),  
    legend.position = "right" 
  )




