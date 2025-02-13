#Assignment 2

rm(list=ls())
 
library(tidyverse)
library(zoo)
library(readr)
library(dplyr)
library(lubridate)

url <- "https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv"
data <- read_csv(url)


#Task 1

#1: For the last 3 months of 2017, calculate the total Sales by month, for Region 1 and Region 9 in the Customer_Segment, Corporate, and Consumer. This output is Table 1.

#Total sales by month for region 1 and 9 in last 3 months of 2017. In segment, Customer, Corporate and Consumer

data <- data %>%
  mutate(Order_Date = as.Date(Order_Date, format = "%Y-%m-%d"),
         Year = year(Order_Date),      
         Month = floor_date(Order_Date, "month")) 

Tabell1 <- data %>%
  filter(Region %in% c("Region 1", "Region 9"),  
         Customer_Segment %in% c("Corporate", "Consumer"),  
         Order_Date >= as.Date("2017-10-01") & Order_Date <= as.Date("2017-12-31")) %>%
  group_by(month = floor_date(Order_Date, "month")) %>%  
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) 

#2: Make a plot of the monthly total Sales in Region 1 and Region 13 in 2015, 2016, and 2017. This output is Figure 1.


#Filtering out the data and making the plot

Figure1 <- data %>%
  filter(Region %in% c("Region 1", "Region 13"),
         Year %in% c(2015, 2016, 2017)) %>%
  group_by(Month, Region) %>%  # Gruppér etter måned og region
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop")


ggplot(Figure1, aes(x = Month, y = Total_Sales, color = Region, group = Region)) +
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  labs(title = "Månedlig Total Sales for Region 1 og 13 (2015-2017)",
       x = "Måned",
       y = "Total Sales") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") 

 #3 In Figure 1, identify the months where the total Sales in Region 13 is greater than the total Sales in Region 1. This output is Table 2.

sales_comparison <- Figure1 %>%
  pivot_wider(names_from = Region, values_from = Total_Sales, names_prefix = "Sales_")

Table2 <- sales_comparison %>%
  filter(`Sales_Region 13` > `Sales_Region 1`) %>%
  select(Month, `Sales_Region 1`, `Sales_Region 13`)

#4 Find the average Profit per Customer_Segment and Product_Category in 2017, for all regions except Region 3, 5 and 8. What segment produced the highest average profit? This output is Table 3.


filtered_data <- data %>%
  filter(Year == 2017, 
         !Region %in% c("Region 3", "Region 5", "Region 8"))

Table3 <- filtered_data %>%
  group_by(Customer_Segment, Product_Category) %>%
  summarise(Average_Profit = mean(Profit, na.rm = TRUE), .groups = "drop")

highest_avg_profit <- Table3 %>%
  filter(Average_Profit == max(Average_Profit))

print(highest_avg_profit)
#This shows that the customer segemtent small businesses under product category technology had the highest profit of 544


#Assignment 2
#Task 2

library(rvest)

url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"
webpage <- read_html(url)
df <- html_table(html_nodes(webpage, "div table")[[1]])

#Cleaning up the dataset

colnames(df) <- c("Modell", "WLTP", "STOPP", "Avvik")
df <- df %>%
  mutate(
    WLTP = as.numeric(str_extract(WLTP, "\\d+")),   # Behold kun tall
    STOPP = as.numeric(str_extract(STOPP, "\\d+"))  # Behold kun tall
  )

#A 

ggplot(df, aes(x = WLTP, y = STOPP)) +
  geom_point(color = "blue", size = 3) +  
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +  
  labs(
    title = "Forventet vs. Faktisk Rekkevidde for Elbiler",
    x = "WLTP-rekkevidde (km)",
    y = "STOPP-rekkevidde (km)",
    caption = "Den røde linjen viser en 1:1-forhold, der bilene faktisk kjører like langt som oppgitt WLTP."
  ) +
  theme_minimal()

#B
model <- lm(STOPP ~ WLTP, data = df)
summary(model)

ggplot(df, aes(x = WLTP, y = STOPP)) +
  geom_point(color = "blue", size = 3) +  # Scatterplot
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +  
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Regresjonslinje
  labs(
    title = "Forventet vs. Faktisk Rekkevidde med Regresjon",
    x = "WLTP-rekkevidde (km)",
    y = "STOPP-rekkevidde (km)",
    caption = "Den røde linjen viser en perfekt 1:1-sammenheng. Den svarte linjen viser faktisk trend basert på regresjon."
  ) +
  theme_minimal()

#Interpretation of the Regression Line

#If the black regression line has a slope lower than 1, it indicates that, on average, cars achieve less real-world range than their advertised WLTP range.
#The more the regression line deviates from the red 45-degree line, the greater the discrepancy between the promised and actual driving range.
#The intercept (y-axis crossing point) may indicate a systematic bias in the WLTP calculations, suggesting that even at lower advertised ranges, real-world performance is consistently lower.



