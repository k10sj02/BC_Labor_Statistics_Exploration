#Load packages for importing, cleansing and analysis
library(readr)
library(tidyverse)
library(dplyr)
##install.packages("lubridate")
library(lubridate)

#Load dataset 
lmic_dataset <- read.csv("example_data_bia.csv")

#View dataset
view(lmic_dataset)
str(lmic_dataset)
summary(lmic_dataset)

#We have 18,480 different observations across four
#variables. The experimental unit here is month.


#Check that variables are coded without duplicates
lmic_dataset$naics
str_sort(unique(lmic_dataset$month))
str_sort(unique(lmic_dataset$naics))
str_sort(unique(lmic_dataset$geo))

#Tidy sector names 

lmic_renamed <- lmic_dataset %>%
  mutate(naics = recode(naics, 
                        "Accommodation_and_food_services_[72]" =
                          "Accommodation and food services [72]",
                        "Wholesale_and_retail_trade_[41,_44-45]" = 
                          "Wholesale and retail trade [41, 44-45]",
                        "Other_services_(except_public_administration)_[81]" = 
                        "Other services (except public administration) [81]"
                        )
        )

str_sort(unique(lmic_renamed$naics)) #double check for misspelled values
view(lmic_renamed)

#assign unique IDs to values in lmic_renamed dataset

lmic_renamed$unique_id <-paste0(as.character(lmic_renamed$month), 
                                as.character(lmic_renamed$naics),
                                as.character(lmic_renamed$geo)
                                )

#reorder columns so that unique ID is the starting column

lmic_renamed <- lmic_renamed[, c(5, 1, 2, 3, 4)]

#find all missing values in lmic_renamed

lmic_renamed[is.na(lmic_renamed$employment_1000s),]

##All missing values for relevant months are from 20-Mar values in BC.

#import statscan dataset: named laborstatsBC

laborstatsBC <- read.csv("missing_BC_data.csv", skip=11)
laborstatsBC <- laborstatsBC[-1,]

#Creating tidy data from imported dataset: rename columns, 
#recode categorical variables, filter rows
  
laborstatsBC_new <- laborstatsBC %>%
  rename("naics"="North.American.Industry.Classification.System..NAICS..5",
         "employment_1000s"="Mar.20")%>%                                    
  #filter(laborstatsBC, naics != "Total employed, all industries 6") %>% 
  mutate(naics=case_when(naics=="Wholesale and retail trade"~"Wholesale and retail trade [41, 44-45]",
                         naics=="Accommodation and food services"~"Accommodation and food services [72]",
                         naics=="Other services (except public administration)" ~ "Other services (except public administration) [81]",
                         TRUE~naics))%>%
  select(naics,employment_1000s)%>%
  mutate(month="Mar-20",
         geo="British Columbia")

#remove rows
laborstatsBC_new <- laborstatsBC_new[2:19,]

#merge datasets using left join and coalesce 

lmic_renamed_new <- lmic_renamed%>% left_join(laborstatsBC_new, by=c("naics","month","geo"))%>%
  mutate(employment_1000s.y=as.numeric(employment_1000s.y),
    employment_1000s=coalesce(employment_1000s.x,employment_1000s.y))%>%      #remove duplicated columns created by leftjoin
  select(-employment_1000s.x,-employment_1000s.y)

#summary

summary(lmic_renamed_new)

#boxplot to visualize data and spot anomalies

lmic_renamed_new%>%
  filter(geo!="Canada")%>%
ggplot(aes(x=employment_1000s))+
  geom_boxplot()+
  labs(title="title here")+
  coord_flip()+
  scale_x_log10()

#percent change in employment by province
lmic_renamed_new %>%
  group_by(geo,month,naics) %>%
  mutate(month=dmy(paste0("01",month)))%>%
  summarise_at(vars(employment_1000s), list(mean = mean))%>%
  ungroup()%>%
  filter(month>="2020-01-01",month<="2020-09-01",
         naics %in% c("Accommodation and food services [72]",
                      "Wholesale and retail trade [41, 44-45]", 
                      "Other services (except public administration) [81]"),
         geo!="Canada")%>%
  mutate(naics=case_when(naics=="Accommodation and food services [72]"~"Accommodation and food services",
                         naics=="Wholesale and retail trade [41, 44-45]"~"Wholesale and retail trade",
                         naics=="Other services (except public administration) [81]"~"Other services (except public administration)",
                         TRUE~naics))%>%
  group_by(geo,naics)%>%
  mutate(change=(mean-lag(mean))/lag(mean)*100)%>% #calculate change in employment
  ggplot(aes(x=month,y=change,fill=naics))+
  geom_col(position="dodge")+
  theme(legend.position = "bottom") +
  facet_wrap(~geo,scales="free_y", nrow = 2)+
  labs(title="Monthly change in employment across Canadian provinces",
       x="Months",
       y="Percent Change in Employment",
       fill="Sectors") + 
      scale_x_date(labels=date_format("%b"),breaks = "1 month")

ggsave("Monthly_per_cent_change_in_employment_across_Canadian_provinces.png", width = 50, height = 30, units = "cm")
