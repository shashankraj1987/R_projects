library(dplyr)
library(lubridate)
library(plotly)
options(digits=2)

########################################################################################
# Importing the DataSet 

df_mdg <- read.csv("D:\\Users\\shash\\Documents\\Work\\Syngenta\\MDG\\Cloud_Dump\\MDG.csv")
df_hld_lst <- read.csv("D:\\Users\\shash\\Documents\\Work\\Syngenta\\MDG\\Cloud_Dump\\Holiday_List.csv") 

#df_mdg <- read.csv("C:\\Users\\s1055806\\OneDrive - Syngenta\\Documents\\Data\\MDG\\Cloud_Dump\\MDG.csv")
#df_hld_lst <- read.csv("C:\\Users\\s1055806\\OneDrive - Syngenta\\Documents\\Data\\MDG\\Cloud_Dump\\Holiday_List.csv")

weekend = c("Saturday", "Sunday")

########################################################################################


head(df_mdg)
head(df_hld_lst)

df_columns <- c("ï..Change.Request","Created.On","Created.At","Created.By","Changed.By","Edition",
                "Status","Type","Description","Changed.On","Changed.At","Finalized.On","Finalized.At","Finalized.By","Description.of.Edition")
df_mdg <- df_mdg[df_columns]
colnames(df_mdg)[1] <- "Change.Request"

df_dates <- c("Created.On","Created.At","Changed.On","Changed.At","Finalized.On","Finalized.At")

head(df_mdg)

colnames(df_hld_lst) <- c("Sr_No","Holiday")
df_hld_lst$Holiday <- mdy(df_hld_lst$Holiday)


########################################################################################
# Merging Date and Time into One Column 

df_mdg$Created_dt_time <- paste(df_mdg$Created.On,df_mdg$Created.At)
df_mdg$Changed_dt_time <- paste(df_mdg$Changed.On,df_mdg$Changed.At)
df_mdg$Finalized_dt_time <- paste(df_mdg$Finalized.On,df_mdg$Finalized.At)

df_columns <- c("Change.Request","Created_dt_time","Changed_dt_time","Finalized_dt_time","Created.By",
                "Status","Type","Description")
df_mdg_final <- df_mdg[df_columns]

head(df_mdg_final)

########################################################################################
# Converting Date and Time columns into string and extracting features  

mdy_hms(df_mdg_final$Created_dt_time) %>% head()

df_mdg_final$Created_dt_time <- mdy_hms(df_mdg_final$Created_dt_time)
df_mdg_final$Changed_dt_time <- mdy_hms(df_mdg_final$Changed_dt_time)
df_mdg_final$Finalized_dt_time <- mdy_hms(df_mdg_final$Finalized_dt_time)

# Adding Dummy Dates to Null Values of Finalized Date Time 

dummy_date <- ymd_hms("2000-01-01 00:00:00")
df_mdg_final$Finalized_dt_time[is.na(df_mdg_final$Finalized_dt_time)]  <- dummy_date

## Calculation 1
## Calculating the Time Difference in Seconds. 

df_mdg_final$TAT_E2E_MDG_Secs <- if_else(
  df_mdg_final$Finalized_dt_time == dummy_date,0,as.numeric(difftime(df_mdg_final$Changed_dt_time,df_mdg_final$Created_dt_time, 
                                                                     units = "secs")))

thshld <- 24*60*60

## Calculation 2
## Calculating the OLA Adherence Status  

df_mdg_final$TAT_E2E_MDG_STATUS <- if_else(df_mdg_final$TAT_E2E_MDG < thshld,"Within OLA Adherence","OLA Breach")

## Calculation 3
## Calculating the Aging Days 

df_mdg_final$TAT_E2E_MDG_Days <- if_else(df_mdg_final$TAT_E2E_MDG_Secs == 0, 0, df_mdg_final$TAT_E2E_MDG_Secs/60/60/24)

df_mdg_final$TAT_E2E_MDG_0_2 <- if_else(df_mdg_final$TAT_E2E_MDG_Days <= 2, 1, 0)
df_mdg_final$TAT_E2E_MDG_3_5 <- if_else(df_mdg_final$TAT_E2E_MDG_Days >= 2 & df_mdg_final$TAT_E2E_MDG_Days <= 5, 1, 0)
df_mdg_final$TAT_E2E_MDG_6_10 <- if_else(df_mdg_final$TAT_E2E_MDG_Days >= 5 & df_mdg_final$TAT_E2E_MDG_Days <= 10, 1, 0)
df_mdg_final$TAT_E2E_MDG_11_20 <- if_else(df_mdg_final$TAT_E2E_MDG_Days >= 10 & df_mdg_final$TAT_E2E_MDG_Days <= 20, 1, 0)
df_mdg_final$TAT_E2E_MDG_21_45 <- if_else(df_mdg_final$TAT_E2E_MDG_Days >= 20 & df_mdg_final$TAT_E2E_MDG_Days <= 45, 1, 0)

sum(df_mdg_final$TAT_E2E_MDG_0_2)
sum(df_mdg_final$TAT_E2E_MDG_3_5)
sum(df_mdg_final$TAT_E2E_MDG_6_10)
sum(df_mdg_final$TAT_E2E_MDG_11_20)
sum(df_mdg_final$TAT_E2E_MDG_21_45)

## Calculation 4
## Removing Saturday, Sundays and Holidays  

wday(df_mdg_final$Created_dt_time,label = TRUE, abbr = FALSE)
hour(df_mdg_final$Created_dt_time) %>% head(20)
df_mdg_final$Created_dt_time %>% head(20)

head(df_mdg_final)
head(df_hld_lst)

## Adding the Days for Created and finalized Time 
df_mdg_final$Created_day <- wday(df_mdg_final$Created_dt_time,label = TRUE,abbr = FALSE)
df_mdg_final$Finalized_day <- wday(df_mdg_final$Finalized_dt_time ,label = TRUE,abbr = FALSE)
df_hld_lst$Holiday_Day <- wday(df_hld_lst$Holiday,label = TRUE,abbr = FALSE)

# Removing Saturday and Sundays from the list 

df_mdg_final <- df_mdg_final %>% subset(Finalized_day != "Saturday" & Finalized_day != "Sunday")
df_mdg_final <- df_mdg_final %>% subset(Created_day != "Saturday" & Created_day != "Sunday")

nrow(df_mdg_final)

# Removing the holidays from the list 
df_mdg_final <- df_mdg_final %>% 
      subset(!as.Date(Created_dt_time) %in% df_hld_lst$Holiday) %>% 
      subset(!as.Date(Finalized_dt_time) %in% df_hld_lst$Holiday) 

df_mdg_final %>% View("Without_Holidays")

# df_mdg_final[,c("Finalized_dt_time","Created.By","Finalized_day")] %>% 
#  subset(!as.Date(Finalized_dt_time) %in% df_hld_lst$Holiday) %>% 
#  arrange(as.Date(Finalized_dt_time)) %>% count()

#################################################################################################
######### Converting output to Excel Format 
#################################################################################################

df_otpt <- df_mdg_final
df_otpt$Created_dt_time <- as.Date(df_otpt$Created_dt_time)
df_otpt$Changed_dt_time <- as.Date(df_otpt$Changed_dt_time)
df_otpt$Finalized_dt_time <- as.Date(df_otpt$Finalized_dt_time)

#write.csv(df_otpt,file = "D:\\Users\\shash\\Documents\\Work\\Syngenta\\MDG\\Cloud_Dump\\MDG_final.csv")
write.csv(df_mdg_final,file = "D:\\Users\\shash\\Documents\\Work\\Syngenta\\MDG\\Cloud_Dump\\MDG_final.csv")
#################################################################################################

head(df_mdg_final)













########################################################################################

df_test <- df_mdg_final[,c("Created_dt_time","Changed_dt_time","Finalized_dt_time")]

head(df_test,20)

df_test$Finalized_dt_time[is.na(df_test$Finalized_dt_time)] <- as.Date("2000-01-01 12:00:00")

#df_test$Time_Diff <- difftime(df_mdg_final$Changed_dt_time,df_mdg_final$Created_dt_time, units = "mins")

sm_dt <- head(df_test,20)

sm_dt$Time_Diff_secs <- if_else(sm_dt$Finalized_dt_time == dummy_date,0,as.numeric(difftime(sm_dt$Changed_dt_time,sm_dt$Created_dt_time, units = "secs")))

sm_dt$Time_Diff_secs <- as.duration(sm_dt$Time_Diff_secs)
as.duration(mean(sm_dt$Time_Diff_secs))

sm_dt$Time_Diff_secs <- as.period(sm_dt$Time_Diff_secs)
as.duration(mean(sm_dt$Time_Diff_secs))

sm_dt$Time_Diff_secs > minute(20)

###########################################

st_time <- mdy_hms("12/21/2020 18:57:28")
end_time <- mdy_hms("12/24/2020 12:17:07")

total_efforts <- difftime(end_time,st_time,units = "hours")

as.period(total_efforts)
as.duration(total_efforts)

d1_shift_strt = ymd_hms(paste(as.Date(st_time) + dhours(9)))
d1_shift_end = ymd_hms(paste(as.Date(st_time) + dhours(18)))

d2_shift_strt = ymd_hms(paste(as.Date(end_time) + dhours(9)))
d2_shift_end = ymd_hms(paste(as.Date(end_time) + dhours(18)))

shift_start_time - st_time

no_of_days_worked <- day(as.period(total_efforts))
no_of_hours_worked <- hour(as.period(total_efforts))

## was the ticket created after shift end time?
d1_shift_end > st_time

if(no_of_hours_worked > 0 ){
  total_shift_hours_worked = no_of_days_worked * hours(9)
  
}

###########################################