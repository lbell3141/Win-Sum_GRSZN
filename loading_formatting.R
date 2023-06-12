#Lindsey Bell
#6/9/2023
#Plotting growing season averages for winter and summer 2017-2022 for 8 sites

#loading libraries  
library(lubridate)
library(dplyr)
library(ggplot2)

#loading all files in at once
getwd()
directory <- "C:/Users/Asus PC/Documents/R_Projects/Win-Sum_GRSZN/Data"
file_names <- list.files(directory, pattern = ".csv", full.names = TRUE)
data_frames = list()

for(file in file_names){
  data <- read.csv(file, header = TRUE, na.strings= "-9999", skip=2, sep = ",")
  data_frames[[basename(file)]]<-data
}

#loading in data and manipulating time stamps  
for (df_name in names(data_frames)) {
  df <- data_frames[[df_name]]
  df$TIMESTAMP_START <- ymd_hm(as.character(df$TIMESTAMP_START))
  df$Day <- day(df$TIMESTAMP_START)
  df$Month <-month(df$TIMESTAMP_START)
  df$Year <- year(df$TIMESTAMP_START)
  data_frames[[df_name]] <- df
}

#condensing data frames to variables of interest 
dat_file <- data_frames$`AMF_US-CMW_BASE_HH_2-5.csv`
dat_CMW <- data.frame(
                      yr = dat_file$Year,
                      mon = dat_file$Month,
                      day = dat_file$Day,
                      swc = dat_file$SWC_1_6_1,
                      gpp = dat_file$GPP_PI,
                      nee = dat_file$NEE_PI,
                      temp_atmos = dat_file$TA_1_2_1,
                      temp_soil = dat_file$TS_1_3_1,
                      precip = dat_file$P,
                      ppfd_in = dat_file$PPFD_IN_PI_F
                      )

dat_file <- data_frames$`AMF_US-MtB_BASE_HH_4-5.csv`
dat_MtB <- data.frame(
  yr = dat_file$Year,
  mon = dat_file$Month,
  day = dat_file$Day,
  swc = dat_file$SWC_1_2_1,
  temp_atmos = dat_file$TA_1_2_1,
  temp_soil = dat_file$TS_1_1_1,
  ppfd_in = dat_file$PPFD_IN_PI_F
)

if(!"precip" %in% colnames(dat_MtB)) {
  dat_MtB$precip <- NA
  }

dat_file <- data_frames$`AMF_US-NR1_BASE_HH_19-5.csv`
dat_NR1 <- data.frame(
  yr = dat_file$Year,
  mon = dat_file$Month,
  day = dat_file$Day,
  swc = dat_file$SWC_PI_F_1,
  temp_atmos = dat_file$TA_1_1_1,
  temp_soil = dat_file$TS_PI_F_1,
  precip = dat_file$P_PI_F_1_1_1,
  ppfd_in = dat_file$PPFD_IN_PI_F
)

dat_file <- data_frames$`AMF_US-SRG_BASE_HH_14-5.csv`
  dat_SRG <- data.frame(
  yr = dat_file$Year,
  mon = dat_file$Month,
  day = dat_file$Day,
  swc = dat_file$SWC_1_2_1,
  temp_atmos = dat_file$TA_1_1_1,
  temp_soil = dat_file$TS_1_3_1,
  precip = dat_file$P,
  ppfd_in = dat_file$PPFD_IN_PI_F
)

dat_file <- data_frames$`AMF_US-SRM_BASE_HH_25-5.csv`
  dat_SRM <- data.frame(
    yr = dat_file$Year,
    mon = dat_file$Month,
    day = dat_file$Day,
    swc = dat_file$SWC_PI_1_1_A,
    temp_atmos = dat_file$TA_1_1_1,
    temp_soil = dat_file$TS_1_4_1,
    precip = dat_file$P,
    ppfd_in = dat_file$PPFD_IN_PI_F
  )
  
dat_file <- data_frames$`AMF_US-Whs_BASE_HH_20-5.csv`
  dat_Whs <- data.frame(
    yr = dat_file$Year,
    mon = dat_file$Month,
    day = dat_file$Day,
    swc = dat_file$SWC_1_1_1,
    temp_atmos = dat_file$TA_1_2_1,
    temp_soil = dat_file$TS_1_3_1,
    precip = dat_file$P,
    ppfd_in = dat_file$PPFD_IN_PI_F
  )
  
dat_file <- data_frames$`AMF_US-Wkg_BASE_HH_20-5.csv`
  dat_Wkg <- data.frame(
    yr = dat_file$Year,
    mon = dat_file$Month,
    day = dat_file$Day,
    swc = dat_file$SWC_1_1_1,
    temp_atmos = dat_file$TA_1_1_1,
    temp_soil = dat_file$TS_1_1_1,
    precip = dat_file$P,
    ppfd_in = dat_file$PPFD_IN_PI_F
  )

#rewriting condensed frames into one data frame
var_df <- list()  
var_df$dat_CMW <- dat_CMW
var_df$dat_MtB <- dat_MtB
var_df$dat_NR1 <- dat_NR1
var_df$dat_SRG <- dat_SRG
var_df$dat_SRM <- dat_SRM
var_df$dat_Whs <- dat_Whs
var_df$dat_Wkg <- dat_Wkg

#splitting into summer and winter growing seasons
sum_months <- c(7, 8, 9, 10)
win_months <- c(11, 12, 1, 2, 3, 4)

is_summer <- function(month) {
  month %in% sum_months
}
is_winter <- function(month) {
  month %in% win_months
}

summer_data <- lapply(data_frames, function(df) {
  filter(df, Month %in% sum_months)
})
winter_data <- lapply(data_frames, function(df) {
  filter(df, Month %in% win_months)
})

#finding z-scores for each variable using sum/win grszn data frames 
#Using multi-annual mean ("long-term") and annual means ("short-term")

#lt_means for sum and win
columns <- c("swc", "temp_atmos", "temp_soil", "precip", "ppfd_in")
multiannual_means <- data.frame()
for (i in seq_along(var_dat)) {
  dat_file
}
  dat_file_fr %>%
  group_by(yr) %>%
  summarise(across(all_of(columns), mean, na.rm = TRUE, .names = "st_mn_{.col}")) 














