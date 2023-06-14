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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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
dat_file <- dat_file[dat_file$Year %in% c(2009:2021),]

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

summer_data <- lapply(var_df, function(df) {
  filter(df, mon %in% sum_months)
})
winter_data <- lapply(var_df, function(df) {
  filter(df, mon %in% win_months)
})

#finding z-scores for each variable using sum/win grszn data frames 
#Using multi-annual mean ("long-term") and annual means ("short-term")

#multiannual means for sum and win
columns <- c("swc", "temp_atmos", "temp_soil", "precip", "ppfd_in")

multiannual_means_summer <- list()
multiannual_sd_summer <- list()
for (df_name in names(summer_data)) {
  df <- summer_data[[df_name]]
  means <- summarize_at(df, vars(all_of(columns)), mean, na.rm = TRUE, .name = "mean_{.col}")
  sd <- summarize_at(df, vars(all_of(columns)), ~sd(. , na.rm = TRUE), .name = "sd_{.col}")
  multiannual_means_summer[[df_name]] <- means
  multiannual_sd_summer[[df_name]] <- sd
}

multiannual_means_winter <- list()
multiannual_sd_winter <- list()
for (df_name in names(winter_data)) {
  df <- winter_data[[df_name]]
  means <- summarize_at(df, vars(all_of(columns)), mean, na.rm = TRUE, .name = "mean_{.col}")
  sd <- summarize_at(df, vars(all_of(columns)), ~sd(. , na.rm = TRUE), .name = "sd_{.col}")
  multiannual_means_winter[[df_name]] <- means
  multiannual_sd_winter[[df_name]] <- sd
  }

#annual means for sum and win
annual_means_summer <- list()
for (df_name in names(summer_data)) {
  df <- summer_data[[df_name]]
  means <- df %>%
  group_by(yr)%>%
    summarize(across(all_of(columns), mean, na.rm = TRUE))
  annual_means_summer[[df_name]] <- means  
}

annual_means_winter <- list()
for (df_name in names(winter_data)) {
  df <- winter_data[[df_name]]
  means <- df %>%
    group_by(yr)%>%
    summarize(across(all_of(columns), mean, na.rm = TRUE))
  annual_means_winter[[df_name]] <- means  
}

#calculating z-scores
annual_zscores <- list()
for (i in 1:7) {
  annual_means <- annual_means_summer[[i]]
  multiannual_means <- multiannual_means_summer[[i]]
  multiannual_sd <- multiannual_sd_summer[[i]]
  
  # removing year; leaving only variables of interest for zscores
  variables <- names(annual_means)[-1]
  
  # Calculate z-scores for each variable
  z_scores <- data.frame(year = annual_means$yr)
  for (var in variables) {
    annual_mean_var <- annual_means[[var]]
    multiannual_mean_var <- multiannual_means[[var]]
    multiannual_sd_var <- multiannual_sd[[var]]
    
    z_score <- (annual_mean_var - multiannual_mean_var) / multiannual_sd_var
    z_scores <- cbind(z_scores, z_score)
  }
  
  annual_zscores[[i]] <- z_scores
}



  

