###### LOCATION AND ACTIVITY SCRIPT ####### Written by Lauren Ferguson (2023) updated by Duncan Grassie (2025) 
###### to apply to microenvironments rather than activities, allow different groups 

rm(list = ls())
##### SET UP #######
library(dplyr)
library(gridExtra)
library(ggpubr)
library(gtable)
library(readr)
library(plyr) 
library(tidyr)
library(RColorBrewer)
library(tidyverse)

## Settings
domestic_breakdown <- TRUE

## import data and remove NAs
BaseWD <- "R:/Projects & research/INHABIT/Code Sharing folder/EXPAIR v5" #dgedit
inpath <- paste0(BaseWD, "/inputdata/timeactivity/") #dgedit
outpath <-paste0(BaseWD, "/intermediate/activity_profiles") #dgedit

## read in location and activity details for all participants
setwd(inpath)
data <- read_csv("loc_and_act.csv")
data <- na.omit(data)
head(data)

## read in definitions of age groups
pop_groups <- read.csv("pop_groups_age_ranges.csv", header=TRUE, sep=",", colClasses=c("NULL", NA, NA,NA))

## read in details for all participants and join with locations
individual_data <-read_csv("individuals.csv")
#individual_data <- na.omit(individual_data)
individual_data_selected <- individual_data %>%
  mutate(across(tail(names(.), 16), ~replace(., is.na(.), -1))) %>%
  select(serial, pnum, tail(names(individual_data), 16))
data <- data %>%
  left_join(individual_data_selected, by = c("serial","pnum"))

## read in time intervals
lookup_file <- read.csv("time_lookupfile.csv", header=TRUE, sep=",", colClasses=c("NULL", NA, NA))

#### set parameters ##### 
activity_labels_MEsonly <- c("Home", "Office", "School",  "Commercial", "Hospitality", "Car", "Bus", "Transport-other", "Out") #This also updates order which they appear in plots and must be updated as a,b,c,d, etc below
activity_labels_dom <- c("Home-Bedroom", "Home-Living room", "Home-Kitchen", "Home-Other","Office", "School",  "Commercial", "Hospitality", "Car", "Bus", "Transport-other", "Out") #This also updates order which they appear in plots and must be updated as a,b,c,d, etc below

if (domestic_breakdown == TRUE){
  activity_labels <- activity_labels_dom
} else {
  activity_labels <- activity_labels_MEsonly
}
activity_labels <- as.data.frame(activity_labels)
activity_labels_vec <- rev(activity_labels$activity_labels)

setwd(outpath)

## Define functions
## 1. replaces for timesteps 1-144, locations and activities with microenv
loc_cal_MEsonly <- function(df) {
  df <- df %>%
    mutate(new_144 = case_when(
      #Added by DG to give necessary categories
      wher_144 == "Home" ~ "Home",
      wher_144 == "Out" ~ "Out",
      wher_144 == "Active travel - outdoors" ~ "Out",
      wher_144 == "Car" ~ "Car",
      wher_144 == "Bus" ~ "Bus",
      wher_144 == "Transport-other" ~ "Transport-other",
      #wher_144 == "Train" ~ "Train",
      #wher_144 == "Tram/underground" ~ "Tram/underground",
      wher_144 == "Commercial" ~ "Commercial",
      wher_144 == "Hospitality" ~ "Hospitality",
      act1_144 == "Sleeping" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Study" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Housework" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Sedentary" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Eating/cooking" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Personal care" & wher_144 == "Not-app" ~ "Home",
      act1_144 == "Travel" & wher_144 == "Not-app" ~ "Transport-other",
      act1_144 == "Work" & wher_144 == "Not-app" ~ "Office",
      act1_144 == "School" & wher_144 == "Not-app" ~ "School",
      act1_144 == "Outdoor" & wher_144 == "Not-app" ~ "Out",
      act1_144 == "Commercial" & wher_144 == "Not-app" ~ "Commercial",
      act1_144 == "Hospitality" & wher_144 == "Not-app" ~ "Hospitality",
      wher_144 == "Not-app" ~ "Not-app", 
      wher_144 == "Office/school" & DVAge < 19 ~ "School",
      wher_144 == "Office/school" & DVAge > 18 ~ "Office"))
  
  oldnames <- c("serial", "pnum", "daynum", "DVAge", "new_144")
  newnames <- c("serial", "pnum", "daynum", "DVAge", paste0("loc_", i)) 
  
  df <- df %>% select_("serial", "pnum", "daynum", "DVAge", "new_144") %>% rename_at(vars(oldnames), ~ newnames)
  
  return(df)
}

loc_cal_dom <- function(df) {
  df <- df %>%
    mutate(new_144 = case_when(
      #Added by DG to give necessary categories
      wher_144 == "Out" ~ "Out",
      wher_144 == "Active travel - outdoors" ~ "Out",
      wher_144 == "Car" ~ "Car",
      wher_144 == "Bus" ~ "Bus",
      wher_144 == "Transport-other" ~ "Transport-other",
      #wher_144 == "Train" ~ "Train",
      #wher_144 == "Tram/underground" ~ "Tram/underground",
      wher_144 == "Commercial" ~ "Commercial",
      wher_144 == "Hospitality" ~ "Hospitality",
      
      act1_144 == "Sleeping" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Bedroom",
      act1_144 == "Study" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Bedroom",
      act1_144 == "Housework" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Living room",
      act1_144 == "Sedentary" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Living room",
      act1_144 == "Eating/cooking" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Kitchen",
      act1_144 == "Personal care" & (wher_144 == "Not-app" | wher_144 == "Home") ~ "Home-Bedroom",
      wher_144 == "Home" ~ "Not-app", #dgedit
      
      act1_144 == "Travel" & wher_144 == "Not-app" ~ "Transport-other",
      act1_144 == "Work" & wher_144 == "Not-app" ~ "Office",
      act1_144 == "Office/school" & wher_144 == "Not-app" & DVAge < 19 ~ "School",
      act1_144 == "Office/school" & wher_144 == "Not-app" & DVAge > 18 ~ "Office",
      act1_144 == "Outdoor" & wher_144 == "Not-app" ~ "Out",
      act1_144 == "Commercial" & wher_144 == "Not-app" ~ "Commercial",
      act1_144 == "Hospitality" & wher_144 == "Not-app" ~ "Hospitality",
      wher_144 == "Not-app" ~ "Not-app",
      
      wher_144 == "Office/school" & DVAge < 19 ~ "School",
      wher_144 == "Office/school" & DVAge > 18 ~ "Office"))
  
  oldnames <- c("serial", "pnum", "daynum", "DVAge", "new_144")
  newnames <- c("serial", "pnum", "daynum", "DVAge", paste0("loc_", i)) 
  
  df <- df %>% select_("serial", "pnum", "daynum", "DVAge", "new_144") %>% rename_at(vars(oldnames), ~ newnames)
  
  return(df)
}

## 2. produces a dataframe with proportions of time spent in each microenvironment
prop_calc_MEsonly <- function(input) {
  adjusted_length <- length(input)- sum(input == "Not-app")
  a <- sum(input == "Home") / adjusted_length
  b <- sum(input == "Office") / adjusted_length
  c <- sum(input == "School") / adjusted_length
  d <- sum(input == "Commercial") / adjusted_length
  e <- sum(input == "Hospitality") / adjusted_length
  f <- sum(input == "Car") / adjusted_length
  g <- sum(input == "Bus") / adjusted_length
  h <- sum(input == "Transport-other") / adjusted_length
  i <- sum(input == "Out") / adjusted_length
  
  prop <- c(a, b, c, d, e, f, g, h, i) ## note  that these always need to be in the same order as the activity labels above
  prop <- format(round(prop, 3), nsmall = 3) #round to 3 decimal places
  
  output <- as.numeric(prop)
  
  return(output)
  
} 

prop_calc_dom <- function(input) {
  adjusted_length <- length(input)- sum(input == "Not-app")
  a <- sum(input == "Home-Bedroom") / adjusted_length
  b <- sum(input == "Home-Living room") / adjusted_length
  c <- sum(input == "Home-Kitchen") / adjusted_length
  d <- sum(input == "Home-Other") / adjusted_length
  e <- sum(input == "Office") / adjusted_length
  f <- sum(input == "School") / adjusted_length
  g <- sum(input == "Commercial") / adjusted_length
  h <- sum(input == "Hospitality") / adjusted_length
  i <- sum(input == "Car") / adjusted_length
  j <- sum(input == "Bus") / adjusted_length
  k <- sum(input == "Transport-other") / adjusted_length
  l <- sum(input == "Out") / adjusted_length
  
  prop <- c(a, b, c, d, e, f, g, h, i, j, k,l) ## note  that these always need to be in the same order as the activity labels above
  prop <- format(round(prop, 3), nsmall = 3) #round to 3 decimal places
  
  output <- as.numeric(prop)
  
  return(output)
  
}
# 3.calculates the proportion of the population doing each activity for all 144 time intervals and reorders df
clean_survey_data <- function(new, domestic_breakdown) {

  if (domestic_breakdown == TRUE){
    prop <- as.data.frame(apply(new[48:191], 2, prop_calc_dom))
    prop <- cbind(activity_labels, prop)
  } else {
    prop <- as.data.frame(apply(new[48:191], 2, prop_calc_MEsonly))
    prop <- cbind(activity_labels, prop)
  }  
  
  long <- gather(prop, Time, Proportion, loc_1:loc_144, factor_key=TRUE)
  long$Time = lookup_file$times[match((long$Time), lookup_file$wide.Time)]
  long$Time <- trimws(long$Time)
  long$Time[long$Time == "24:00:00"] <- "00:00:00"
  long <- long[order(long$Time), ]

  long$activity_labels <- factor(long$activity_labels, levels = activity_labels_vec)
  #  cols_to_reorder <- c("Office","Home")
  #  cols <-cols[!cols %in% cols_to_reorder]
  #  new_order <- c(cols,cols_to_reorder)
  #  wide <- wide[,new_order]  
  wide <- spread(long, activity_labels, Proportion)
  
  return(list(wide,long))
}

## categorise location data - these can be modified if interested in nondomestic environments 
data <- data %>%
  mutate_at(.vars = vars(contains("wher_")), #dgedit Change to "wher_" for 2015, "loc" for 2023
            .funs = funs(case_when(as.character(.) %in% c(11, 12, 14) ~ "Home",
                                   as.character(.) %in% c(13) ~ "Office/school", 
                                   as.character(.) %in% c(15) ~ "Hospitality",
                                   as.character(.) %in% c(16, 17, 19) ~ "Commercial", 
                                   as.character(.) %in% c(18, 20) ~ "Out", 
                                   as.character(.) %in% c(30, 34, 35, 36, 37, 38, 39, 41, 47, 90) ~ "Car",
                                   as.character(.) %in% c(40, 42, 49) ~ "Bus",
                                   as.character(.) %in% c(43,44,45,46) ~ "Transport-other", 
                                   #as.character(.) %in% c(44) ~ "Train", #Outside
                                   as.character(.) %in% c(31, 32, 33, 48) ~ "Active travel - outdoors",  
                                   as.character(.) %in% c(99,-9,-7,-2,0,10,21) ~ "Not-app",
                                   TRUE ~ as.character(.))))


## categorise activity data
data <- data %>%
  mutate_at(.vars = vars(contains("act1_")),
            .funs = funs(case_when(as.character(.) %in% c(110, 111, 120, 5310) ~ "Sleeping",
                                   as.character(.) %in% c(8120, 2120, 8100, 8110, 8120, 8190, 2210, 
                                                          8000, 7220) ~ "Study",
                                   as.character(.) %in% c(3000, 3210,  3220, 3230,  3240,  3290, 3320,  3300,  3390,  3330, 3420,  
                                                          3430,  3510,  3520, 3710,  3713,  3720,  3721, 3722, 3724,  3725,  
                                                          3726,  3727,   3729,  3800,  3810,  3819, 3820,  3830,  3890,  
                                                          3919, 3910,  3911,  3920,  3921, 3929,  4271, 4273, 4275,  4277,  4279) ~ "Housework",
                                   as.character(.) %in% c(5110,  5120,  5140,  7231, 7239,  7241,  7240,  7249, 7250,  
                                                          7251,  7259,  7300, 7310,  7320,  7322,  7330, 8210,  8211,  
                                                          8212,  8219, 8220,  8221, 8222,  8229, 9941, 9950,  8320, 
                                                          7000, 7329, 4272, 7390, 7230, 7140) ~ "Sedentary",
                                   as.character(.) %in% c(210, 3100, 3110, 3130, 3140, 3190, 3200, 3230, 3250, 3310,8300,
                                                          8310, 8311, 8312,8319, 3811) ~ "Eating/cooking",
                                   as.character(.) %in% c(0, 300, 310, 390) ~ "Personal care",
                                   as.character(.) %in% c(9000,  9010, 9100,  9110, 9120, 9130,  9210,   
                                                          9230,  9310, 9360,  9370, 9380, 9390,  9400,   
                                                          9410,  9420, 9430,  9440, 9500, 9510,  9520,   
                                                          9600,  9610, 9620,  9630, 9710, 9720,  9800,   
                                                          9810,  9820, 9890,  9940) ~ "Travel",  
                                   as.character(.) %in% c( 1100,  1110,  1120,  1000, 1210,  1220, 1300, 
                                                           1310,  1390,   1391,  1399, 4000,  4100, 4110,  
                                                           4120,  4190,  4310, 4260, 7170) ~ "Office/school",
                                   as.character(.) %in% c(2000, 2100, 2110, 2190) ~ "Office/school", 
                                   as.character(.) %in% c(6000,  6100, 6140, 6141, 6142,   
                                                          6149, 6150, 6160, 6170, 6171, 6179, 6190, 
                                                          6200, 6290, 6310, 6311, 6312, 7100, 7110, 
                                                          7111, 7119, 7121, 7120, 7129, 7130, 7112, 
                                                          7150, 5200, 5210, 5220, 5221, 5222, 5223, 
                                                          5224, 5225, 5229, 5230, 5240, 5241, 5242, 
                                                          5243, 5244, 5245, 5249, 5250, 5290, 5291, 
                                                          5292, 5293, 5294, 5295, 5299, 7190, 7321, 
                                                          3531) ~ "Outdoor",
                                   as.character(.) %in% c(3600, 3610, 3611, 3612,  3613, 3614, 3615, 
                                                          3619,  3620, 3630,  3690) ~ "Commercial",
                                   as.character(.) %in% c(4250, 5190, 5130, 5000, 5100, 5200, 7340) ~ "Hospitality",
                                   as.character(.) %in% c(3530, 3410, 3440, 3540, 3590, 4390, 4210, 4220, 4290, 
                                                          3914, 3500, 3539, 3840, 4200, 4230, 4240, 4283, 4300, 4274,
                                                          4289, 4270, 4281, 3490, 4282, 4280, 3924, 7160, 4278,  
                                                          4320, 6110, 6111, 6119, 6120, 6131, 6130, 6132, 6143, 
                                                          6144, 6210, 6220) ~ "Other", #miscellaneous
                                   as.character(.) %in% c(-1, 9980, 9990, 9999, 9960, 9970) ~ "Not-app",
                                   TRUE ~ as.character(.))))


## create new column ("loc_") estimating time_activity/loc from both activity and location columns categorised above 
# this may take a few minutes to run
for (i in 1:144) {
  act_col <- paste0("act1_", i)
  loc_col <- paste0("wher_", i)
  
  oldnames <- c("serial", "pnum", "daynum", "DVAge", act_col, loc_col)
  newnames <- c("serial", "pnum", "daynum", "DVAge", "act1_144", "wher_144")
  
  new_df <- data %>% select_("serial", "pnum", "daynum", "DVAge", "act_col", "loc_col") %>% rename_at(vars(oldnames), ~ newnames)

  if (domestic_breakdown == TRUE){
    location_column <-loc_cal_dom(df=new_df)
    data <- data %>% left_join(location_column, by=c("serial", "pnum", "daynum", "DVAge"))
  } else {
    location_column <- loc_cal_MEsonly(df=new_df)
    data <- data %>% left_join(location_column, by=c("serial", "pnum", "daynum", "DVAge"))
  }
}

# subset the dataframe to just include the newly categorised column for all 144 time intervals
data2 <- subset(data[, c(1:31, 320:479)]) ### subset to just include data for new columns and identifier uncategorised variables
####

for (i in 1:nrow(pop_groups)) {
  name <- pop_groups$Group_Name[i]
  cat("Processing:", name, "\n")
  lower <- pop_groups$Lower_Age[i]
  upper <- pop_groups$Upper_Age[i]
  
  subset_df <- subset(data2, DVAge >= lower & DVAge <= upper)
  assign(paste0("data_", name), subset_df)

  age_label <- paste0(lower,"-",upper)
  graph_title <- paste0("Time-activity patterns of the ",age_label," age group")
  act_loc_file_name <- paste0("activity_location_",age_label, "_years.csv")
  time_act_file_name <- paste0("EXPAIR_time_input_",age_label, "_years.csv")
  pdf_file_name <- paste0("p2015_",age_label,"_years.pdf")
    
  pop_groups$Number_Entries[i] <- nrow(subset_df)
  pop_groups$pdf_Filename[i] <- pdf_file_name
  pop_groups$age_label[i] <- age_label

#data_primary <-data2[data2$DVAge<11,] #Added to isolate school aged population group
#data_secondary <- subset(data2, DVAge<19 & DVAge>10)
#data_adult <- subset(data2, DVAge<51 & DVAge>18)
#data_middle <- subset(data2, DVAge<65 & DVAge>50)
#data_older <- subset(data2, DVAge<81 & DVAge>64)
#data_elderly <- data2[data2$DVAge>80,]
#data_worker <- subset(data_adult, dilodefr==1)
#data_unemployed <- subset(data_adult, dilodefr==2 | dilodefr==3)

#  write_csv(subset_df, act_loc_file_name) #dgedit turn back on for diagnoses
  result <- clean_survey_data(subset_df, domestic_breakdown) 
  daily_profiles <- result[[1]]
  long <-result[[2]]
  write_csv(daily_profiles, time_act_file_name)
  
  # Create plot
  timez <- c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00",
             "18:00", "20:00", "22:00")
  
  long$activity_labels <- factor(long$activity_labels) 
  n_levels <- length(levels(long$activity_labels))

  activity_colors <- colorRampPalette(RColorBrewer::brewer.pal(11, "Paired"))(n_levels)
  used_levels <- levels(long$activity_labels)[tapply(long$Proportion,
                                                      long$activity_labels, function(x) sum(x) > 0)]
  # Compute counts per level for the current plot in order to filter out unused in legend guides line
#  counts <- table(long$activity_labels[long$Proportion > 0])
#  alpha_values <- ifelse(counts == 0, 0, 1)
  
  p2015 <-  ggplot(long, aes(fill=activity_labels, y=Proportion, x=Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = activity_colors,
                      breaks = used_levels,   # Only show used levels in legend
                      drop = FALSE) +         # Keep color mapping consistent
    labs(title = graph_title,
         y="Proportion of survey population") +
    labs(fill = "Activity") +
    theme(text = element_text(size = 10)) +
    theme(legend.title = element_text(size = 11),
          legend.text = element_text(size = 9.5)) +
    scale_x_discrete("Hour of day", breaks=c("00:00:00", "02:00:00", "04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", 
                                             "18:00:00", "20:00:00", "22:00:00"), labels=timez) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0))# + #ADDed by copilot
    #guides(fill = guide_legend(override.aes = list(alpha = alpha_values))) # optional transparency for zero
  ggsave(pdf_file_name,units="cm",width = 16, height = 9,plot=p2015)
}
#write_csv(pop_groups,"Population Groups.csv") #dgedit turn back on for diagnoses

#### these daily patterns can be subset by weekend/weekday/season to see how activity patterns vary by type of day/season
### this is done by subseting variables in the dataset "data2"
### they can show variations by age by subsetting data2 using the 'DVAge' variable - it's also possible to merge the dataframe 
###  with the individual interview data to disaggregate them further by additional socio-demographic variables
 










