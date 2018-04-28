library(tidyr)
library(reshape2)
library(dplyr)
setwd("~/Documents/UH/prospectus/")


data <- read.csv("data_4.csv")

### section 1
data[data$X1_1_0 ==1,"X1_1_0"] <- "18 -- 24"
data[data$X1_1_1 ==1,"X1_1_1"] <- "25 -- 34"
data[data$X1_1_2 ==1,"X1_1_2"] <- "35 -- 44"
data[data$X1_1_3 ==1,"X1_1_3"] <- "45 -- 54"
data[data$X1_1_4 ==1,"X1_1_4"] <- "55 -- 64"
data[data$X1_1_5 ==1,"X1_1_5"] <- "65 -- 74"
data[data$X1_1_6 ==1,"X1_1_6"] <- "75+"

data[data$X1_2_0 ==1,"X1_2_0"] <- "Female"
data[data$X1_2_1 ==1,"X1_2_1"] <- "Male"

data[data$X1_3_0 ==1,"X1_3_0"] <- "Pohnpei State"
data[data$X1_3_1 ==1,"X1_3_1"] <- "Chuuk State"
data[data$X1_3_2 ==1,"X1_3_2"] <- "Kosrae State"
data[data$X1_3_3 ==1,"X1_3_3"] <- "Yap State"
data[data$X1_3_4 ==1,"X1_3_4"] <- "RMI"
data[data$X1_3_5 ==1,"X1_3_5"] <- "Palau"
data[data$X1_3_6 ==1,"X1_3_6"] <- "Guam"
data[data$X1_3_7 ==1,"X1_3_7"] <- "CNMI"
data[data$X1_3_8 ==1,"X1_3_8"] <- "Hawaii"
data[data$X1_3_9 ==1,"X1_3_9"] <- "US Mainland"
data[data$X1_3_10 ==1,"X1_3_10"] <- "Other"

data[data$X1_6_0 ==1,"X1_6_0"] <- "FSM"
data[data$X1_6_1 ==1,"X1_6_1"] <- "RMI"
data[data$X1_6_2 ==1,"X1_6_2"] <- "Palau"
data[data$X1_6_3 ==1,"X1_6_3"] <- "USA"
data[data$X1_6_4 ==1,"X1_6_4"] <- "Other"

data[data$X1_7_0 ==1,"X1_7_0"] <- "Nett"
data[data$X1_7_1 ==1,"X1_7_1"] <- "Uh"
data[data$X1_7_2 ==1,"X1_7_2"] <- "Sokehs"
data[data$X1_7_3 ==1,"X1_7_3"] <- "Madolenihmw"
data[data$X1_7_4 ==1,"X1_7_4"] <- "Kitti"

data[data$X1_9_0 ==1,"X1_9_0"] <- "0 - 4 years"
data[data$X1_9_1 ==1,"X1_9_1"] <- "5 - 9 years"
data[data$X1_9_2 ==1,"X1_9_2"] <- "10 - 19 years"
data[data$X1_9_3 ==1,"X1_9_3"] <- "20 - 29 years"
data[data$X1_9_4 ==1,"X1_9_4"] <- "30 - 39 years"
data[data$X1_9_5 ==1,"X1_9_5"] <- "40 or more years"

data[data$X1_10_0 ==1,"X1_10_0"] <- "0 - 4 years"
data[data$X1_10_1 ==1,"X1_10_1"] <- "5 - 9 years"
data[data$X1_10_2 ==1,"X1_10_2"] <- "10 - 19 years"
data[data$X1_10_3 ==1,"X1_10_3"] <- "20 - 29 years"
data[data$X1_10_4 ==1,"X1_10_4"] <- "30 - 39 years"
data[data$X1_10_5 ==1,"X1_10_5"] <- "40 or more years"

data[data$X1_11_0 ==1,"X1_11_0"] <- "0 - 4 years"
data[data$X1_11_1 ==1,"X1_11_1"] <- "5 - 9 years"
data[data$X1_11_2 ==1,"X1_11_2"] <- "10 - 19 years"
data[data$X1_11_3 ==1,"X1_11_3"] <- "20 - 29 years"
data[data$X1_11_4 ==1,"X1_11_4"] <- "30 - 39 years"
data[data$X1_11_5 ==1,"X1_11_5"] <- "40 or more years"

data[data$X1_12_0 ==1,"X1_12_0"] <- "Yes"
data[data$X1_12_1 ==1,"X1_12_1"] <- "No"

data[data$X1_14_0 ==1,"X1_14_0"] <- "No schooling completed"
data[data$X1_14_1 ==1,"X1_14_1"] <- "Kingergarten - 8th grade"
data[data$X1_14_2 ==1,"X1_14_2"] <- "Some high school, no diploma"
data[data$X1_14_3 ==1,"X1_14_3"] <- "High school, diploma, or GED"
data[data$X1_14_4 ==1,"X1_14_4"] <- "Some college, no degree"
data[data$X1_14_5 ==1,"X1_14_5"] <- "Trade/Technical/vocational training"
data[data$X1_14_6 ==1,"X1_14_6"] <- "Associate degree"
data[data$X1_14_7 ==1,"X1_14_7"] <- "Bachelor's degree"
data[data$X1_14_8 ==1,"X1_14_8"] <- "Master's degree"
data[data$X1_14_9 ==1,"X1_14_9"] <- "Professional degree (JD, MD, etc.)"
data[data$X1_14_10 ==1,"X1_14_10"] <- "Doctorate degree"

data[data$X1_15_0 ==1,"X1_15_0"] <- "Public school"
data[data$X1_15_1 ==1,"X1_15_1"] <- "Private school"

data[data$X1_16_0 ==1,"X1_16_0"] <- "Public school"
data[data$X1_16_1 ==1,"X1_16_1"] <- "Private school"

data[data$X1_17_0 ==1,"X1_17_0"] <- "zero"
data[data$X1_17_1 ==1,"X1_17_1"] <- "1"
data[data$X1_17_2 ==1,"X1_17_2"] <- "2"
data[data$X1_17_3 ==1,"X1_17_3"] <- "3"
data[data$X1_17_4 ==1,"X1_17_4"] <- "4"
data[data$X1_17_5 ==1,"X1_17_5"] <- "5"
data[data$X1_17_6 ==1,"X1_17_6"] <- "6"
data[data$X1_17_7 ==1,"X1_17_7"] <- "7"
data[data$X1_17_8 ==1,"X1_17_8"] <- "8 or more"

#### section 2
data[data$X2_3_0 ==1,"X2_3_0"] <- "Not at all"
data[data$X2_3_1 ==1,"X2_3_1"] <- "Somewhat well"
data[data$X2_3_2 ==1,"X2_3_2"] <- "Well"
data[data$X2_3_3 ==1,"X2_3_3"] <- "Very well"

### section 3
data[data$X3_1_1_0 ==1,"X3_1_1_0"] <- "Pohnpeian"
data[data$X3_1_1_1 ==1,"X3_1_1_1"] <- "Pingelapese"
data[data$X3_1_1_2 ==1,"X3_1_1_2"] <- "Mokilese"
data[data$X3_1_1_3 ==1,"X3_1_1_3"] <- "Chuukese"
data[data$X3_1_1_4 ==1,"X3_1_1_4"] <- "English"
data[data$X3_1_1_5 ==1,"X3_1_1_5"] <- "Kosraean"
data[data$X3_1_1_6 ==1,"X3_1_1_6"] <- "Mortlockese"
data[data$X3_1_1_7 ==1,"X3_1_1_7"] <- "Other"

data[data$X3_1_2_0 ==1,"X3_1_2_0"] <- "Pohnpeian"
data[data$X3_1_2_1 ==1,"X3_1_2_1"] <- "Pingelapese"
data[data$X3_1_2_2 ==1,"X3_1_2_2"] <- "Mokilese"
data[data$X3_1_2_3 ==1,"X3_1_2_3"] <- "Chuukese"
data[data$X3_1_2_4 ==1,"X3_1_2_4"] <- "English"
data[data$X3_1_2_5 ==1,"X3_1_2_5"] <- "Kosraean"
data[data$X3_1_2_6 ==1,"X3_1_2_6"] <- "Mortlockese"
data[data$X3_1_2_7 ==1,"X3_1_2_7"] <- "Other"

data[data$X3_1_3_0 ==1,"X3_1_3_0"] <- "Pohnpeian"
data[data$X3_1_3_1 ==1,"X3_1_3_1"] <- "Pingelapese"
data[data$X3_1_3_2 ==1,"X3_1_3_2"] <- "Mokilese"
data[data$X3_1_3_3 ==1,"X3_1_3_3"] <- "Chuukese"
data[data$X3_1_3_4 ==1,"X3_1_3_4"] <- "English"
data[data$X3_1_3_5 ==1,"X3_1_3_5"] <- "Kosraean"
data[data$X3_1_3_6 ==1,"X3_1_3_6"] <- "Mortlockese"
data[data$X3_1_3_7 ==1,"X3_1_3_7"] <- "Other"

data[data$X3_1_4_0 ==1,"X3_1_4_0"] <- "Pohnpeian"
data[data$X3_1_4_1 ==1,"X3_1_4_1"] <- "Pingelapese"
data[data$X3_1_4_2 ==1,"X3_1_4_2"] <- "Mokilese"
data[data$X3_1_4_3 ==1,"X3_1_4_3"] <- "Chuukese"
data[data$X3_1_4_4 ==1,"X3_1_4_4"] <- "English"
data[data$X3_1_4_5 ==1,"X3_1_4_5"] <- "Kosraean"
data[data$X3_1_4_6 ==1,"X3_1_4_6"] <- "Mortlockese"
data[data$X3_1_4_7 ==1,"X3_1_4_7"] <- "Other"

data[data$X3_1_5_0 ==1,"X3_1_5_0"] <- "Pohnpeian"
data[data$X3_1_5_1 ==1,"X3_1_5_1"] <- "Pingelapese"
data[data$X3_1_5_2 ==1,"X3_1_5_2"] <- "Mokilese"
data[data$X3_1_5_3 ==1,"X3_1_5_3"] <- "Chuukese"
data[data$X3_1_5_4 ==1,"X3_1_5_4"] <- "English"
data[data$X3_1_5_5 ==1,"X3_1_5_5"] <- "Kosraean"
data[data$X3_1_5_6 ==1,"X3_1_5_6"] <- "Mortlockese"
data[data$X3_1_5_7 ==1,"X3_1_5_7"] <- "Other"

data[data$X3_1_6_0 ==1,"X3_1_6_0"] <- "Pohnpeian"
data[data$X3_1_6_1 ==1,"X3_1_6_1"] <- "Pingelapese"
data[data$X3_1_6_2 ==1,"X3_1_6_2"] <- "Mokilese"
data[data$X3_1_6_3 ==1,"X3_1_6_3"] <- "Chuukese"
data[data$X3_1_6_4 ==1,"X3_1_6_4"] <- "English"
data[data$X3_1_6_5 ==1,"X3_1_6_5"] <- "Kosraean"
data[data$X3_1_6_6 ==1,"X3_1_6_6"] <- "Mortlockese"
data[data$X3_1_6_7 ==1,"X3_1_6_7"] <- "Other"

data[data$X3_1_7_0 ==1,"X3_1_7_0"] <- "Pohnpeian"
data[data$X3_1_7_1 ==1,"X3_1_7_1"] <- "Pingelapese"
data[data$X3_1_7_2 ==1,"X3_1_7_2"] <- "Mokilese"
data[data$X3_1_7_3 ==1,"X3_1_7_3"] <- "Chuukese"
data[data$X3_1_7_4 ==1,"X3_1_7_4"] <- "English"
data[data$X3_1_7_5 ==1,"X3_1_7_5"] <- "Kosraean"
data[data$X3_1_7_6 ==1,"X3_1_7_6"] <- "Mortlockese"
data[data$X3_1_7_7 ==1,"X3_1_7_7"] <- "Other"

data[data$X3_1_8_0 ==1,"X3_1_8_0"] <- "Pohnpeian"
data[data$X3_1_8_1 ==1,"X3_1_8_1"] <- "Pingelapese"
data[data$X3_1_8_2 ==1,"X3_1_8_2"] <- "Mokilese"
data[data$X3_1_8_3 ==1,"X3_1_8_3"] <- "Chuukese"
data[data$X3_1_8_4 ==1,"X3_1_8_4"] <- "English"
data[data$X3_1_8_5 ==1,"X3_1_8_5"] <- "Kosraean"
data[data$X3_1_8_6 ==1,"X3_1_8_6"] <- "Mortlockese"
data[data$X3_1_8_7 ==1,"X3_1_8_7"] <- "Other"

data[data$X3_1_9_0 ==1,"X3_1_9_0"] <- "Pohnpeian"
data[data$X3_1_9_1 ==1,"X3_1_9_1"] <- "Pingelapese"
data[data$X3_1_9_2 ==1,"X3_1_9_2"] <- "Mokilese"
data[data$X3_1_9_3 ==1,"X3_1_9_3"] <- "Chuukese"
data[data$X3_1_9_4 ==1,"X3_1_9_4"] <- "English"
data[data$X3_1_9_5 ==1,"X3_1_9_5"] <- "Kosraean"
data[data$X3_1_9_6 ==1,"X3_1_9_6"] <- "Mortlockese"
data[data$X3_1_9_7 ==1,"X3_1_9_7"] <- "Other"

data[data$X3_1_10_0 ==1,"X3_1_10_0"] <- "Pohnpeian"
data[data$X3_1_10_1 ==1,"X3_1_10_1"] <- "Pingelapese"
data[data$X3_1_10_2 ==1,"X3_1_10_2"] <- "Mokilese"
data[data$X3_1_10_3 ==1,"X3_1_10_3"] <- "Chuukese"
data[data$X3_1_10_4 ==1,"X3_1_10_4"] <- "English"
data[data$X3_1_10_5 ==1,"X3_1_10_5"] <- "Kosraean"
data[data$X3_1_10_6 ==1,"X3_1_10_6"] <- "Mortlockese"
data[data$X3_1_10_7 ==1,"X3_1_10_7"] <- "Other"

data[data$X3_1_11_0 ==1,"X3_1_11_0"] <- "Pohnpeian"
data[data$X3_1_11_1 ==1,"X3_1_11_1"] <- "Pingelapese"
data[data$X3_1_11_2 ==1,"X3_1_11_2"] <- "Mokilese"
data[data$X3_1_11_3 ==1,"X3_1_11_3"] <- "Chuukese"
data[data$X3_1_11_4 ==1,"X3_1_11_4"] <- "English"
data[data$X3_1_11_5 ==1,"X3_1_11_5"] <- "Kosraean"
data[data$X3_1_11_6 ==1,"X3_1_11_6"] <- "Mortlockese"
data[data$X3_1_11_7 ==1,"X3_1_11_7"] <- "Other"

data[data$X3_2_1_0 ==1,"X3_2_1_0"] <- "Pohnpeian"
data[data$X3_2_1_1 ==1,"X3_2_1_1"] <- "Pingelapese"
data[data$X3_2_1_2 ==1,"X3_2_1_2"] <- "Mokilese"
data[data$X3_2_1_3 ==1,"X3_2_1_3"] <- "Chuukese"
data[data$X3_2_1_4 ==1,"X3_2_1_4"] <- "English"
data[data$X3_2_1_5 ==1,"X3_2_1_5"] <- "Kosraean"
data[data$X3_2_1_6 ==1,"X3_2_1_6"] <- "Mortlockese"
data[data$X3_2_1_7 ==1,"X3_2_1_7"] <- "Other"

data[data$X3_2_2_0 ==1,"X3_2_2_0"] <- "Pohnpeian"
data[data$X3_2_2_1 ==1,"X3_2_2_1"] <- "Pingelapese"
data[data$X3_2_2_2 ==1,"X3_2_2_2"] <- "Mokilese"
data[data$X3_2_2_3 ==1,"X3_2_2_3"] <- "Chuukese"
data[data$X3_2_2_4 ==1,"X3_2_2_4"] <- "English"
data[data$X3_2_2_5 ==1,"X3_2_2_5"] <- "Kosraean"
data[data$X3_2_2_6 ==1,"X3_2_2_6"] <- "Mortlockese"
data[data$X3_2_2_7 ==1,"X3_2_2_7"] <- "Other"

data[data$X3_2_3_0 ==1,"X3_2_3_0"] <- "Pohnpeian"
data[data$X3_2_3_1 ==1,"X3_2_3_1"] <- "Pingelapese"
data[data$X3_2_3_2 ==1,"X3_2_3_2"] <- "Mokilese"
data[data$X3_2_3_3 ==1,"X3_2_3_3"] <- "Chuukese"
data[data$X3_2_3_4 ==1,"X3_2_3_4"] <- "English"
data[data$X3_2_3_5 ==1,"X3_2_3_5"] <- "Kosraean"
data[data$X3_2_3_6 ==1,"X3_2_3_6"] <- "Mortlockese"
data[data$X3_2_3_7 ==1,"X3_2_3_7"] <- "Other"

data[data$X3_2_4_0 ==1,"X3_2_4_0"] <- "Pohnpeian"
data[data$X3_2_4_1 ==1,"X3_2_4_1"] <- "Pingelapese"
data[data$X3_2_4_2 ==1,"X3_2_4_2"] <- "Mokilese"
data[data$X3_2_4_3 ==1,"X3_2_4_3"] <- "Chuukese"
data[data$X3_2_4_4 ==1,"X3_2_4_4"] <- "English"
data[data$X3_2_4_5 ==1,"X3_2_4_5"] <- "Kosraean"
data[data$X3_2_4_6 ==1,"X3_2_4_6"] <- "Mortlockese"
data[data$X3_2_4_7 ==1,"X3_2_4_7"] <- "Other"

data[data$X3_2_5_0 ==1,"X3_2_5_0"] <- "Pohnpeian"
data[data$X3_2_5_1 ==1,"X3_2_5_1"] <- "Pingelapese"
data[data$X3_2_5_2 ==1,"X3_2_5_2"] <- "Mokilese"
data[data$X3_2_5_3 ==1,"X3_2_5_3"] <- "Chuukese"
data[data$X3_2_5_4 ==1,"X3_2_5_4"] <- "English"
data[data$X3_2_5_5 ==1,"X3_2_5_5"] <- "Kosraean"
data[data$X3_2_5_6 ==1,"X3_2_5_6"] <- "Mortlockese"
data[data$X3_2_5_7 ==1,"X3_2_5_7"] <- "Other"

data[data$X3_2_6_0 ==1,"X3_2_6_0"] <- "Pohnpeian"
data[data$X3_2_6_1 ==1,"X3_2_6_1"] <- "Pingelapese"
data[data$X3_2_6_2 ==1,"X3_2_6_2"] <- "Mokilese"
data[data$X3_2_6_3 ==1,"X3_2_6_3"] <- "Chuukese"
data[data$X3_2_6_4 ==1,"X3_2_6_4"] <- "English"
data[data$X3_2_6_5 ==1,"X3_2_6_5"] <- "Kosraean"
data[data$X3_2_6_6 ==1,"X3_2_6_6"] <- "Mortlockese"
data[data$X3_2_6_7 ==1,"X3_2_6_7"] <- "Other"

data[data$X3_2_7_0 ==1,"X3_2_7_0"] <- "Pohnpeian"
data[data$X3_2_7_1 ==1,"X3_2_7_1"] <- "Pingelapese"
data[data$X3_2_7_2 ==1,"X3_2_7_2"] <- "Mokilese"
data[data$X3_2_7_3 ==1,"X3_2_7_3"] <- "Chuukese"
data[data$X3_2_7_4 ==1,"X3_2_7_4"] <- "English"
data[data$X3_2_7_5 ==1,"X3_2_7_5"] <- "Kosraean"
data[data$X3_2_7_6 ==1,"X3_2_7_6"] <- "Mortlockese"
data[data$X3_2_7_7 ==1,"X3_2_7_7"] <- "Other"

##
data[data$X3_3_1_0 ==1,"X3_3_1_0"] <- "Pohnpeian"
data[data$X3_3_1_1 ==1,"X3_3_1_1"] <- "Pingelapese"
data[data$X3_3_1_2 ==1,"X3_3_1_2"] <- "Mokilese"
data[data$X3_3_1_3 ==1,"X3_3_1_3"] <- "Chuukese"
data[data$X3_3_1_4 ==1,"X3_3_1_4"] <- "English"
data[data$X3_3_1_5 ==1,"X3_3_1_5"] <- "Kosraean"
data[data$X3_3_1_6 ==1,"X3_3_1_6"] <- "Mortlockese"
data[data$X3_3_1_7 ==1,"X3_3_1_7"] <- "Other"

data[data$X3_3_2_0 ==1,"X3_3_2_0"] <- "Pohnpeian"
data[data$X3_3_2_1 ==1,"X3_3_2_1"] <- "Pingelapese"
data[data$X3_3_2_2 ==1,"X3_3_2_2"] <- "Mokilese"
data[data$X3_3_2_3 ==1,"X3_3_2_3"] <- "Chuukese"
data[data$X3_3_2_4 ==1,"X3_3_2_4"] <- "English"
data[data$X3_3_2_5 ==1,"X3_3_2_5"] <- "Kosraean"
data[data$X3_3_2_6 ==1,"X3_3_2_6"] <- "Mortlockese"
data[data$X3_3_2_7 ==1,"X3_3_2_7"] <- "Other"

data[data$X3_3_3_0 ==1,"X3_3_3_0"] <- "Pohnpeian"
data[data$X3_3_3_1 ==1,"X3_3_3_1"] <- "Pingelapese"
data[data$X3_3_3_2 ==1,"X3_3_3_2"] <- "Mokilese"
data[data$X3_3_3_3 ==1,"X3_3_3_3"] <- "Chuukese"
data[data$X3_3_3_4 ==1,"X3_3_3_4"] <- "English"
data[data$X3_3_3_5 ==1,"X3_3_3_5"] <- "Kosraean"
data[data$X3_3_3_6 ==1,"X3_3_3_6"] <- "Mortlockese"
data[data$X3_3_3_7 ==1,"X3_3_3_7"] <- "Other"

data[data$X3_3_4_0 ==1,"X3_3_4_0"] <- "Pohnpeian"
data[data$X3_3_4_1 ==1,"X3_3_4_1"] <- "Pingelapese"
data[data$X3_3_4_2 ==1,"X3_3_4_2"] <- "Mokilese"
data[data$X3_3_4_3 ==1,"X3_3_4_3"] <- "Chuukese"
data[data$X3_3_4_4 ==1,"X3_3_4_4"] <- "English"
data[data$X3_3_4_5 ==1,"X3_3_4_5"] <- "Kosraean"
data[data$X3_3_4_6 ==1,"X3_3_4_6"] <- "Mortlockese"
data[data$X3_3_4_7 ==1,"X3_3_4_7"] <- "Other"

data[data$X3_3_5_0 ==1,"X3_3_5_0"] <- "Pohnpeian"
data[data$X3_3_5_1 ==1,"X3_3_5_1"] <- "Pingelapese"
data[data$X3_3_5_2 ==1,"X3_3_5_2"] <- "Mokilese"
data[data$X3_3_5_3 ==1,"X3_3_5_3"] <- "Chuukese"
data[data$X3_3_5_4 ==1,"X3_3_5_4"] <- "English"
data[data$X3_3_5_5 ==1,"X3_3_5_5"] <- "Kosraean"
data[data$X3_3_5_6 ==1,"X3_3_5_6"] <- "Mortlockese"
data[data$X3_3_5_7 ==1,"X3_3_5_7"] <- "Other"

data[data$X3_3_6_0 ==1,"X3_3_6_0"] <- "Pohnpeian"
data[data$X3_3_6_1 ==1,"X3_3_6_1"] <- "Pingelapese"
data[data$X3_3_6_2 ==1,"X3_3_6_2"] <- "Mokilese"
data[data$X3_3_6_3 ==1,"X3_3_6_3"] <- "Chuukese"
data[data$X3_3_6_4 ==1,"X3_3_6_4"] <- "English"
data[data$X3_3_6_5 ==1,"X3_3_6_5"] <- "Kosraean"
data[data$X3_3_6_6 ==1,"X3_3_6_6"] <- "Mortlockese"
data[data$X3_3_6_7 ==1,"X3_3_6_7"] <- "Other"

data[data$X3_3_7_0 ==1,"X3_3_7_0"] <- "Pohnpeian"
data[data$X3_3_7_1 ==1,"X3_3_7_1"] <- "Pingelapese"
data[data$X3_3_7_2 ==1,"X3_3_7_2"] <- "Mokilese"
data[data$X3_3_7_3 ==1,"X3_3_7_3"] <- "Chuukese"
data[data$X3_3_7_4 ==1,"X3_3_7_4"] <- "English"
data[data$X3_3_7_5 ==1,"X3_3_7_5"] <- "Kosraean"
data[data$X3_3_7_6 ==1,"X3_3_7_6"] <- "Mortlockese"
data[data$X3_3_7_7 ==1,"X3_3_7_7"] <- "Other"

##
data[data$X3_4_1_0 ==1,"X3_4_1_0"] <- "Agree"
data[data$X3_4_1_1 ==1,"X3_4_1_1"] <- "Disagree"

data[data$X3_4_2_0 ==1,"X3_4_2_0"] <- "Agree"
data[data$X3_4_2_1 ==1,"X3_4_2_1"] <- "Disagree"


data[data$X3_4_3_0 ==1,"X3_4_3_0"] <- "Agree"
data[data$X3_4_3_1 ==1,"X3_4_3_1"] <- "Disagree"

data[data$X3_4_4_0 ==1,"X3_4_4_0"] <- "Agree"
data[data$X3_4_4_1 ==1,"X3_4_4_1"] <- "Disagree"

data[data$X3_4_5_0 ==1,"X3_4_5_0"] <- "Agree"
data[data$X3_4_5_1 ==1,"X3_4_5_1"] <- "Disagree"

data[data$X3_4_6_0 ==1,"X3_4_6_0"] <- "Agree"
data[data$X3_4_6_1 ==1,"X3_4_6_1"] <- "Disagree"

data[data$X3_4_7_0 ==1,"X3_4_7_0"] <- "Agree"
data[data$X3_4_7_1 ==1,"X3_4_7_1"] <- "Disagree"

data[data$X3_4_8_0 ==1,"X3_4_8_0"] <- "Agree"
data[data$X3_4_8_1 ==1,"X3_4_8_1"] <- "Disagree"

data[data$X3_4_9_0 ==1,"X3_4_9_0"] <- "Agree"
data[data$X3_4_9_1 ==1,"X3_4_9_1"] <- "Disagree"

data[data$X3_4_10_0 ==1,"X3_4_10_0"] <- "Agree"
data[data$X3_4_10_1 ==1,"X3_4_10_1"] <- "Disagree"

data[data$X3_4_11_0 ==1,"X3_4_11_0"] <- "Agree"
data[data$X3_4_11_1 ==1,"X3_4_11_1"] <- "Disagree"

data[data$X3_4_12_0 ==1,"X3_4_12_0"] <- "Agree"
data[data$X3_4_12_1 ==1,"X3_4_12_1"] <- "Disagree"

data[data$X3_4_13_0 ==1,"X3_4_13_0"] <- "Agree"
data[data$X3_4_13_1 ==1,"X3_4_13_1"] <- "Disagree"

data[data$X3_4_14_0 ==1,"X3_4_14_0"] <- "Agree"
data[data$X3_4_14_1 ==1,"X3_4_14_1"] <- "Disagree"

data[data$X3_4_15_0 ==1,"X3_4_15_0"] <- "Agree"
data[data$X3_4_15_1 ==1,"X3_4_15_1"] <- "Disagree"

data[data$X3_4_16_0 ==1,"X3_4_16_0"] <- "Agree"
data[data$X3_4_16_1 ==1,"X3_4_16_1"] <- "Disagree"

data[data$X3_4_17_0 ==1,"X3_4_17_0"] <- "Agree"
data[data$X3_4_17_1 ==1,"X3_4_17_1"] <- "Disagree"

data[data$X3_4_18_0 ==1,"X3_4_18_0"] <- "Agree"
data[data$X3_4_18_1 ==1,"X3_4_18_1"] <- "Disagree"

data[data$X3_4_19_0 ==1,"X3_4_19_0"] <- "Agree"
data[data$X3_4_19_1 ==1,"X3_4_19_1"] <- "Disagree"

data[data$X3_4_20_0 ==1,"X3_4_20_0"] <- "Agree"
data[data$X3_4_20_1 ==1,"X3_4_20_1"] <- "Disagree"

data[data$X3_4_21_0 ==1,"X3_4_21_0"] <- "Agree"
data[data$X3_4_21_1 ==1,"X3_4_21_1"] <- "Disagree"

data[data$X3_4_22_0 ==1,"X3_4_22_0"] <- "Agree"
data[data$X3_4_22_1 ==1,"X3_4_22_1"] <- "Disagree"

data[data$X3_4_23_0 ==1,"X3_4_23_0"] <- "Agree"
data[data$X3_4_23_1 ==1,"X3_4_23_1"] <- "Disagree"

data[data$X3_4_24_0 ==1,"X3_4_24_0"] <- "Agree"
data[data$X3_4_24_1 ==1,"X3_4_24_1"] <- "Disagree"

data[data$X3_4_25_0 ==1,"X3_4_25_0"] <- "Agree"
data[data$X3_4_25_1 ==1,"X3_4_25_1"] <- "Disagree"

data[data$X3_4_26_0 ==1,"X3_4_26_0"] <- "Agree"
data[data$X3_4_26_1 ==1,"X3_4_26_1"] <- "Disagree"

data[data$X3_4_27_0 ==1,"X3_4_27_0"] <- "Agree"
data[data$X3_4_27_1 ==1,"X3_4_27_1"] <- "Disagree"

data[data$X3_4_28_0 ==1,"X3_4_28_0"] <- "Agree"
data[data$X3_4_28_1 ==1,"X3_4_28_1"] <- "Disagree"

data[data$X3_4_29_0 ==1,"X3_4_29_0"] <- "Agree"
data[data$X3_4_29_1 ==1,"X3_4_29_1"] <- "Disagree"

data[data$X3_4_30_0 ==1,"X3_4_30_0"] <- "Agree"
data[data$X3_4_30_1 ==1,"X3_4_30_1"] <- "Disagree"

data[data$X3_4_31_0 ==1,"X3_4_31_0"] <- "Agree"
data[data$X3_4_31_1 ==1,"X3_4_31_1"] <- "Disagree"

data[data$X3_4_32_0 ==1,"X3_4_32_0"] <- "Agree"
data[data$X3_4_32_1 ==1,"X3_4_32_1"] <- "Disagree"

data[data$X3_4_33_0 ==1,"X3_4_33_0"] <- "Agree"
data[data$X3_4_33_1 ==1,"X3_4_33_1"] <- "Disagree"

data[data$X3_4_34_0 ==1,"X3_4_34_0"] <- "Agree"
data[data$X3_4_34_1 ==1,"X3_4_34_1"] <- "Disagree"

data[data$X3_4_35_0 ==1,"X3_4_35_0"] <- "Agree"
data[data$X3_4_35_1 ==1,"X3_4_35_1"] <- "Disagree"

data[data$X3_4_36_0 ==1,"X3_4_36_0"] <- "Agree"
data[data$X3_4_36_1 ==1,"X3_4_36_1"] <- "Disagree"

data[data$X3_4_37_0 ==1,"X3_4_37_0"] <- "Agree"
data[data$X3_4_37_1 ==1,"X3_4_37_1"] <- "Disagree"

data[data$X3_4_38_0 ==1,"X3_4_38_0"] <- "Agree"
data[data$X3_4_38_1 ==1,"X3_4_38_1"] <- "Disagree"

data[data$X3_5_1_0 ==1,"X3_5_1_0"] <- 1
data[data$X3_5_1_1 ==1,"X3_5_1_1"] <- 2
data[data$X3_5_1_2 ==1,"X3_5_1_2"] <- 3
data[data$X3_5_1_3 ==1,"X3_5_1_3"] <- 4

data[data$X3_5_2_0 ==1,"X3_5_2_0"] <- 1
data[data$X3_5_2_1 ==1,"X3_5_2_1"] <- 2
data[data$X3_5_2_2 ==1,"X3_5_2_2"] <- 3
data[data$X3_5_2_3 ==1,"X3_5_2_3"] <- 4

data[data$X3_5_3_0 ==1,"X3_5_3_0"] <- 1
data[data$X3_5_3_1 ==1,"X3_5_3_1"] <- 2
data[data$X3_5_3_2 ==1,"X3_5_3_2"] <- 3
data[data$X3_5_3_3 ==1,"X3_5_3_3"] <- 4

data[data$X3_5_4_0 ==1,"X3_5_4_0"] <- 1
data[data$X3_5_4_1 ==1,"X3_5_4_1"] <- 2
data[data$X3_5_4_2 ==1,"X3_5_4_2"] <- 3
data[data$X3_5_4_3 ==1,"X3_5_4_3"] <- 4

data[data$X3_5_5_0 ==1,"X3_5_5_0"] <- 1
data[data$X3_5_5_1 ==1,"X3_5_5_1"] <- 2
data[data$X3_5_5_2 ==1,"X3_5_5_2"] <- 3
data[data$X3_5_5_3 ==1,"X3_5_5_3"] <- 4

data[data$X3_5_6_0 ==1,"X3_5_6_0"] <- 1
data[data$X3_5_6_1 ==1,"X3_5_6_1"] <- 2
data[data$X3_5_6_2 ==1,"X3_5_6_2"] <- 3
data[data$X3_5_6_3 ==1,"X3_5_6_3"] <- 4

data[data$X3_5_7_0 ==1,"X3_5_7_0"] <- 1
data[data$X3_5_7_1 ==1,"X3_5_7_1"] <- 2
data[data$X3_5_7_2 ==1,"X3_5_7_2"] <- 3
data[data$X3_5_7_3 ==1,"X3_5_7_3"] <- 4

data[data$X3_5_8_0 ==1,"X3_5_8_0"] <- 1
data[data$X3_5_8_1 ==1,"X3_5_8_1"] <- 2
data[data$X3_5_8_2 ==1,"X3_5_8_2"] <- 3
data[data$X3_5_8_3 ==1,"X3_5_8_3"] <- 4

data[data$X3_5_9_0 ==1,"X3_5_9_0"] <- 1
data[data$X3_5_9_1 ==1,"X3_5_9_1"] <- 2
data[data$X3_5_9_2 ==1,"X3_5_9_2"] <- 3
data[data$X3_5_9_3 ==1,"X3_5_9_3"] <- 4

data[data$X3_5_10_0 ==1,"X3_5_10_0"] <- 1
data[data$X3_5_10_1 ==1,"X3_5_10_1"] <- 2
data[data$X3_5_10_2 ==1,"X3_5_10_2"] <- 3
data[data$X3_5_10_3 ==1,"X3_5_10_3"] <- 4

data[data$X3_5_11_0 ==1,"X3_5_11_0"] <- 1
data[data$X3_5_11_1 ==1,"X3_5_11_1"] <- 2
data[data$X3_5_11_2 ==1,"X3_5_11_2"] <- 3
data[data$X3_5_11_3 ==1,"X3_5_11_3"] <- 4

data[data$X3_5_12_0 ==1,"X3_5_12_0"] <- 1
data[data$X3_5_12_1 ==1,"X3_5_12_1"] <- 2
data[data$X3_5_12_2 ==1,"X3_5_12_2"] <- 3
data[data$X3_5_12_3 ==1,"X3_5_12_3"] <- 4

data[data$X3_5_13_0 ==1,"X3_5_13_0"] <- 1
data[data$X3_5_13_1 ==1,"X3_5_13_1"] <- 2
data[data$X3_5_13_2 ==1,"X3_5_13_2"] <- 3
data[data$X3_5_13_3 ==1,"X3_5_13_3"] <- 4

data[data$X3_5_14_0 ==1,"X3_5_14_0"] <- 1
data[data$X3_5_14_1 ==1,"X3_5_14_1"] <- 2
data[data$X3_5_14_2 ==1,"X3_5_14_2"] <- 3
data[data$X3_5_14_3 ==1,"X3_5_14_3"] <- 4

data[data$X3_5_15_0 ==1,"X3_5_15_0"] <- 1
data[data$X3_5_15_1 ==1,"X3_5_15_1"] <- 2
data[data$X3_5_15_2 ==1,"X3_5_15_2"] <- 3
data[data$X3_5_15_3 ==1,"X3_5_15_3"] <- 4

data[data$X3_5_16_0 ==1,"X3_5_16_0"] <- 1
data[data$X3_5_16_1 ==1,"X3_5_16_1"] <- 2
data[data$X3_5_16_2 ==1,"X3_5_16_2"] <- 3
data[data$X3_5_16_3 ==1,"X3_5_16_3"] <- 4

data[data$X3_5_17_0 ==1,"X3_5_17_0"] <- 1
data[data$X3_5_17_1 ==1,"X3_5_17_1"] <- 2
data[data$X3_5_17_2 ==1,"X3_5_17_2"] <- 3
data[data$X3_5_17_3 ==1,"X3_5_17_3"] <- 4

data[data$X3_5_18_0 ==1,"X3_5_18_0"] <- 1
data[data$X3_5_18_1 ==1,"X3_5_18_1"] <- 2
data[data$X3_5_18_2 ==1,"X3_5_18_2"] <- 3
data[data$X3_5_18_3 ==1,"X3_5_18_3"] <- 4

data[data$X3_5_19_0 ==1,"X3_5_19_0"] <- 1
data[data$X3_5_19_1 ==1,"X3_5_19_1"] <- 2
data[data$X3_5_19_2 ==1,"X3_5_19_2"] <- 3
data[data$X3_5_19_3 ==1,"X3_5_19_3"] <- 4

data[data$X3_5_20_0 ==1,"X3_5_20_0"] <- 1
data[data$X3_5_20_1 ==1,"X3_5_20_1"] <- 2
data[data$X3_5_20_2 ==1,"X3_5_20_2"] <- 3
data[data$X3_5_20_3 ==1,"X3_5_20_3"] <- 4

data[data$X3_5_21_0 ==1,"X3_5_21_0"] <- 1
data[data$X3_5_21_1 ==1,"X3_5_21_1"] <- 2
data[data$X3_5_21_2 ==1,"X3_5_21_2"] <- 3
data[data$X3_5_21_3 ==1,"X3_5_21_3"] <- 4

data[data$X3_5_22_0 ==1,"X3_5_22_0"] <- 1
data[data$X3_5_22_1 ==1,"X3_5_22_1"] <- 2
data[data$X3_5_22_2 ==1,"X3_5_22_2"] <- 3
data[data$X3_5_22_3 ==1,"X3_5_22_3"] <- 4

data[data$X3_5_23_0 ==1,"X3_5_23_0"] <- 1
data[data$X3_5_23_1 ==1,"X3_5_23_1"] <- 2
data[data$X3_5_23_2 ==1,"X3_5_23_2"] <- 3
data[data$X3_5_23_3 ==1,"X3_5_23_3"] <- 4

data[data$X3_5_24_0 ==1,"X3_5_24_0"] <- 1
data[data$X3_5_24_1 ==1,"X3_5_24_1"] <- 2
data[data$X3_5_24_2 ==1,"X3_5_24_2"] <- 3
data[data$X3_5_24_3 ==1,"X3_5_24_3"] <- 4

data[data$X3_5_25_0 ==1,"X3_5_25_0"] <- 1
data[data$X3_5_25_1 ==1,"X3_5_25_1"] <- 2
data[data$X3_5_25_2 ==1,"X3_5_25_2"] <- 3
data[data$X3_5_25_3 ==1,"X3_5_25_3"] <- 4

data[data$X3_5_26_0 ==1,"X3_5_26_0"] <- 1
data[data$X3_5_26_1 ==1,"X3_5_26_1"] <- 2
data[data$X3_5_26_2 ==1,"X3_5_26_2"] <- 3
data[data$X3_5_26_3 ==1,"X3_5_26_3"] <- 4

data[data$X3_5_27_0 ==1,"X3_5_27_0"] <- 1
data[data$X3_5_27_1 ==1,"X3_5_27_1"] <- 2
data[data$X3_5_27_2 ==1,"X3_5_27_2"] <- 3
data[data$X3_5_27_3 ==1,"X3_5_27_3"] <- 4






#### merging columns togethers

### section 1
age <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X1_1_0","X1_1_1","X1_1_2","X1_1_3","X1_1_4","X1_1_5","X1_1_6"),
                       variable.name="Response",
                       value.name="age")

age <- age %>% filter(age != 0) %>% select(-Response)

sex <- melt(data,
           id.vars="questionnaire_id",
           measure.vars=c("X1_2_0","X1_2_1"),
           variable.name="Response",
           value.name="sex")

sex <- sex %>% filter(sex != 0) %>% select(-Response)

birth_location <- melt(data,
           id.vars="questionnaire_id",
           measure.vars=c("X1_3_0","X1_3_1","X1_3_2","X1_3_3","X1_3_4","X1_3_5","X1_3_6","X1_3_7","X1_3_8","X1_3_9","X1_3_10"),
           variable.name="Response",
           value.name="birth_location")

birth_location <- birth_location %>% filter(birth_location != 0) %>% select(-Response)

citizenship <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X1_6_0","X1_6_1","X1_6_2","X1_6_3","X1_6_4"),
                       variable.name="Response",
                       value.name="citizenship")

citizenship <- citizenship %>% filter(citizenship != 0) %>% select(-Response)

current_muni <- melt(data,
                    id.vars="questionnaire_id",
                    measure.vars=c("X1_7_0","X1_7_1","X1_7_2","X1_7_3","X1_7_4"),
                    variable.name="Response",
                    value.name="current_muni")

current_muni <- current_muni %>% filter(current_muni != 0) %>% select(-Response)

time_fsm <- melt(data,
                     id.vars="questionnaire_id",
                     measure.vars=c("X1_9_0","X1_9_1","X1_9_2","X1_9_3","X1_9_4","X1_9_5"),
                     variable.name="Response",
                     value.name="time_fsm")

time_fsm <- time_fsm %>% filter(time_fsm != 0) %>% select(-Response)

time_pni <- melt(data,
                 id.vars="questionnaire_id",
                 measure.vars=c("X1_10_0","X1_10_1","X1_10_2","X1_10_3","X1_10_4","X1_10_5"),
                 variable.name="Response",
                 value.name="time_pni")

time_pni <- time_pni %>% filter(time_pni != 0) %>% select(-Response)

pni_current_place <- melt(data,
                 id.vars="questionnaire_id",
                 measure.vars=c("X1_11_0","X1_11_1","X1_11_2","X1_11_3","X1_11_4","X1_11_5"),
                 variable.name="Response",
                 value.name="pni_current_place")

pni_current_place <- pni_current_place %>% filter(pni_current_place != 0) %>% select(-Response)

travelled_abroad <- melt(data,
                          id.vars="questionnaire_id",
                          measure.vars=c("X1_12_0","X1_12_1"),
                          variable.name="Response",
                          value.name="travelled_abroad")

travelled_abroad <- travelled_abroad %>% filter(travelled_abroad != 0) %>% select(-Response)

education <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X1_14_0","X1_14_1","X1_14_2","X1_14_3","X1_14_4","X1_14_5","X1_14_6","X1_14_7","X1_14_8","X1_14_9","X1_14_10"),
                       variable.name="Response",
                       value.name="education")

education <- education %>% filter(education != 0) %>% select(-Response)

elementary_type <- melt(data,
                         id.vars="questionnaire_id",
                         measure.vars=c("X1_15_0","X1_15_1"),
                         variable.name="Response",
                         value.name="elementary_type")

elementary_type <- elementary_type %>% filter(elementary_type != 0) %>% select(-Response)

hs_type <- melt(data,
                         id.vars="questionnaire_id",
                         measure.vars=c("X1_16_0","X1_16_1"),
                         variable.name="Response",
                         value.name="hs_type")

hs_type <- hs_type %>% filter(hs_type != 0) %>% select(-Response)

children <- melt(data,
                  id.vars="questionnaire_id",
                  measure.vars=c("X1_17_0","X1_17_1","X1_17_2","X1_17_3","X1_17_4","X1_17_5","X1_17_6","X1_17_7","X1_17_8"),
                  variable.name="Response",
                  value.name="children")

children <- children %>% filter(children != 0) %>% select(-Response)
children$children[children$children =="zero"] <- 0

### section 2
meing <- melt(data,
                id.vars="questionnaire_id",
                measure.vars=c("X2_3_0","X2_3_1","X2_3_2","X2_3_3"),
                variable.name="Response",
                value.name="meing")

meing <- meing %>% filter(meing != 0) %>% select(-Response)

### section 3
making_friends <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_1_1_0","X3_1_1_1","X3_1_1_2","X3_1_1_3","X3_1_1_4","X3_1_1_5","X3_1_1_6","X3_1_1_7"),
              variable.name="Response",
              value.name="making_friends")

making_friends <- making_friends %>% filter(making_friends != 0) %>% select(-Response)

being_successful <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_2_0","X3_1_2_1","X3_1_2_2","X3_1_2_3","X3_1_2_4","X3_1_2_5","X3_1_2_6","X3_1_2_7"),
                       variable.name="Response",
                       value.name="being_successful")

being_successful <- being_successful %>% filter(being_successful != 0) %>% select(-Response)

good_education <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_3_0","X3_1_3_1","X3_1_3_2","X3_1_3_3","X3_1_3_4","X3_1_3_5","X3_1_3_6","X3_1_3_7"),
                       variable.name="Response",
                       value.name="good_education")

good_education <- good_education %>% filter(good_education != 0) %>% select(-Response)

happy_relationships <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_4_0","X3_1_4_1","X3_1_4_2","X3_1_4_3","X3_1_4_4","X3_1_4_5","X3_1_4_6","X3_1_4_7"),
                       variable.name="Response",
                       value.name="happy_relationships")

happy_relationships <- happy_relationships %>% filter(happy_relationships != 0) %>% select(-Response)

getting_money <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_5_0","X3_1_5_1","X3_1_5_2","X3_1_5_3","X3_1_5_4","X3_1_5_5","X3_1_5_6","X3_1_5_7"),
                       variable.name="Response",
                       value.name="getting_money")

getting_money <- getting_money %>% filter(getting_money != 0) %>% select(-Response)

reading <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_6_0","X3_1_6_1","X3_1_6_2","X3_1_6_3","X3_1_6_4","X3_1_6_5","X3_1_6_6","X3_1_6_7"),
                       variable.name="Response",
                       value.name="reading")

reading <- reading %>% filter(reading != 0) %>% select(-Response)

writing <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_7_0","X3_1_7_1","X3_1_7_2","X3_1_7_3","X3_1_7_4","X3_1_7_5","X3_1_7_6","X3_1_7_7"),
                       variable.name="Response",
                       value.name="writing")

writing <- writing %>% filter(writing != 0) %>% select(-Response)

radio <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_8_0","X3_1_8_1","X3_1_8_2","X3_1_8_3","X3_1_8_4","X3_1_8_5","X3_1_8_6","X3_1_8_7"),
                       variable.name="Response",
                       value.name="radio")

radio <- radio %>% filter(radio != 0) %>% select(-Response)

tv <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_9_0","X3_1_9_1","X3_1_9_2","X3_1_9_3","X3_1_9_4","X3_1_9_5","X3_1_9_6","X3_1_9_7"),
                       variable.name="Response",
                       value.name="tv")

tv <- tv %>% filter(tv != 0) %>% select(-Response)

accepted_pni <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_10_0","X3_1_10_1","X3_1_10_2","X3_1_10_3","X3_1_10_4","X3_1_10_5","X3_1_10_6","X3_1_10_7"),
                       variable.name="Response",
                       value.name="accepted_pni")

accepted_pni <- accepted_pni %>% filter(accepted_pni != 0) %>% select(-Response)

talking_teachers <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_1_11_0","X3_1_11_1","X3_1_11_2","X3_1_11_3","X3_1_11_4","X3_1_11_5","X3_1_11_6","X3_1_11_7"),
                       variable.name="Response",
                       value.name="talking_teachers")

talking_teachers <- talking_teachers %>% filter(talking_teachers != 0) %>% select(-Response)


talking_villages <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_2_1_0","X3_2_1_1","X3_2_1_2","X3_2_1_3","X3_2_1_4","X3_2_1_5","X3_2_1_6","X3_2_1_7"),
                       variable.name="Response",
                       value.name="talking_villages")

talking_villages <- talking_villages %>% filter(talking_villages != 0) %>% select(-Response)

funerals <- melt(data,
                         id.vars="questionnaire_id",
                         measure.vars=c("X3_2_2_0","X3_2_2_1","X3_2_2_2","X3_2_2_3","X3_2_2_4","X3_2_2_5","X3_2_2_6","X3_2_2_7"),
                         variable.name="Response",
                         value.name="funerals")

funerals <- funerals %>% filter(funerals != 0) %>% select(-Response)

kamadipw <- melt(data,
                       id.vars="questionnaire_id",
                       measure.vars=c("X3_2_3_0","X3_2_3_1","X3_2_3_2","X3_2_3_3","X3_2_3_4","X3_2_3_5","X3_2_3_6","X3_2_3_7"),
                       variable.name="Response",
                       value.name="kamadipw")

kamadipw <- kamadipw %>% filter(kamadipw != 0) %>% select(-Response)

sakau <- melt(data,
                            id.vars="questionnaire_id",
                            measure.vars=c("X3_2_4_0","X3_2_4_1","X3_2_4_2","X3_2_4_3","X3_2_4_4","X3_2_4_5","X3_2_4_6","X3_2_4_7"),
                            variable.name="Response",
                            value.name="sakau")

sakau <- sakau %>% filter(sakau != 0) %>% select(-Response)

facebook <- melt(data,
                      id.vars="questionnaire_id",
                      measure.vars=c("X3_2_5_0","X3_2_5_1","X3_2_5_2","X3_2_5_3","X3_2_5_4","X3_2_5_5","X3_2_5_6","X3_2_5_7"),
                      variable.name="Response",
                      value.name="facebook")

facebook <- facebook %>% filter(facebook != 0) %>% select(-Response)

talking_kolonia <- melt(data,
                id.vars="questionnaire_id",
                measure.vars=c("X3_2_6_0","X3_2_6_1","X3_2_6_2","X3_2_6_3","X3_2_6_4","X3_2_6_5","X3_2_6_6","X3_2_6_7"),
                variable.name="Response",
                value.name="talking_kolonia")

talking_kolonia <- talking_kolonia %>% filter(talking_kolonia != 0) %>% select(-Response)

talking_chief <- melt(data,
                id.vars="questionnaire_id",
                measure.vars=c("X3_2_7_0","X3_2_7_1","X3_2_7_2","X3_2_7_3","X3_2_7_4","X3_2_7_5","X3_2_7_6","X3_2_7_7"),
                variable.name="Response",
                value.name="talking_chief")

talking_chief <- talking_chief %>% filter(talking_chief != 0) %>% select(-Response)
#
talking_gov <- melt(data,
                         id.vars="questionnaire_id",
                         measure.vars=c("X3_3_1_0","X3_3_1_1","X3_3_1_2","X3_3_1_3","X3_3_1_4","X3_3_1_5","X3_3_1_6","X3_3_1_7"),
                         variable.name="Response",
                         value.name="talking_gov")

talking_gov <- talking_gov %>% filter(talking_gov != 0) %>% select(-Response)

good_job <- melt(data,
                 id.vars="questionnaire_id",
                 measure.vars=c("X3_3_2_0","X3_3_2_1","X3_3_2_2","X3_3_2_3","X3_3_2_4","X3_3_2_5","X3_3_2_6","X3_3_2_7"),
                 variable.name="Response",
                 value.name="good_job")

good_job <- good_job %>% filter(good_job != 0) %>% select(-Response)

friends_school <- melt(data,
                 id.vars="questionnaire_id",
                 measure.vars=c("X3_3_3_0","X3_3_3_1","X3_3_3_2","X3_3_3_3","X3_3_3_4","X3_3_3_5","X3_3_3_6","X3_3_3_7"),
                 variable.name="Response",
                 value.name="friends_school")

friends_school <- friends_school %>% filter(friends_school != 0) %>% select(-Response)

church <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_3_4_0","X3_3_4_1","X3_3_4_2","X3_3_4_3","X3_3_4_4","X3_3_4_5","X3_3_4_6","X3_3_4_7"),
              variable.name="Response",
              value.name="church")

church <- church %>% filter(church != 0) %>% select(-Response)

store <- melt(data,
                 id.vars="questionnaire_id",
                 measure.vars=c("X3_3_5_0","X3_3_5_1","X3_3_5_2","X3_3_5_3","X3_3_5_4","X3_3_5_5","X3_3_5_6","X3_3_5_7"),
                 variable.name="Response",
                 value.name="store")

store <- store %>% filter(store != 0) %>% select(-Response)

talking_neighbors <- melt(data,
                        id.vars="questionnaire_id",
                        measure.vars=c("X3_3_6_0","X3_3_6_1","X3_3_6_2","X3_3_6_3","X3_3_6_4","X3_3_6_5","X3_3_6_6","X3_3_6_7"),
                        variable.name="Response",
                        value.name="talking_neighbors")

talking_neighbors <- talking_neighbors %>% filter(talking_neighbors != 0) %>% select(-Response)

us_relatives <- melt(data,
                      id.vars="questionnaire_id",
                      measure.vars=c("X3_3_7_0","X3_3_7_1","X3_3_7_2","X3_3_7_3","X3_3_7_4","X3_3_7_5","X3_3_7_6","X3_3_7_7"),
                      variable.name="Response",
                      value.name="us_relatives")

us_relatives <- us_relatives %>% filter(us_relatives != 0) %>% select(-Response)

local_lang <- melt(data,
                     id.vars="questionnaire_id",
                     measure.vars=c("X3_4_1_0","X3_4_1_1"),
                     variable.name="Response",
                     value.name="local_lang")

local_lang <- local_lang %>% filter(local_lang != 0) %>% select(-Response)

english_more_import_local <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_2_0","X3_4_2_1"),
                   variable.name="Response",
                   value.name="english_more_import_local")

english_more_import_local <- english_more_import_local %>% filter(english_more_import_local != 0) %>% select(-Response)

english_smarter <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_3_0","X3_4_3_1"),
                   variable.name="Response",
                   value.name="english_smarter")

english_smarter <- english_smarter %>% filter(english_smarter != 0) %>% select(-Response)

eng_pni_diff <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_4_0","X3_4_4_1"),
                   variable.name="Response",
                   value.name="eng_pni_diff")

eng_pni_diff <- eng_pni_diff %>% filter(eng_pni_diff != 0) %>% select(-Response)

learn_pni_first <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_5_0","X3_4_5_1"),
                   variable.name="Response",
                   value.name="learn_pni_first")

learn_pni_first <- learn_pni_first %>% filter(learn_pni_first != 0) %>% select(-Response)

pni_more_import_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_6_0","X3_4_6_1"),
                   variable.name="Response",
                   value.name="pni_more_import_eng")

pni_more_import_eng <- pni_more_import_eng %>% filter(pni_more_import_eng != 0) %>% select(-Response)

pni_jobs_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_7_0","X3_4_7_1"),
                   variable.name="Response",
                   value.name="pni_jobs_pni")

pni_jobs_pni <- pni_jobs_pni %>% filter(pni_jobs_pni != 0) %>% select(-Response)


pni_jobs_abroad <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_8_0","X3_4_8_1"),
                   variable.name="Response",
                   value.name="pni_jobs_abroad")

pni_jobs_abroad <- pni_jobs_abroad %>% filter(pni_jobs_abroad != 0) %>% select(-Response)

eng_jobs_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_9_0","X3_4_9_1"),
                   variable.name="Response",
                   value.name="eng_jobs_pni")

eng_jobs_pni <- eng_jobs_pni %>% filter(eng_jobs_pni != 0) %>% select(-Response)

eng_jobs_abroad <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_10_0","X3_4_10_1"),
                   variable.name="Response",
                   value.name="eng_jobs_abroad")

eng_jobs_abroad <- eng_jobs_abroad %>% filter(eng_jobs_abroad != 0) %>% select(-Response)

many_lang_easy <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_11_0","X3_4_11_1"),
                   variable.name="Response",
                   value.name="many_lang_easy")

many_lang_easy <- many_lang_easy %>% filter(many_lang_easy != 0) %>% select(-Response)

many_lang_important <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_12_0","X3_4_12_1"),
                   variable.name="Response",
                   value.name="many_lang_important")

many_lang_important <- many_lang_important %>% filter(many_lang_important != 0) %>% select(-Response)

one_lang_life_diff <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_13_0","X3_4_13_1"),
                   variable.name="Response",
                   value.name="one_lang_life_diff")

one_lang_life_diff <- one_lang_life_diff %>% filter(one_lang_life_diff != 0) %>% select(-Response)

eng_more_import_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_14_0","X3_4_14_1"),
                   variable.name="Response",
                   value.name="eng_more_import_pni")

eng_more_import_pni <- eng_more_import_pni %>% filter(eng_more_import_pni != 0) %>% select(-Response)

sad.pni.no.pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_15_0","X3_4_15_1"),
                   variable.name="Response",
                   value.name="sad.pni.no.pni")

sad.pni.no.pni <- sad.pni.no.pni %>% filter(sad.pni.no.pni != 0) %>% select(-Response)

sad_pni_no_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_16_0","X3_4_16_1"),
                   variable.name="Response",
                   value.name="sad_pni_no_eng")

sad_pni_no_eng <- sad_pni_no_eng %>% filter(sad_pni_no_eng != 0) %>% select(-Response)

sad_pni_abroad <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_17_0","X3_4_17_1"),
                   variable.name="Response",
                   value.name="sad_pni_abroad")

sad_pni_abroad <- sad_pni_abroad %>% filter(sad_pni_abroad != 0) %>% select(-Response)

sad_eng_abroad <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_18_0","X3_4_18_1"),
                   variable.name="Response",
                   value.name="sad_eng_abroad")

sad_eng_abroad <- sad_eng_abroad %>% filter(sad_eng_abroad != 0) %>% select(-Response)

youths_bad_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_19_0","X3_4_19_1"),
                   variable.name="Response",
                   value.name="youths_bad_pni")

youths_bad_pni <- youths_bad_pni %>% filter(youths_bad_pni != 0) %>% select(-Response)

youths_bad_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_20_0","X3_4_20_1"),
                   variable.name="Response",
                   value.name="youths_bad_eng")

youths_bad_eng <- youths_bad_eng %>% filter(youths_bad_eng != 0) %>% select(-Response)


micros_need_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_21_0","X3_4_21_1"),
                   variable.name="Response",
                   value.name="micros_need_eng")

micros_need_eng <- micros_need_eng %>% filter(micros_need_eng != 0) %>% select(-Response)

pnis_need_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_22_0","X3_4_22_1"),
                   variable.name="Response",
                   value.name="pnis_need_eng")

pnis_need_eng <- pnis_need_eng %>% filter(pnis_need_eng != 0) %>% select(-Response)

kolonia_need_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_23_0","X3_4_23_1"),
                   variable.name="Response",
                   value.name="kolonia_need_eng")

kolonia_need_eng <- kolonia_need_eng %>% filter(kolonia_need_eng != 0) %>% select(-Response)

all_lang_live_together <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_24_0","X3_4_24_1"),
                   variable.name="Response",
                   value.name="all_lang_live_together")

all_lang_live_together <- all_lang_live_together %>% filter(all_lang_live_together != 0) %>% select(-Response)

pni_unfashionable <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_25_0","X3_4_25_1"),
                   variable.name="Response",
                   value.name="pni_unfashionable")

pni_unfashionable <- pni_unfashionable %>% filter(pni_unfashionable != 0) %>% select(-Response)

eng_more_val_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_26_0","X3_4_26_1"),
                   variable.name="Response",
                   value.name="eng_more_val_pni")

eng_more_val_pni <- eng_more_val_pni %>% filter(eng_more_val_pni != 0) %>% select(-Response)

micro_youth_like_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_27_0","X3_4_27_1"),
                   variable.name="Response",
                   value.name="micro_youth_like_eng")

micro_youth_like_eng <- micro_youth_like_eng %>% filter(micro_youth_like_eng != 0) %>% select(-Response)

micro_older_like_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_28_0","X3_4_28_1"),
                   variable.name="Response",
                   value.name="micro_older_like_eng")

micro_older_like_eng <- micro_older_like_eng %>% filter(micro_older_like_eng != 0) %>% select(-Response)

mehn_wai_need_learn_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_29_0","X3_4_29_1"),
                   variable.name="Response",
                   value.name="mehn_wai_need_learn_pni")

mehn_wai_need_learn_pni <- mehn_wai_need_learn_pni %>% filter(mehn_wai_need_learn_pni != 0) %>% select(-Response)

pni_youth_like_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_30_0","X3_4_30_1"),
                   variable.name="Response",
                   value.name="pni_youth_like_pni")

pni_youth_like_pni <- pni_youth_like_pni %>% filter(pni_youth_like_pni != 0) %>% select(-Response)

pni_older_like_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_31_0","X3_4_31_1"),
                   variable.name="Response",
                   value.name="pni_older_like_pni")

pni_older_like_pni <- pni_older_like_pni %>% filter(pni_older_like_pni != 0) %>% select(-Response)

pni_important_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_32_0","X3_4_32_1"),
                   variable.name="Response",
                   value.name="pni_important_pni")

pni_important_pni <- pni_important_pni %>% filter(pni_important_pni != 0) %>% select(-Response)

pni_simpler_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_33_0","X3_4_33_1"),
                   variable.name="Response",
                   value.name="pni_simpler_eng")

pni_simpler_eng <- pni_simpler_eng %>% filter(pni_simpler_eng != 0) %>% select(-Response)

choose_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_34_0","X3_4_34_1"),
                   variable.name="Response",
                   value.name="choose_pni")

choose_pni <- choose_pni %>% filter(choose_pni != 0) %>% select(-Response)

choose_eng <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_35_0","X3_4_35_1"),
                   variable.name="Response",
                   value.name="choose_eng")

choose_eng <- choose_eng %>% filter(choose_eng != 0) %>% select(-Response)

feel_positive_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_36_0","X3_4_36_1"),
                   variable.name="Response",
                   value.name="feel_positive_pni")

feel_positive_pni <- feel_positive_pni %>% filter(feel_positive_pni != 0) %>% select(-Response)

to_be_pni_speak_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_37_0","X3_4_37_1"),
                   variable.name="Response",
                   value.name="to_be_pni_speak_pni")

to_be_pni_speak_pni <- to_be_pni_speak_pni %>% filter(to_be_pni_speak_pni != 0) %>% select(-Response)

cant_pni_no_pni <- melt(data,
                   id.vars="questionnaire_id",
                   measure.vars=c("X3_4_38_0","X3_4_38_1"),
                   variable.name="Response",
                   value.name="cant_pni_no_pni")

cant_pni_no_pni <- cant_pni_no_pni %>% filter(cant_pni_no_pni != 0) %>% select(-Response)

quiet <- melt(data,
                        id.vars="questionnaire_id",
                        measure.vars=c("X3_5_1_0","X3_5_1_1","X3_5_1_2","X3_5_1_3"),
                        variable.name="Response",
                        value.name="quiet")

quiet <- quiet %>% filter(quiet != 0) %>% select(-Response)

stupid <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_2_0","X3_5_2_1","X3_5_2_2","X3_5_2_3"),
              variable.name="Response",
              value.name="stupid")

stupid <- stupid %>% filter(stupid != 0) %>% select(-Response)

loud <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_3_0","X3_5_3_1","X3_5_3_2","X3_5_3_3"),
              variable.name="Response",
              value.name="loud")

loud <- loud %>% filter(loud != 0) %>% select(-Response)

kindhearted <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_4_0","X3_5_4_1","X3_5_4_2","X3_5_4_3"),
              variable.name="Response",
              value.name="kindhearted")

kindhearted <- kindhearted %>% filter(kindhearted != 0) %>% select(-Response)

feminine <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_5_0","X3_5_5_1","X3_5_5_2","X3_5_5_3"),
              variable.name="Response",
              value.name="feminine")

feminine <- feminine %>% filter(feminine != 0) %>% select(-Response)

badtempered <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_6_0","X3_5_6_1","X3_5_6_2","X3_5_6_3"),
              variable.name="Response",
              value.name="badtempered")

badtempered <- badtempered %>% filter(badtempered != 0) %>% select(-Response)


masculine <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_7_0","X3_5_7_1","X3_5_7_2","X3_5_7_3"),
              variable.name="Response",
              value.name="masculine")

masculine <- masculine %>% filter(masculine != 0) %>% select(-Response)


honest <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_8_0","X3_5_8_1","X3_5_8_2","X3_5_8_3"),
              variable.name="Response",
              value.name="honest")

honest <- honest %>% filter(honest != 0) %>% select(-Response)


modern <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_9_0","X3_5_9_1","X3_5_9_2","X3_5_9_3"),
              variable.name="Response",
              value.name="modern")

modern <- modern %>% filter(modern != 0) %>% select(-Response)

attractive <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_10_0","X3_5_10_1","X3_5_10_2","X3_5_10_3"),
              variable.name="Response",
              value.name="attractive")

attractive <- attractive %>% filter(attractive != 0) %>% select(-Response)

successful <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_11_0","X3_5_11_1","X3_5_11_2","X3_5_11_3"),
              variable.name="Response",
              value.name="successful")

successful <- successful %>% filter(successful != 0) %>% select(-Response)


peaceful <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_12_0","X3_5_12_1","X3_5_12_2","X3_5_12_3"),
              variable.name="Response",
              value.name="peaceful")

peaceful <- peaceful %>% filter(peaceful != 0) %>% select(-Response)


violent <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_13_0","X3_5_13_1","X3_5_13_2","X3_5_13_3"),
              variable.name="Response",
              value.name="violent")

violent <- violent %>% filter(violent != 0) %>% select(-Response)


young <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_14_0","X3_5_14_1","X3_5_14_2","X3_5_14_3"),
              variable.name="Response",
              value.name="young")

young <- young %>% filter(young != 0) %>% select(-Response)


poor <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_15_0","X3_5_15_1","X3_5_15_2","X3_5_15_3"),
              variable.name="Response",
              value.name="poor")

poor <- poor %>% filter(poor != 0) %>% select(-Response)


old <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_16_0","X3_5_16_1","X3_5_16_2","X3_5_16_3"),
              variable.name="Response",
              value.name="old")

old <- old %>% filter(old != 0) %>% select(-Response)


rich <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_17_0","X3_5_17_1","X3_5_17_2","X3_5_17_3"),
              variable.name="Response",
              value.name="rich")

rich <- rich %>% filter(rich != 0) %>% select(-Response)

pretentious <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_18_0","X3_5_18_1","X3_5_18_2","X3_5_18_3"),
              variable.name="Response",
              value.name="pretentious")

pretentious <- pretentious %>% filter(pretentious != 0) %>% select(-Response)


proud <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_19_0","X3_5_19_1","X3_5_19_2","X3_5_19_3"),
              variable.name="Response",
              value.name="proud")

proud <- proud %>% filter(proud != 0) %>% select(-Response)


respectful <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_20_0","X3_5_20_1","X3_5_20_2","X3_5_20_3"),
              variable.name="Response",
              value.name="respectful")

respectful <- respectful %>% filter(respectful != 0) %>% select(-Response)


wise <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_21_0","X3_5_21_1","X3_5_21_2","X3_5_21_3"),
              variable.name="Response",
              value.name="wise")

wise <- wise %>% filter(wise != 0) %>% select(-Response)


patriotic <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_22_0","X3_5_22_1","X3_5_22_2","X3_5_22_3"),
              variable.name="Response",
              value.name="patriotic")

patriotic <- patriotic %>% filter(patriotic != 0) %>% select(-Response)


cultured <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_23_0","X3_5_23_1","X3_5_23_2","X3_5_23_3"),
              variable.name="Response",
              value.name="cultured")

cultured <- cultured %>% filter(cultured != 0) %>% select(-Response)


showoffs <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_24_0","X3_5_24_1","X3_5_24_2","X3_5_24_3"),
              variable.name="Response",
              value.name="showoffs")

showoffs <- showoffs %>% filter(showoffs != 0) %>% select(-Response)


humble <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_25_0","X3_5_25_1","X3_5_25_2","X3_5_25_3"),
              variable.name="Response",
              value.name="humble")

humble <- humble %>% filter(humble != 0) %>% select(-Response)

generous <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_26_0","X3_5_26_1","X3_5_26_2","X3_5_26_3"),
              variable.name="Response",
              value.name="generous")

generous <- generous %>% filter(generous != 0) %>% select(-Response)

uneducated <- melt(data,
              id.vars="questionnaire_id",
              measure.vars=c("X3_5_27_0","X3_5_27_1","X3_5_27_2","X3_5_27_3"),
              variable.name="Response",
              value.name="uneducated")

uneducated <- uneducated %>% filter(uneducated != 0) %>% select(-Response)


#### merge all together into new df
new_data2 <- Reduce(function(x, y) merge(x, y, all=T), list(age, sex, birth_location,citizenship,current_muni,time_fsm,time_pni,pni_current_place,travelled_abroad,education,elementary_type,hs_type,children,meing,making_friends,being_successful,good_education,happy_relationships,getting_money,reading,writing,radio,tv,accepted_pni,talking_teachers,talking_villages,funerals,kamadipw,sakau,facebook,talking_kolonia,talking_chief,talking_gov,good_job,friends_school,church,store,talking_neighbors,us_relatives,local_lang,english_more_import_local,english_smarter,eng_pni_diff,learn_pni_first,pni_more_import_eng,pni_jobs_pni,pni_jobs_abroad,eng_jobs_abroad,eng_jobs_pni,many_lang_easy,many_lang_important,one_lang_life_diff,eng_more_import_pni,sad.pni.no.pni,sad_pni_no_eng,sad_pni_abroad,sad_eng_abroad,youths_bad_pni,youths_bad_eng,micros_need_eng,pnis_need_eng,kolonia_need_eng,all_lang_live_together,pni_unfashionable,eng_more_val_pni,micro_youth_like_eng,micro_older_like_eng,mehn_wai_need_learn_pni,pni_youth_like_pni,pni_older_like_pni,pni_important_pni,pni_simpler_eng,choose_pni,choose_eng,feel_positive_pni,to_be_pni_speak_pni,cant_pni_no_pni,quiet,stupid,loud,kindhearted,feminine,badtempered,masculine,honest,modern,attractive,successful,peaceful,violent,young,poor,old,rich,pretentious,proud,respectful,wise,patriotic,cultured,showoffs,humble,generous,uneducated))
write.csv(new_data2,file="new_data2.csv")
### need to remove extras

### merge with other dataset
data_google <- read.csv("pni_lang_survey.csv")

data_google_selected <- data_google %>% select(questionnaire_id,age, sex, birth_location,citizenship,current_muni,current_village,time_fsm,time_pni,pni_current_place,travelled_abroad,education,elementary_type,hs_type,children,meing,making_friends,being_successful,good_education,happy_relationships,getting_money,reading,writing,radio,tv,accepted_pni,talking_teachers,talking_villages,funerals,kamadipw,sakau,facebook,talking_kolonia,talking_chief,talking_gov,good_job,friends_school,church,store,talking_neighbors,us_relatives,local_lang,english_more_import_local,english_smarter,eng_pni_diff,learn_pni_first,pni_more_import_eng,pni_jobs_pni,pni_jobs_abroad,eng_jobs_abroad,eng_jobs_pni,many_lang_easy,many_lang_important,one_lang_life_diff,eng_more_import_pni,sad.pni.no.pni,sad_pni_no_eng,sad_pni_abroad,sad_eng_abroad,youths_bad_pni,youths_bad_eng,micros_need_eng,pnis_need_eng,kolonia_need_eng,all_lang_live_together,pni_unfashionable,eng_more_val_pni,micro_youth_like_eng,micro_older_like_eng,mehn_wai_need_learn_pni,pni_youth_like_pni,pni_older_like_pni,pni_important_pni,pni_simpler_eng,choose_pni,choose_eng,feel_positive_pni,to_be_pni_speak_pni,cant_pni_no_pni,quiet,stupid,loud,kindhearted,feminine,masculine,honest,modern,attractive,successful,peaceful,violent,young,poor,old,rich,pretentious,proud,respectful,wise,patriotic,cultured,showoffs,humble,generous,uneducated)
write.csv(data_google_selected,file="google_selected.csv")

data_google$questionnaire_id <- as.factor(data_google$questionnaire_id)
read.csv("new_data2.csv")
survey_data_complete <- Reduce(function(x, y) merge(x, y, all=T), list(data_google_selected,new_data2))
write.csv(survey_data_complete,file="complete_data.csv",row.names=F)
