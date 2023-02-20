# This file computes some calculations of the data file on cervical cancer biopsy 
# results and potential contributors.

library("dplyr")
library("ggplot2")

bc_data <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-kmutzet/main/cervicalcancerdata.csv", stringsAsFactor = FALSE)

num_columns <- ncol(bc_data)

# Here we are adding two columns summarizing the number of homronal contraceptives
# used. Combines hormonal contraceptives (aka the pill) with if they are using an IUD
# since an IUD counts as a contraceptive.
bc_data <- bc_data %>%
 mutate(combined_contraceptives = Hormonal.Contraceptives + IUD) 

bc_data <- bc_data %>%
 mutate(combined_years = Hormonal.Contraceptives..years. + IUD..years.)  

#percentage of hormonal contraceptive users?
hc <- bc_data$combined_contraceptives

hc_percentage <- paste0(
    round(sum(hc == 1, na.rm = TRUE) / length(hc) * 100, 2), "%") 

#ratio of those under 30 using contraceptives
under_thirty <- bc_data %>%
 group_by(Age, combined_contraceptives) %>%
 filter(Age<30)
under_thirty_ratio <- paste0(round(nrow(under_thirty %>%
 filter(combined_contraceptives ==1)) / nrow(under_thirty) *100, 2), "%")

#What is the youngest age of birth control use and what is oldest? 
youngest_user <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == min(Age)) %>%
 pull(Age)

oldest_user <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == max(Age)) %>%
 pull(Age)

#How long have they been using it
youngest_time <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == min(Age)) %>%
 pull(combined_years)

oldest_time <- bc_data %>% 
 filter(combined_contraceptives > 0) %>%
 filter(Age == max(Age)) %>%
 pull(combined_years)

# how many people have cancer that didnt take contraceptives? 
no_bc_cancer <- bc_data %>%
 filter(combined_contraceptives == 0, na.rm = TRUE) %>%
 filter(Biopsy == 1) 
no_bc_cancer <- nrow(no_bc_cancer)

# how many people have cancer that took the contraceptives?
bc_cancer <- bc_data %>% 
 filter(combined_contraceptives > 0) %>%
 filter(Biopsy == 1)
bc_cancer <- nrow(bc_cancer)

# what is the difference between iud year and cancer vs pill years and 
# cancer?
iud_highest_years <- bc_data %>%
 filter(IUD..years. > 0) %>% 
 filter(Biopsy == 1) %>% 
 filter(IUD..years. == max(IUD..years.)) %>% 
 pull(IUD..years.)

pill_highest_years <-  bc_data %>% 
 filter(Hormonal.Contraceptives..years. > 0) %>% 
 filter(Biopsy == 1) %>% 
 filter(Hormonal.Contraceptives..years. == max(Hormonal.Contraceptives..years.)) %>% 
 pull(Hormonal.Contraceptives..years.)

difference_iud_pill <- pill_highest_years - iud_highest_years

#how does smoking, contraceptives, and biopsy connect?
smoke_bc <- bc_data %>% 
 filter(Smokes == 1) %>% 
 filter(combined_contraceptives > 0) %>% 
 filter(Biopsy == 1)
smoke_bc <- nrow(smoke_bc)





