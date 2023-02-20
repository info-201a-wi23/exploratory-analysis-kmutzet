# This file computes some calculations of the data file on cervical cancer biopsy 
# results and potential contributors.

library("dplyr")
library("ggplot2")

# This will read our birth control dataset in. 
bc_data <- read.csv("https://raw.githubusercontent.com/info-201a-wi23/exploratory-analysis-kmutzet/main/cervicalcancerdata.csv", stringsAsFactor = FALSE)

# Shows how many columns there is in the dataset. 
num_columns <- ncol(bc_data)

# Here we are adding two columns summarizing the number of contraceptives
# used. Combines hormonal contraceptives (aka the pill) with if they are using an IUD
# since an IUD counts as a contraceptive.
bc_data <- bc_data %>%
 mutate(combined_contraceptives = Hormonal.Contraceptives + IUD) 

bc_data <- bc_data %>%
 mutate(combined_years = Hormonal.Contraceptives..years. + IUD..years.)  

# This function extracts the used contraceptives and then
# uses those numbers to find the percentage of users. 
hc <- bc_data$combined_contraceptives

hc_percentage <- paste0(
    round(sum(hc == 1, na.rm = TRUE) / length(hc) * 100, 2), "%") 

# This creates a dataset that finds all of the users that are under the age of 30
# and then the function finds the ratio of birth control and being under that age. 
under_thirty <- bc_data %>%
 group_by(Age, combined_contraceptives) %>%
 filter(Age<30)
under_thirty_ratio <- paste0(round(nrow(under_thirty %>%
 filter(combined_contraceptives ==1)) / nrow(under_thirty) *100, 2), "%")

# This function finds the oldest and youngest ages that use birth control.
youngest_user <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == min(Age)) %>%
 pull(Age)

oldest_user <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == max(Age)) %>%
 pull(Age)

# This function uses the youngest and oldest age to find how long
# these people have been using contraceptives. 
youngest_time <- bc_data %>%
 filter(combined_contraceptives > 0) %>%
 filter(Age == min(Age)) %>%
 pull(combined_years)

oldest_time <- bc_data %>% 
 filter(combined_contraceptives > 0) %>%
 filter(Age == max(Age)) %>%
 pull(combined_years)

# This function provides the number of people who had cervical cancer without
# taking birth control. 
no_bc_cancer <- bc_data %>%
 filter(combined_contraceptives == 0, na.rm = TRUE) %>%
 filter(Biopsy == 1) 
no_bc_cancer <- nrow(no_bc_cancer)

# This function provides the number of people who had cervical cancer with
# taking birth control. 
bc_cancer <- bc_data %>% 
 filter(combined_contraceptives > 0) %>%
 filter(Biopsy == 1)
bc_cancer <- nrow(bc_cancer)

# This function looks at the highest year someone had an IUD that was connected 
# to cancer and reports back the years they had the contraceptive. 
iud_highest_years <- bc_data %>%
 filter(IUD..years. > 0) %>% 
 filter(Biopsy == 1) %>% 
 filter(IUD..years. == max(IUD..years.)) %>% 
 pull(IUD..years.)

# This function looks at the highest year someone had been taking 
# hormonal contraceptive pills that was connected 
# to cancer and reports back the years they used it. 
pill_highest_years <-  bc_data %>% 
 filter(Hormonal.Contraceptives..years. > 0) %>% 
 filter(Biopsy == 1) %>% 
 filter(Hormonal.Contraceptives..years. == max(Hormonal.Contraceptives..years.)) %>% 
 pull(Hormonal.Contraceptives..years.)

# This function grabs both the highest years of IUD and pills and finds 
# the difference between the pill years and IUD years. 
difference_iud_pill <- pill_highest_years - iud_highest_years

# This function compares to the risk of smoking while on contraceptives 
# that got linked to cancer. The output is how many people did both and 
# ended up with cancer.
smoke_bc <- bc_data %>% 
 filter(Smokes == 1) %>% 
 filter(combined_contraceptives > 0) %>% 
 filter(Biopsy == 1)
smoke_bc <- nrow(smoke_bc)


