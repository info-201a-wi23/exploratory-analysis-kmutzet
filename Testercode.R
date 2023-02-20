library("dplyr")
library("ggplot2")

bc_data <- read.csv("/Users/tammynguyen/Desktop/Info201/exploratory-analysis-kmutzet/cervicalcancerdata.csv", stringsAsFactor = FALSE)

num_columns <- ncol(bc_data)

#Add column hormonal Contraceptives and iud together and call it contraceptive
bc_data <- bc_data %>% mutate(combined_contraceptives = Hormonal.Contraceptives + IUD) 

bc_data <- bc_data %>% mutate(combined_years = Hormonal.Contraceptives..years. + IUD..years.)  

#percentage of hormonal contraceptive users?
hc <- bc_data$combined_contraceptives

hc_percentage <- paste0(round(sum(hc == 1, na.rm = TRUE) / length(hc) * 100, 2), "%") 

#ratio of those under 30 using contraceptives
under_thirty <- bc_data %>% group_by(Age, combined_contraceptives) %>% filter(Age<30)
under_thirty_ratio <- paste0(round(nrow(under_thirty %>% filter(combined_contraceptives ==1)) / nrow(under_thirty) *100, 2), "%")

#What is the youngest age of birth control use and what is oldest? 
youngest_user <- bc_data %>% filter(combined_contraceptives ==1) %>% filter(Age == min(Age)) %>% pull(Age)

oldest_user <- bc_data %>% filter(combined_contraceptives ==1) %>% filter(Age == max(Age)) %>% pull(Age)

#How long have they been using it
youngest_time <- bc_data %>% filter(combined_contraceptives ==1) %>% filter(Age == min(Age)) %>% pull(combined_contraceptives..years.)

oldest_time <- bc_data %>% filter(combined_contraceptives ==1) %>% filter(Age == max(Age)) %>% pull(combined_contraceptives..years.)

# how many people have cancer that didnt take contraceptives? 
no_bc_cancer <- bc_data %>% filter(combined_contraceptives == 0, na.rm = TRUE) %>% filter(Biopsy == 1) 
no_bc_cancer <- nrow(no_bc_cancer)

# how many people have cancer that took the contraceptives?
bc_cancer <- bc_data %>% filter(combined_contraceptives > 0) %>% filter(Biopsy == 1)
bc_cancer <- nrow(bc_cancer)

#how does smoking, contraceptives, and biopsy connect?
smoke_bc <- bc_data %>% filter(Smokes == 1) %>% filter(combined_contraceptives > 0) %>% filter(Biopsy == 1)
smoke_bc <- nrow(smoke_bc)

#Plot graph with years of contraceptives (x) and cervical biopsy results
ggplot(bc_data) + geom_line(aes(x = combined_years, y = Biopsy))
# or
ggplot(bc_data) + geom_line(aes(x = Hormonal.Contraceptives..years., y = Biopsy))

#Plot graph with years of iud (x) and cer. cancer biopsy results (y)
ggplot(bc_data) + geom_line(aes(x = IUD..years., y = Biopsy))

#Age and cervical cancer biopsy results
ggplot(bc_data) + geom_line(aes(x = Age, y = Biopsy))

#Dot graph of ages using contraceptives 
ggplot(bc_data) + geom_line(aes(x = Age, y = total_contraceptives))

#





