library(ggplot2)
library(dplyr)

bc_data <- read.csv("./cervicalcancerdata.csv", stringsAsFactor = FALSE)

ggplot(bc_data) + 
  geom_histogram(aes(x = Hormonal.Contraceptives..years.)) +
  labs(title = "Years of getting hormonal contraceptives",
       x = "Years of hormonal contraceptives")
  