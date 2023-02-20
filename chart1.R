library(ggplot2)
library(dplyr)

bc_data <- read.csv("./cervicalcancerdata.csv", stringsAsFactor = FALSE)

ggplot(bc_data) + 
  geom_point(aes(x = Hormonal.Contraceptives..years., y = Biopsy)) +
  labs(title = "Relations of Biopsy and years of getting hormonal contraceptives",
       x = "Years of hormonal contraceptives",
       y = "Get Biopsy")
  