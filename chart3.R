library(ggplot2)
library(dplyr)

bc_data <- read.csv("./cervicalcancerdata.csv", stringsAsFactor = FALSE)

ggplot(bc_data) +
  geom_line(aes(x = Age,
                y = IUD..years.,
                color = Biopsy)) +
  labs(title = "Years of IUD vs. Age based on Biopsy",
       x = "Age",
       y = "Years of taking IUD",
       color = "Taking Biopsy or not")
