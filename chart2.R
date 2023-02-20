library(ggplot2)
library(dplyr)

bc_data <- read.csv("./cervicalcancerdata.csv", stringsAsFactor = FALSE)

bc_data <- bc_data %>% 
  mutate(combined_contraceptives = Hormonal.Contraceptives + IUD)  

bc_cancer <- bc_data %>% 
  filter(combined_contraceptives > 0) %>% 
  filter(Biopsy == 1)

ggplot(bc_cancer) +
  geom_point(aes(x = Age,
                y = Hormonal.Contraceptives..years.,
                color = Smokes)) +
  labs(title = "Years of Hormonal Contraceptives vs. Age",
       x = "Age",
       y = "Years of Hormonal Contraceptives",
       color = "People Smoke or not")
