library("dplyr")

table <- bc_data
table$Hormonal.Contraceptives..years. <- 
  round(table$Hormonal.Contraceptives..years.,2)
table$combined_years <- 
  round(table$combined_years,2)
table$IUD..years. <- round(table$IUD..years.,2)
table <- table %>% mutate(age_by_decade = round((Age/10-0.5),0))
table <- table %>%
  group_by(age_by_decade) %>% 
  summarize(sum(Smokes, na.rm = T), 
    sum(combined_contraceptives, na.rm = TRUE), 
    sum(combined_years, na.rm = TRUE),
    sum(Hormonal.Contraceptives, na.rm = TRUE), 
    sum(Hormonal.Contraceptives..years., na.rm = TRUE),
    sum(IUD, na.rm = TRUE),
    sum(IUD..years., na.rm = TRUE),
    sum(Biopsy, na.rm = TRUE))




#group_by(round((Age/10-0.5),0)) %>%
