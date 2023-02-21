library("dplyr")

table <- bc_data
table$Hormonal.Contraceptives..years. <- 
  round(table$Hormonal.Contraceptives..years.,2)
table$combined_years <- 
  round(table$combined_years,2)
table <- table %>% mutate(age_by_decade = round((Age/10-0.5),0))
table <- table %>%
  group_by(age_by_decade) %>% 
  summarize(sum(Smokes), 
            sum(Hormonal.Contraceptives), 
            sum(Hormonal.Contraceptives..years.),
            sum(IUD),
            sum(IUD..years.),
            sum(Biopsy))




#group_by(round((Age/10-0.5),0)) %>%