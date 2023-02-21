library("dplyr")

table <- bc_data
table$Hormonal.Contraceptives..years. <- 
  round(table$Hormonal.Contraceptives..years.,2)
table$combined_years <- 
  round(table$combined_years,2)
table <- table %>%
  group_by(Age) %>%
  summarize(sum(Smokes), 
            sum(Hormonal.Contraceptives), 
            sum(Hormonal.Contraceptives..years.),
            sum(IUD),
            sum(IUD..years.),
            sum(Biopsy))


