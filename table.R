library("dplyr")

table <- bc_data
table$Hormonal.Contraceptives..years. <- 
  round(table$Hormonal.Contraceptives..years.,2)
table$combined_years <- 
  round(table$combined_years,2)
table$IUD..years. <- round(table$IUD..years.,2)
table <- table %>% mutate(age_by_decade = paste0(round((Age/10-0.5),0)*10,"s"))
table <- table %>%
  group_by(age_by_decade) %>% 
  summarize(smokes = sum(Smokes, na.rm = T), 
    combined_contraceptives = sum(combined_contraceptives, na.rm = TRUE), 
    combined_years = sum(combined_years, na.rm = TRUE),
    hormonal_contraceptives = sum(Hormonal.Contraceptives, na.rm = TRUE), 
    hormonal_contraceptives_years = sum(Hormonal.Contraceptives..years., na.rm = TRUE),
    iud = sum(IUD, na.rm = TRUE),
    iud_years = sum(IUD..years., na.rm = TRUE),
    biopsy = sum(Biopsy, na.rm = TRUE))

