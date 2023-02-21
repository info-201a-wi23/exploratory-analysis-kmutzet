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
  summarize(sum(Smokes, na.rm = T), 
    sum(combined_contraceptives, na.rm = TRUE), 
    sum(combined_years, na.rm = TRUE),
    sum(Hormonal.Contraceptives, na.rm = TRUE), 
    sum(Hormonal.Contraceptives..years., na.rm = TRUE),
    sum(IUD, na.rm = TRUE),
    sum(IUD..years., na.rm = TRUE),
    sum(Biopsy, na.rm = TRUE))
#table <- table %>% rename("sum(combined_contraceptives, na.rm = TRUE)" = "combined_contraceptives")
#table <- table %>% rename("sum(combined_years, na.rm = TRUE)" = "combined_years")
#table <- table %>% rename("sum(Hormonal.Contraceptives, na.rm = TRUE)" = "hormonal_contraceptives")
#table <- table %>% rename("sum(Hormonal.Contraceptives..years., na.rm = TRUE)" = "hormonal_contraceptives_years")
#table <- table %>% rename("sum(IUD, na.rm = TRUE)" = "IUD")
#table <- table %>% rename("sum(IUD..years., na.rm = TRUE)" = "IUD_years")
#table <- table %>% rename("sum(Biopsy, na.rm = TRUE)" = "Biopsy")

#group_by(round((Age/10-0.5),0)) %>%
