library(tidyverse)
library(reshape2)
library(waffle)
library(cowplot)
library(ggforce)
library(showtext)


full_data = read.csv("data/processed/data_renamed.csv") %>%
  select(-X) %>%
  mutate(Q2.1 = replace(Q2.1, Q2.1 == 3, 4),
         Q2.1 = replace(Q2.1, is.na(Q2.1), 4))

full_data_all_genders = read.csv("data/processed/data_renamed.csv") %>%
  select(-X) %>%
  mutate(Q2.1 = replace(Q2.1, is.na(Q2.1), 4))

#colors
dark_blue = "#334152"
  soft_blue = "#d9e5ec"
    teal = "#009b91"
      
    
values_gender = c("männlich" = dark_blue, "weiblich" = teal,  "keine Angabe" = soft_blue)
names_gender = c("männlich", "weiblich","keine Angabe/divers")

values_gender_all = c("weiblich" = teal, "männlich" = dark_blue,  "keine Angabe" = soft_blue, divers = "grey95")

#lookup names
lookup_names = read.csv("data/processed/names_sauber.csv", header = TRUE) %>%
  mutate(long_to_short = paste0("\"", lang, "\"", " = ", "\"", kurz, "\""),
         short_to_long = paste0("\"", kurz, "\"", " = ", "\"", lang, "\""))

codes_gender = data.frame(code = c(2, 1, 4),
                          gender = c("männlich", "weiblich",  "keine Angabe"))

codes_all_gender = data.frame(code = c(2, 3, 1, 4),
                          gender = c("männlich", "divers", "weiblich",   "keine Angabe"))

codes_faculty = data.frame(code = c(1, 2, 3, 4, 5, 6, 7),
                           faculty = c("Architektur und Gestaltung", "Bauingenieurwesen", "Elektrotechnik und Informationstechnik", 
                                       "Informatik", "Maschinenbau", "Wirtschafts-, Kultur- und Rechtswissenschaften", "keine Angabe"))
main_question3 = c("Q3.1", "Q3.5", "Q3.9", "Q3.13", "Q3.17", "Q3.21", "Q3.25", "Q3.29", "Q3.33", "Q3.37", "Q3.41", "Q3.45", "Q3.49", "Q3.53", "Q3.57")
long_names3= lookup_names$lang[match(main_question3, lookup_names$kurz)]

main_question4 = c("Q4.1", "Q4.7", "Q4.13", "Q4.19", "Q4.25", "Q4.31", "Q4.37", "Q4.43", 
                  "Q4.49", "Q4.55", "Q4.61", "Q4.67", "Q4.73", "Q4.79")
long_names4= lookup_names$lang[match(main_question4, lookup_names$kurz)]

fem_acting = c("Q3.3A1",  "Q3.7A1", "Q3.11A1", "Q3.15A1", "Q3.19A1", "Q3.23A1", "Q3.27A1", "Q3.31A1", "Q3.35A1", "Q3.39A1", "Q3.43A1", "Q3.47A1", "Q3.51A1","Q3.55A1", "Q3.59A1",
        "Q3.3A3","Q3.7A3", "Q3.11A3", "Q3.15A3", "Q3.19A3", "Q3.23A3", "Q3.27A3", "Q3.31A3", "Q3.35A3", "Q3.39A3", "Q3.43A3", "Q3.47A3", "Q3.51A3","Q3.55A3", "Q3.59A3")
male_acting = c("Q3.3A2",  "Q3.7A2", "Q3.11A2", "Q3.15A2", "Q3.19A2", "Q3.23A2", "Q3.27A2", "Q3.31A2", "Q3.35A2", "Q3.39A2", "Q3.43A2", "Q3.47A2", "Q3.51A2","Q3.55A2", "Q3.59A2")
other_acting = c("Q3.3A4",  "Q3.7A4", "Q3.11A4", "Q3.15A4", "Q3.19A4", "Q3.23A4", "Q3.27A4", "Q3.31A4", "Q3.35A4", "Q3.39A4", "Q3.43A4", "Q3.47A4", "Q3.51A4","Q3.55A4", "Q3.59A4")

stud = c("Q3.2A1", "Q3.6A1", "Q3.10A1", "Q3.14A1", "Q3.18A1", "Q3.22A1", 
         "Q3.26A1", "Q3.30A1", "Q3.34A1", "Q3.38A1", "Q3.42A1", "Q3.46A1", "Q3.50A1", "Q3.54A1", "Q3.58A1")

doz = c("Q3.2A2", "Q3.6A2", "Q3.10A2", "Q3.14A2", "Q3.18A2", "Q3.22A2", 
        "Q3.26A2", "Q3.30A2", "Q3.34A2", "Q3.38A2", "Q3.42A2", "Q3.46A2", "Q3.50A2", "Q3.54A2", "Q3.58A2")

ang = c("Q3.2A3", "Q3.6A3", "Q3.10A3", "Q3.14A3", "Q3.18A3", "Q3.22A3", 
        "Q3.26A3", "Q3.30A3", "Q3.34A3", "Q3.38A3", "Q3.42A3", "Q3.46A3", "Q3.50A3", "Q3.54A3", "Q3.58A3")

scale_q3 = c("Q3.4", "Q3.8", "Q3.12", "Q3.16", "Q3.20", "Q3.24", "Q3.28", "Q3.32", 
             "Q3.36", "Q3.40", "Q3.44", "Q3.48", "Q3.52", "Q3.56", "Q3.60")


#takes from report
df_all_students = data.frame(faculty = c("Architektur und Gestaltung", "Bauingenieurwesen", 
                                         "Elektrotechnik und Informationstechnik", "Informatik", "Maschinenbau", 
                                         "Wirtschafts-, Kultur- und Rechtswissenschaften"),
                             total_female = c(187 + 117 + 58 + 25, 63 + 28 + 47 + 63,
                                              2 + 1 + 17 + 18 + 13 + 7 + 19, 38 + 57 + 56 + 16 + 17, 
                                              32 + 2 + 25 + 17 + 10 + 15 + 1 + 4 + 12, 
                                              154 + 167 + 64 + 36 + 28 + 14 + 32 + 22 + 28 + 29 + 29),
                             first_semester = c(38 + 21 + 17,
                                                22 + 17 +8,
                                                19 + 9 + 12 + 7 + 19,
                                                26 + 35 + 15 + 5,
                                                21 + 20 + 13 + 14 + 11 + 8,
                                               42 + 49 + 10 + 7 + 10 + 1),
                             total = c(301 + 155 + 92 + 31, 237 + 112 + 127 + 63,
                                       38 + 15 + 211 + 154 + 51 + 64 + 63, 271 + 263 + 109 + 90 + 45,
                                       16 + 276 + 29 + 252 + 63 + 5 + 52 + 99 + 29 + 51 + 57,
                                       315 + 260 + 112 + 46 + 57 + 31 + 39 + 27 + 60 + 1 + 39 + 44)) %>%
  mutate(total_other = total - total_female,
         total_wo_1 = total - first_semester)


#plot settings
font_add("swiss", "code/fonts/Swiss/TTF/tt0001m_.ttf")
showtext_auto()   

add_common_layout = function(fontsize = 15) {
  theme_void() %+replace%
    theme(axis.title = element_text(size = fontsize),
          legend.background = element_rect(fill='transparent', color = NA),
          legend.box.background = element_rect(fill='transparent', color = NA),
          legend.text = element_text(size = fontsize),
          panel.background = element_rect(fill = "transparent", colour = NA),  
          plot.margin = margin(0, 0, 0, 0, "cm"),
          plot.background = element_rect(fill = "transparent", colour = NA),
          strip.background = element_rect(fill = "transparent", color = NA),
          strip.text = element_text(size = fontsize, family = "swiss"),
          text = element_text(size = fontsize, family = "swiss"))
  
}

 
    
    