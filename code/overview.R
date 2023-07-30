library(tidyverse)
library(reshape2)
library(stringr)
library(cowplot)
library(ggforce)
library(ggdist)
library(ggridges)
setwd("~/03_Outreach/dataviz/konstanz")

full_data = read.csv("data/processed/data_renamed.csv") %>%
  select(-X)

#utils
dark_blue = "#334152"
soft_blue = "#d9e5ec"
teal = "#009b91"

lookup_names = read.csv("data/processed/names_sauber.csv", header = TRUE) %>%
  mutate(long_to_short = paste0("\"", lang, "\"", " = ", "\"", kurz, "\""),
         short_to_long = paste0("\"", kurz, "\"", " = ", "\"", lang, "\""))

codes_gender = data.frame(code = c(1, 2, 3, 4),
                          gender = c("weiblich", "männlich", "divers", "keine Angabe"))

codes_faculty = data.frame(code = c(1, 2, 3, 4, 5, 6, 7),
                          faculty = c("Architektur und Gestaltung", "Bauingenieurwesen", "Elektrotechnik und Informationstechnik", 
                                     "Informatik", "Maschinenbau", "Wirtschafts-, Kultur- und Rechtswissenschaften", "keine Angabe"))



#Mit welchem Geschlecht identifizieren Sie sich?

df_gender = full_data %>%
  select(Q2.1) %>%
  filter(!is.na(Q2.1)) %>%
  melt(variable.name = "1") %>%
  mutate(gender = codes_gender$gender[match(value, codes_gender$code)])

df_gender$gender = factor(df_gender$gender,
                          levels = codes_gender$gender)

df = table(df_gender$gender) %>%
  melt(value.name = "count", variable.name = "gender") %>%
  rename("gender" = "Var1") %>%
  mutate(fraction = count/sum(count)) %>%
  mutate(ymax = cumsum(fraction))
df = df %>%
  mutate(ymin = c(0, head(df$ymax, n = -1)))

(p1 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) + theme_void() +
  scale_y_continuous(name = "") +
  scale_x_continuous(limits = c(1.5, 4)) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") +
  coord_polar(theta = "y") +
  geom_rect( color = "grey50", linewidth = .05) +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.direction = "vertical"))
ggsave("reports/figures/geschlecht.png", dpi = 700)

#An welcher Fakultät studieren Sie?
df_all_students = data.frame(faculty = c("Architektur und Gestaltung", "Bauingenieurwesen", 
                                         "Elektrotechnik und Informationstechnik", "Informatik", "Maschinenbau", 
                                         "Wirtschafts-, Kultur- und Rechtswissenschaften"),
                             total_female = c(187 + 117 + 58 + 25, 63 + 28 + 47 + 63,
                                        2 + 1 + 17 + 18 + 13 + 7 + 19, 38 + 57 + 56 + 16 + 17, 
                                        32 + 2 + 25 + 17 + 10 + 15 + 1 + 4 + 12, 
                                        154 + 167 + 64 + 36 + 28 + 14 + 32 + 22 + 28 + 29 + 29),
                             total = c(301 + 155 + 92 + 31, 237 + 112 + 127 + 63,
                                       38 + 15 + 211 + 154 + 51 + 64 + 63, 271 + 263 + 109 + 90 + 45,
                                       16 + 276 + 29 + 252 + 63 + 5 + 52 + 99 + 29 + 51 + 57,
                                       315 + 260 + 112 + 46 + 57 + 31 + 39 + 27 + 60 + 1 + 39 + 44)) %>%
  mutate(total_other = total - total_female)
                             
                             
df_faculty = full_data %>%
  select(Q2.2, Q2.1) %>%
  filter(!is.na(Q2.2),
         !is.na(Q2.1)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  group_by(faculty, gender) %>%
  summarize(value = n()) %>%
  dcast(faculty ~ gender) %>%
  full_join(df_all_students) %>%
  replace(is.na(.), 0) %>%
  rename("m" = "männlich",
         "kA" = "keine Angabe") %>%
  mutate(share_female = weiblich/total_female,
         share_other = (m + kA + divers)/total_other) %>%
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>%
  rename("männlich" = "m",
         "keine Angabe" = "kA") %>%
  melt(measure.vars = c("share_female", "share_other"), variable.name = "share", value.name = "share_value") %>%
  melt(measure.vars = codes_gender$gender, variable.name = "gender", value.name = "students") %>%
  mutate(share_value = - share_value*100)
  
df_faculty$faculty = factor(df_faculty$faculty,
                          levels = rev(codes_faculty$faculty))

df_faculty$gender = factor(df_faculty$gender,
                          levels = rev(codes_gender$gender))

df_faculty$share = factor(df_faculty$share, level = rev(c("share_female", "share_other")))

(p2 = ggplot(data = df_faculty, aes(x = faculty)) + theme_void() + 
  geom_bar(aes(y = students, fill = gender), stat = "identity", width = 0.7) +
  geom_bar(aes(y = share_value, fill = share), stat = "identity", alpha = .4,  width = 0.7) +
  geom_text(aes(y = 0, label = faculty, x = factor(faculty)), size = 5,
            hjust = 0, nudge_x = 0.5, check_overlap = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0,0), breaks = c(200, 400), limits = c(-300, 500)) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50", 
                               "share_female" = dark_blue, "share_other" = soft_blue),
                    name = "") +
  theme(axis.text.x = element_text(color = "grey20"),
        axis.line.x = element_line(),
        legend.position = "None",
        text = element_text(size = 12)) +
  guides(fill = guide_legend(reverse=T)))

ggsave("reports/figures/fakultät.png", dpi = 700)


#what happended
main_question = c("Q3.1", "Q3.5", "Q3.9", "Q3.13", "Q3.17", "Q3.21", "Q3.25", "Q3.29", "Q3.33", "Q3.37", "Q3.41", "Q3.45", "Q3.49", "Q3.53", "Q3.57")
long_names= lookup_names$lang[match(main_question, lookup_names$kurz)]

#absolute values
df_incident = full_data %>% 
  select(all_of(main_question), "Q2.1") %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) %>%
  filter_all(any_vars(. == 1)) %>%
  melt(id.vars = c("gender")) %>%
  filter(!is.na(value),
         !is.na(variable),
         !is.na(gender))

ggplot() + theme_void() +
  coord_flip() +
  scale_x_discrete(labels =str_wrap(str_replace_all(long_names, "foo" , " "), width = 70), expand = c(0,0)) + #induce line break
  scale_y_continuous(expand = c(0,0), limits = c(0, 160), breaks = c(50, 100, 150)) +
  geom_bar(data = df_incident[df_incident$value == 1,], stat = "identity", 
           aes(x = variable, y = value, fill = gender)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"), name = "")
  
ggsave("reports/figures/wasistpassiert.png", dpi = 700)

#zeugen

main_question = c("Q4.1", "Q4.7", "Q4.13", "Q4.19", "Q4.25", "Q4.31", "Q4.37", "Q4.43", 
                  "Q4.49", "Q4.55", "Q4.61", "Q4.67", "Q4.73", "Q4.79")
long_names= lookup_names$lang[match(main_question, lookup_names$kurz)]

#absolute values
df_whitness = full_data %>% 
  select(all_of(main_question), "Q2.1") %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) %>%
  filter_all(any_vars(. == 1)) %>%
  melt(id.vars = c("gender")) %>%
  filter(!is.na(value),
         !is.na(variable),
         !is.na(gender))

ggplot() + theme_void() +
  coord_flip() +
  scale_x_discrete(labels =str_wrap(str_replace_all(long_names, "foo" , " "), width = 70), expand = c(0,0)) + #induce line break
  scale_y_continuous(expand = c(0,0), limits = c(0, 160), breaks = c(50, 100, 150)) +
  geom_bar(data = df_whitness[df_whitness$value == 1,], stat = "identity", 
           aes(x = variable, y = value, fill = gender)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"), name = "")

ggsave("reports/figures/zeugen.png", dpi = 700)



##zeugen und erlebtes
q3q4_paare = read.csv("data/processed/Q3_Q4_Paare.csv")

df_whitness_join = df_whitness %>%
  filter(value == 1) %>%
  count(gender, variable) %>%
  rename("Q4_n" = n,
         "variable_Q4" = variable)
  
df_incident_join = df_incident %>%
  filter(value == 1) %>%
  count(gender, variable) %>%
  rename("Q3_n" = n, 
         "variable_Q3" = variable) %>%
  mutate(variable_Q4 = q3q4_paare$Q4[match(variable_Q3, q3q4_paare$Q3)],
         name = q3q4_paare$gesehen[match(variable_Q3, q3q4_paare$Q3)]) %>%
  left_join(df_whitness_join) %>%
  mutate(Q3_n = -Q3_n) %>%
  filter(!is.na(Q4_n))

# Barplot

(ggplot(data = df_incident_join, aes(x = reorder(name, Q3_n, sum))) + theme_void() +
  geom_bar(aes(y = Q3_n, fill = gender), stat = "identity", width = 0.6) +
  geom_bar(aes(y = Q4_n, fill = gender), stat = "identity", width = 0.6) +
  geom_hline(yintercept = 0, color = "white", linewidth = .8) +
  geom_text(aes(y = 0, label =name, x = factor(name)), size = 4,
            hjust = .5, nudge_x = -0.45, check_overlap = TRUE) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(-155, 155), breaks = c(0, 100, 150)) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") +
  theme(axis.text.x = element_text(color = "grey20"),
        axis.line.x = element_line(color = "grey20"),
        legend.position = "None",
        text = element_text(size = 12)) +
  guides(fill = guide_legend(reverse=T)))


ggsave("reports/figures/q3q4.png", dpi = 700, width = 10, height = 8)


#wer sieht, wem passiert?

main_question_q3 = c("Q3.1", "Q3.5", "Q3.9", "Q3.13", "Q3.17", "Q3.21", "Q3.25", "Q3.29", 
                  "Q3.33", "Q3.37", "Q3.41", "Q3.45", "Q3.49", "Q3.53", "Q3.57")
main_question_q4 = c("Q4.1", "Q4.7", "Q4.13", "Q4.19", "Q4.25", "Q4.31", "Q4.37", "Q4.43", 
                  "Q4.49", "Q4.55", "Q4.61", "Q4.67", "Q4.73", "Q4.79")
df_groups = full_data %>%
  select(Q2.1,  all_of(main_question_q3), all_of(main_question_q4)) %>%
  mutate(q3 = ifelse(if_any(main_question_q3, ~ .x == 1),  1, 0),
         q4 = ifelse(if_any(main_question_q4, ~ .x == 1),  1, 0),
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) 

ggplot() + theme_void() +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = .5) +
  scale_x_reverse() +
  geom_point(data = df_groups, position = position_jitter(),
             aes(x = q3, y = q4, fill = gender), color = "grey20", shape = 21, stroke = .2, size = 3.5, alpha = .9) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") 

ggsave("reports/figures/q3vsq4.png", dpi = 700, width = 7, height = 6)


#handelnde Person
codes_person = data.frame(code = c())

main_question_q3 = c("Q3.1", "Q3.5", "Q3.9", "Q3.13", "Q3.17", "Q3.21", "Q3.25", "Q3.29", 
                     "Q3.33", "Q3.37", "Q3.41", "Q3.45", "Q3.49", "Q3.53", "Q3.57")
main_question_q4 = c("Q4.1", "Q4.7", "Q4.13", "Q4.19", "Q4.25", "Q4.31", "Q4.37", "Q4.43", 
                     "Q4.49", "Q4.55", "Q4.61", "Q4.67", "Q4.73", "Q4.79")

stud = c("Q3.2A1", "Q3.6A1", "Q3.10A1", "Q3.14A1", "Q3.18A1", "Q3.22A1", 
                "Q3.26A1", "Q3.30A1", "Q3.34A1", "Q3.38A1", "Q3.42A1", "Q3.46A1", "Q3.50A1", "Q3.54A1", "Q3.58A1")

doz = c("Q3.2A2", "Q3.6A2", "Q3.10A2", "Q3.14A2", "Q3.18A2", "Q3.22A2", 
                "Q3.26A2", "Q3.30A2", "Q3.34A2", "Q3.38A2", "Q3.42A2", "Q3.46A2", "Q3.50A2", "Q3.54A2", "Q3.58A2")

ang = c("Q3.2A3", "Q3.6A3", "Q3.10A3", "Q3.14A3", "Q3.18A3", "Q3.22A3", 
                "Q3.26A3", "Q3.30A3", "Q3.34A3", "Q3.38A3", "Q3.42A3", "Q3.46A3", "Q3.50A3", "Q3.54A3", "Q3.58A3")

df_offender = full_data %>%
 select(Q2.1, stud, doz, ang) %>%
  group_by(Q2.1) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  filter(!is.na(Q2.1)) %>%
  mutate("Mitstudierende*r" = rowSums(across(stud)),
         "Dozierende*r" = rowSums(across(doz)),
         "HTWG Angestellte*r" = rowSums(across(ang)),
         "gender" = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select("gender", "Mitstudierende*r", "Dozierende*r", "HTWG Angestellte*r") %>%
  melt(id.vars = "gender")

df_offender$variable = factor(df_offender$variable, levels = c("Mitstudierende*r", "Dozierende*r",  "HTWG Angestellte*r"))
df_offender$gender = factor(df_offender$gender, levels = codes_gender$gender)

p1 = ggplot() + theme_void() +
  geom_bar(data = df_offender[df_offender$gender %in% c("weiblich", "männlich"),], aes(x = gender, y = value, fill = variable), stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Mitstudierende*r" = dark_blue, "HTWG Angestellte*r" = soft_blue, "Dozierende*r" = teal),
                   name = "Handelnde Person") +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0 %", "50 %", "100 %")) +
  scale_x_discrete(name = "Betroffene Person") +
  theme(axis.text = element_text(),
        axis.title.x = element_text())

p2 = ggplot() + theme_void() +
  geom_bar(data = df_offender, aes(x = gender, y = value, fill = variable), stat = "identity") +
  scale_fill_manual(values = c("Mitstudierende*r" = dark_blue, "HTWG Angestellte*r" = soft_blue, "Dozierende*r" = teal),
                    name = "Handelnde Person") +
  scale_x_discrete(name = "Betroffene Person") +
  theme(axis.text = element_text(),
        axis.title.x = element_text())

df_offender_summarized = full_data %>%
  select(Q2.1, stud, doz, ang) %>%
  group_by(Q2.1) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  filter(!is.na(Q2.1)) %>%
  mutate("Mitstudierende*r" = rowSums(across(stud)),
         "Dozierende*r" = rowSums(across(doz)),
         "HTWG Angestellte*r" = rowSums(across(ang)),
         "gender" = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select("Mitstudierende*r", "Dozierende*r", "HTWG Angestellte*r") %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  melt() %>% 
  mutate(fraction = value/sum(value)) %>%
  mutate(ymax = cumsum(fraction))
df = df_offender_summarized %>%
  mutate(ymin = c(0, head(df_offender_summarized$ymax, n = -1)))

(p3 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable)) + theme_void() +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4)) +
    scale_fill_manual(values = c("Mitstudierende*r" = dark_blue, "HTWG Angestellte*r" = soft_blue, "Dozierende*r" = teal),
                      name = "Handelnde Person") +
    coord_polar(theta = "y") +
    geom_rect( color = "grey50", linewidth = .05) +
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "vertical"))

(p = plot_grid(p3, p2, p1, nrow = 1))
ggsave("reports/figures/handelne_personen.png", dpi = 700, width = 15) 

#was ist passiert pro Fakultät

df_faculty_incident = full_data %>%
  select(Q2.2, Q2.1, main_question_q3) %>%
  filter(!is.na(Q2.2),
         !is.na(Q2.1)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  filter_at(vars(main_question_q3), any_vars (. == 1)) %>%
  select(-Q2.2, -Q2.1) %>%
  melt(id.vars = c("gender", "faculty")) %>%
  filter(value == 1) %>%
  count(faculty, gender) 

p1 = ggplot() + theme_void() + 
  geom_bar(data = df_faculty_incident, aes(x = faculty, y = n, fill = gender), stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") +
  scale_y_continuous(name = "Anzahl der Betroffenen") +
  theme(axis.text = element_text(hjust = 1),
        axis.title.x = element_text())


df_faculty_affected = full_data %>%
  select(Q2.2, Q2.1, main_question_q3) %>%
  filter(!is.na(Q2.2),
         !is.na(Q2.1)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  filter_at(vars(main_question_q3), any_vars (. == 1)) %>%
  select(-Q2.2, -Q2.1) %>%
  count(faculty, gender)

df_faculty_all = full_data %>%
  select(Q2.2, Q2.1) %>%
  filter(!is.na(Q2.2),
         !is.na(Q2.1)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  count(faculty, gender) %>%
  rename(n_all = n) %>%
  left_join(df_faculty_affected) %>%
  mutate(share = n/n_all) %>%
  filter(gender %in% c("männlich", "weiblich"),
         faculty != "keine Angabe")


p2 = ggplot() + theme_void() + 
  geom_bar(data = df_faculty_all, aes(x = faculty, y = share, fill = gender), stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") +
  scale_y_continuous(breaks = c(0, 0.25, 0.5), labels = c("0 %", "25 %", "50 %"), name = "Prozent der Befragten") +
  theme(axis.text = element_text(hjust = 1),
        axis.title.x = element_text())

(p = plot_grid(p1, p2, nrow = 2))
ggsave("reports/figures/vergleich_fakultäten.png", dpi = 700) 


#wie unangenehm?
main_question_q3 = c("Q3.1", "Q3.5", "Q3.9", "Q3.13", "Q3.17", "Q3.21", "Q3.25", "Q3.29", 
                    "Q3.33", "Q3.37", "Q3.41", "Q3.45", "Q3.49", "Q3.53", "Q3.57")
scale_q3 = c("Q3.4", "Q3.8", "Q3.12", "Q3.16", "Q3.20", "Q3.24", "Q3.28", "Q3.32", 
                    "Q3.36", "Q3.40", "Q3.44", "Q3.48", "Q3.52", "Q3.56", "Q3.60")

df_scale = full_data %>%
  select(Q2.1,  all_of(scale_q3)) %>%
  filter_at(vars(scale_q3), any_vars (!is.na(.))) %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) 

names(df_scale) = c(main_question_q3, "gender")

df_scale = df_scale %>%
  melt(id.vars = "gender") %>%
  filter(!is.na(value),
         !is.na(gender)) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(variable, gender) %>%
  summarize(mean = mean(value),
            sd = sd(value)) %>%
  filter(gender %in% c("männlich", "weiblich"))
  

ggplot(data = df_scale, aes(x = variable, y = mean, color = gender)) + theme_void() +
  geom_pointrange(aes(ymin = -sd, ymax = sd)) +
  coord_flip() +
  scale_color_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"),
                    name = "") +
  theme(axis.line.x = element_line(),
        axis.text = element_text())

