library(tidyverse)
library(reshape2)
library(stringr)
library(cowplot)

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
  mutate(fraction = count/sum(count))
df = df %>%
  mutate(ymax = cumsum(fraction)) %>%
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

(p2 = ggplot(data = df_faculty, aes(x = faculty)) + theme_void() + ggtitle("An welcher Fakultät studieren Sie?") +
  geom_bar(aes(y = students, fill = gender), stat = "identity", width = 0.7) +
  geom_bar(aes(y = share_value, fill = share), stat = "identity", alpha = .4,  width = 0.7) +
  geom_text(aes(y = 0, label = faculty, x = factor(faculty)), size = 5,
            hjust = 0, nudge_x = 0.5, check_overlap = TRUE) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0,0), breaks = c(200, 400)) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50", 
                               "share_female" = dark_blue, "share_other" = soft_blue),
                    name = "") +
  theme(axis.text.x = element_text(color = "grey20"),
        axis.line.x = element_line(),
        legend.position = "None",
        text = element_text(size = 12)) +
  guides(fill = guide_legend(reverse=T)))

ggsave("reports/figures/fakultät.png", dpi = 700)

plot_grid(p1, p2, rel_widths = c(1,2))

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
  scale_y_continuous(expand = c(0,0), limits = c(0, 200), breaks = c(50, 100, 150)) +
  geom_bar(data = df_incident[df_whitness$value == 1,], stat = "identity", 
           aes(x = variable, y = value, fill = gender)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"), name = "")
  annotate("text", x = 3, y = 155, label = "60 %  berichten dass in ihrer Nähe unangenehme Kommentare \nmit sexuell aufgeladenen inhalten gemacht wurden", color = "grey20") +
  annotate("text", x = 15, y = 50, label = "X Personen berichten von sexuell aufgeladenen Kommentaren", color = "grey20")

  
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
  scale_y_continuous(expand = c(0,0), limits = c(0, 200), breaks = c(50, 100, 150)) +
  geom_bar(data = df_whitness[df_whitness$value == 1,], stat = "identity", 
           aes(x = variable, y = value, fill = gender)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_fill_manual(values = c("weiblich" = dark_blue, "männlich" = soft_blue, "divers" = teal, "keine Angabe" = "grey50"), name = "")
annotate("text", x = 3, y = 155, label = "60 %  berichten dass in ihrer Nähe unangenehme Kommentare \nmit sexuell aufgeladenen inhalten gemacht wurden", color = "grey20") +
  annotate("text", x = 15, y = 50, label = "X Personen berichten von sexuell aufgeladenen Kommentaren", color = "grey20")

ggsave("reports/figures/zeugen.png", dpi = 700)

#textlabel mit häufigster und seltenste idee
#combinations? can't put it in same plot, it will be way to many -> maybe string plot just to show which ones appear together?
#cluster analysis? which questions cluster?
#circular graph: from weightened adjacency matrix https://stackoverflow.com/questions/73896306/circular-graph-diagram-with-ggplot-in-r


#Vorfälle insgesamt pro Fakultät

#Wie was ist passiert aber mit ver hat etwas gesehen -> oder gleich kombiniert?

#tätergruppen

