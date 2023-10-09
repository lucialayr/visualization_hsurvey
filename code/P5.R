setwd("~/03_Outreach/dataviz/konstanz")

source("code/utils.R")

#This script creates a series of plots that compares incidents per faculty to average

#Was ist passiert pro Fakultät


df = full_data %>%
  select(Q2.2, Q2.1, main_question3) %>%
  filter(!is.na(Q2.2)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  filter_at(vars(main_question3), any_vars (. == 1)) %>%
  select(-Q2.2, -Q2.1) %>%
  melt(id.vars = c("gender", "faculty")) %>%
  filter(value == 1) %>%
  count(faculty, gender) 

(p1 = ggplot() + 
    theme_void() + 
    add_common_layout() +
    coord_flip() +
    geom_bar(data = df[df$faculty != "keine Angabe", ], stat = "identity", color = "white",
           aes(x = faculty, y = n, fill = gender)) +
    geom_text(data = df[df$n > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 4,
              aes(x = faculty, y = n,  label = n, group = gender)) +
    scale_fill_manual(values = values_gender,
                    name = "Mit welchem Geschlecht \nidentifizieren Sie sich?") +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Anzahl der Vorfälle") +
    theme(axis.text = element_text(hjust = 1),
        axis.title.x = element_text(),
        legend.position = "bottom",
        legend.direction = "horizontal"))


df_faculty_affected = full_data %>%
  select(Q2.2, Q2.1, main_question3) %>%
  filter(!is.na(Q2.2),
         !is.na(Q2.1)) %>%
  mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  filter_at(vars(main_question3), any_vars (. == 1)) %>%
  select(-Q2.2, -Q2.1) %>%
  count(faculty, gender)

df = full_data %>%
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


(p2 = ggplot() + 
    theme_void() + 
    add_common_layout() +
    geom_bar(data = df, stat = "identity", position = "dodge", color = "white",
           aes(x = faculty, y = share, fill = gender)) +
    geom_text(data = df, position = position_dodge(width = .9), color = "grey20", size = 4,
              aes(x = faculty, y = share + .05,  label = paste(round(share*100), "%"), group = gender)) +
    coord_flip() +
    scale_fill_manual(values = values_gender,
                    name = "") +
    scale_x_discrete(name = "") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5), labels = c("0 %", "25 %", "50 %"), name = "Betroffene in % der Befragten") +
    theme(axis.text = element_text(hjust = 1),
        axis.title.x = element_text(),
        legend.position = "None"))

(p = plot_grid(p1, p2, nrow = 2))
ggsave("reports/figures/P5.1.png", dpi = 700) 
