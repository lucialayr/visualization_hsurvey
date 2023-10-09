setwd("~/03_Outreach/dataviz/konstanz")

source("code/utils.R")

#This creates plot P2, answering the question: An welcher Fakultät studieren Sie?

#takes from report
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


df = full_data %>%
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
  melt(measure.vars = codes_gender$gender, variable.name = "gender", value.name = "students") %>%
  mutate(share = case_when(gender == "weiblich" ~ -students*100/total_female,
                           gender != "weiblich" ~ -students*100/total_other)) %>%
  mutate(share = ifelse(is.infinite(share), 0, share),
         text_share = paste(round(share/(-1)), "%")) %>% 
  select(faculty, gender, students, share, text_share, total_female, total_other)


df$faculty = factor(df$faculty,
                            levels = rev(codes_faculty$faculty))

df$gender = factor(df$gender,
                           levels = rev(codes_gender$gender))


(p2 = ggplot(data = df[df$faculty != "keine Angabe", ], aes(x = faculty), color = "white") + theme_void() + 
    geom_bar(color = "white", stat = "identity", width = 0.7,
             aes(y = students, fill = gender)) +
    geom_bar(color = "white", stat = "identity", alpha = .4,  width = 0.7,
             aes(y = share*2, fill = gender)) +
    geom_text(aes(y = 0, label = faculty, x = factor(faculty)), size = 5,
              hjust = 0, nudge_x = 0.5, check_overlap = TRUE) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 5,
              aes(x = faculty, y = students,  label = students, group = gender)) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 5,
              aes(x = faculty, y = students,  label = students, group = gender)) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 4,
              aes(x = faculty, y = share*2,  label = text_share, group = gender)) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey20") +
    scale_y_continuous(name = "Teilnehmende Studierende (relativ/ absolut)",
                       expand = c(0,0), limits = c(-175, 300),
                       breaks = c(-75*2, -100, -50,  100, 200),
                       labels = c("75 %", "50 %", "25 %", 100, 200)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(name = "",
                      values = c("weiblich" = dark_blue, "männlich" = teal,  "keine Angabe" = soft_blue, 
                                 "share_female" = dark_blue, "share_other" = teal)) +
    add_common_layout(fontsize = 15) +
    theme(axis.text.x = element_text(color = "grey20"),
          axis.line.x = element_line(),
          axis.ticks = element_line(color = "grey20"), 
          axis.ticks.length = unit(.1, "cm"),
          legend.position = "None") +
    guides(fill = guide_legend(reverse=T)))

ggsave("reports/figures/P2.png", dpi = 700, width = 9)

df %>%
  select(faculty, gender, students, share, total_female, total_other) %>%
  mutate(share = round(share*(-1))) %>%
  rename("Fakultät" = faculty, "Geschlecht" = gender, "Studierende" = students, 
         "Anteil Gesamtstudierende" = share, "Gesamtstudierende weiblich" = total_female,
         "Gesamtstudierende andere" = total_other) %>%
  write_delim("data/final/P2.csv", delim = ",")
