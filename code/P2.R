source("code/utils.R")

#This creates plot P2, answering the question: An welcher Fakultät studieren Sie?

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
  mutate(total_asked = rowSums(across(.cols = codes_gender$gender))) %>%
  melt(measure.vars = codes_gender$gender, variable.name = "gender", value.name = "students") %>%
  mutate(share = (-1)*total_asked/total_wo_1*100) %>%
  mutate(share = ifelse(is.infinite(share), 0, share),
         text_share = paste(round(share/(-1)), "%")) %>% 
  select(faculty, gender, students, share, text_share, total_female, total_other, total_wo_1)


df$faculty = factor(df$faculty,
                            levels = rev(codes_faculty$faculty))

df$gender = factor(df$gender,
                           levels = rev(codes_gender$gender))


(p2 = ggplot(data = df[df$faculty != "keine Angabe", ], aes(x = faculty), color = "white") + theme_void() + 
    geom_bar(color = "white", stat = "identity", width = 0.7,
             aes(y = students, fill = gender)) +
    geom_bar(data = df[df$faculty != "keine Angabe" & df$gender == "weiblich", ], color = "white", stat = "identity", alpha = .4,  width = 0.7,
             aes(y = share*2)) +
    geom_text(aes(y = 0, label = faculty, x = factor(faculty)), size = 5, family = "swiss", 
              hjust = 0, nudge_x = 0.5, check_overlap = TRUE) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 5, family = "swiss", 
              aes(x = faculty, y = students,  label = students, group = gender)) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 5, family = "swiss", 
              aes(x = faculty, y = students,  label = students, group = gender)) +
    geom_text(data = df[df$students > 4, ], position = position_stack(vjust = 0.5), color = "white", size = 5, family = "swiss", 
              aes(x = faculty, y = share*2,  label = text_share, group = gender)) +
    coord_flip() +
    geom_hline(yintercept = 0, color = "grey20") +
    scale_y_continuous(name = "Teilnehmende Studierende (relativ/ absolut)",
                       expand = c(0,0), limits = c(-110, 300),
                       breaks = c(-100, -50,  100, 200),
                       labels = c("50 %", "25 %", 100, 200)) +
    scale_x_discrete(name = "") +
    scale_fill_manual(values = values_gender, labels = names_gender,
                      name = "") +
    add_common_layout(fontsize = 15) +
    theme(axis.text.x = element_text(color = "grey20"),
          axis.line.x = element_line(),
          axis.ticks.x = element_line(color = "grey20"), 
          axis.ticks.length = unit(.1, "cm"),
          legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(reverse=T)))

ggsave("reports/figures/P2.pdf", dpi = 300, width = 9)

df %>%
  select(faculty, gender, students, share, total_wo_1) %>%
  dcast(faculty + share + total_wo_1 ~ gender, value.var = "students") %>%
  mutate(share = round(share*(-1))) %>%
  rename("Fakultät" = faculty, "befragte Studierende männlich" = `männlich`, "befragte Studierende weiblich" = weiblich, "befragte Studierende kA" = `keine Angabe`,
         "Anteil befragter Studierende an Gesamtstudierenden (alle Geschlechter) in %" = share, "Gesamtstudierende (ohne Semester 1)" = total_wo_1) %>%
  write_delim("data/final/P2.csv", delim = ",")
