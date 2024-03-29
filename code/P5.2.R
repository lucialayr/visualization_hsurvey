source("code/utils.R")

#This script creates a series of plots that compares incidents per faculty to average

#Was ist passiert pro Fakultät

#faculty comparison
faculty_benchmark = function(faculty_code, full_data, main_question3, codes_faculty, codes_gender, df_all_students) {
  df_all_students_gender = df_all_students %>%
    rename(Gesamt = total_wo_1) %>%
    select(faculty, Gesamt)
  
  
  df_faculty_affected = full_data %>%
    select(Q2.2, Q2.1, main_question3) %>%
    filter(!is.na(Q2.2),
           !is.na(Q2.1)) %>%
    mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
           gender = codes_gender$gender[match(Q2.1, codes_gender$code)],
           gender = replace(gender, gender %in% c("männlich", "keine Angabe"), "andere")) %>%
    filter_at(vars(main_question3), any_vars (. == 1)) %>%
    select(-Q2.2, -Q2.1) %>%
    count(faculty)
  
  df = full_data %>%
    select(Q2.2, Q2.1) %>%
    filter(!is.na(Q2.2),
           !is.na(Q2.1)) %>%
    mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
           gender = codes_gender$gender[match(Q2.1, codes_gender$code)],
           gender = replace(gender, gender %in% c("männlich", "keine Angabe"), "andere")) %>%
    count(faculty) %>%
    rename("Teilnehmende" = n) %>%
    left_join(df_faculty_affected) %>%
    rename("Betroffene" = n) %>%
    filter(faculty != "keine Angabe") %>%
    left_join(df_all_students_gender) %>%
    mutate("Teilnehmende in % Studierende" = 100*Teilnehmende/ Gesamt,
           "Betroffene in % Teilnehmende" = 100*Betroffene/ Teilnehmende,
           "Betroffene in % Studierende" = 100*Betroffene/ Gesamt) 
  
  df_mean = df %>%
    summarize_at(c("Teilnehmende in % Studierende", "Betroffene in % Teilnehmende", "Betroffene in % Studierende"), ~ mean(.x, na.rm = TRUE)) %>%
    melt(value.name = "mean")
  
  df = df %>%
    filter(faculty == codes_faculty[codes_faculty$code == faculty_code, ]$faculty)  %>%
    select(c("Teilnehmende in % Studierende", "Betroffene in % Teilnehmende", "Betroffene in % Studierende")) %>%
    melt(value.name = "selected") %>%
    left_join(df_mean) %>%
    mutate(direction = (case_when(mean < selected ~ "überdurchschnittlich",
                                  mean > selected ~ "unterdurchschnittlich",
                                  mean == selected ~ "durchschnittlich")))
  
  df$variable = factor(df$variable, levels = c("Betroffene in % Studierende", "Betroffene in % Teilnehmende", "Teilnehmende in % Studierende"))
  
  (p1 = ggplot(data = df) + 
      theme_void() +
      add_common_layout() +
      coord_flip() +
      geom_link(alpha = .5, color = soft_blue, show.legend = FALSE,
                aes(x = variable, xend = variable, y = mean, yend = selected, colour = gender, size = after_stat(index))) +
      geom_point(size = 6, color = dark_blue,
                 aes(y = selected, x = variable, shape = "Fakultät")) +
      geom_point(size = 2, stroke = 1, color = dark_blue,
                 aes(y = mean, x = variable, shape = "Durchschnitt"), fill = "white") +
      scale_x_discrete(name = "", expand = c(0.05,0.05)) +
      scale_y_continuous(name = "Prozent (%)") +
      scale_color_manual(values = c(weiblich = teal, andere = dark_blue),
                         name = "") +
      scale_shape_manual(breaks=c('Durchschnitt', "Fakultät"),
                         values=c('Durchschnitt'=21, "Fakultät"=16),
                         name = "") +
      theme(axis.text = element_text(),
            axis.title.x = element_text(),
            axis.line.x = element_line(color = "grey50"), 
            axis.ticks.x = element_line(color = "black"),
            legend.box.margin = margin(0, .5, 0, 0, "cm")))
  
  df %>%
    mutate(selected = round(selected, 1),
           mean = round(mean, 1)) %>%
    rename("Kategorie" = variable, "Durchschnitt Fakultät in %" = selected, "Durchschnitt global in %" = mean, "Richtung" = direction) %>%
    write_delim(paste0("data/final/P5.2A", codes_faculty[codes_faculty$code == faculty_code, ]$faculty, ".csv"), delim = ",")
  
  ggsave(paste0("reports/figures/P5.2A_", codes_faculty[codes_faculty$code == faculty_code, ]$faculty, ".pdf"), dpi = 300, width = 13, height = 2) 
  
  df_faculty_all = full_data %>%
    select(Q2.2, Q2.1) %>%
    filter(!is.na(Q2.2),
           !is.na(Q2.1)) %>%
    mutate(faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)],
           gender = codes_gender$gender[match(Q2.1, codes_gender$code)],
           gender = replace(gender, gender %in% c("männlich", "keine Angabe"), "andere")) %>%
    count(faculty) %>%
    rename("Teilnehmende" = n) %>%
    left_join(df_faculty_affected) %>%
    rename("Betroffene" = n) %>%
    filter(faculty != "keine Angabe")
  
  
  df_faculty_affected_waffle = df_faculty_all %>%
    left_join(df_all_students_gender) %>%
    filter(faculty == codes_faculty[codes_faculty$code == faculty_code, ]$faculty) %>%
    summarize_at(c("Betroffene", "Teilnehmende", "Gesamt"), ~ sum(.x, na.rm = TRUE), 0) %>%
    mutate("Teilnehmende" = round(100*Teilnehmende/ Gesamt, 0),
           "Betroffene" = round(100*Betroffene/ Gesamt, 0),
           "Studierende" = 100 - Teilnehmende - Betroffene) %>%
    select(-Gesamt)
  

  p2 = waffle(df_faculty_affected_waffle, rows = 4, colors = c(teal, dark_blue, soft_blue), legend_pos = "bottom") +
    add_common_layout() +
    theme(legend.box.margin = margin(0, .5, 0, 0, "cm"))
  
  df_faculty_affected_waffle %>%
    write_delim(paste0("data/final/P5.2B", codes_faculty[codes_faculty$code == faculty_code, ]$faculty, ".csv"), delim = ",")
    
  ggsave(paste0("reports/figures/P5.2B_", codes_faculty[codes_faculty$code == faculty_code, ]$faculty, ".pdf"), dpi = 300, width = 11, height = 2) 
  
  (plot_grid(p1, p2, ncol = 1))
  ggsave(paste0("reports/figures/P5.2C_", codes_faculty[codes_faculty$code == faculty_code, ]$faculty, ".pdf"), dpi = 300, width = 13) 
}

for (i in c(1:6)) {
  faculty_benchmark(faculty_code = i, full_data, main_question3, codes_faculty, codes_gender, df_all_students)
}
