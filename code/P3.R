setwd("~/03_Outreach/dataviz/konstanz")

source("code/utils.R")

#This creates a plot series P3.X which show the ansers to the question blocks
#Q3 and Q4


#P3.1
#what happended


#absolute values
df1 = full_data %>% 
  select(all_of(main_question3), "Q2.1") %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) %>%
  filter_all(any_vars(. == 1)) %>%
  melt(id.vars = c("gender")) %>%
  filter(!is.na(value),
         !is.na(variable),
         !is.na(gender),
         value == 1) %>%
  count(gender, variable)

df_labels = df1 %>%
  group_by(variable) %>%
  summarise(n = sum(n))

ggplot() + theme_void() +
  add_common_layout(fontsize = 15) +
  coord_flip() +
  geom_bar(data = df1, stat = "identity", color = "white",
           aes(x = variable, y = n, fill = gender)) +
  geom_text(data = df_labels, color = "grey20", size = 5,
            aes(x = variable, y = n+5,  label = n)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_x_discrete(expand = c(0,0), name = "", 
                   labels =str_wrap(str_replace_all(long_names, "foo" , " "), width = 50)) + #induce line break
  scale_y_continuous(expand = c(0,0), limits = c(0, 160), name = "Anzahl Studierende",
                     breaks = c(50, 100, 150)) +
  scale_fill_manual(values = values_gender, name = "")

ggsave("reports/figures/P3.1.png", dpi = 700, width = 16, height = 11)

df1 %>%
  rename("Geschlecht" = gender, "Frage" = variable, "Studierende" = n)  %>%
  write_delim("data/final/P3.1.csv", delim = ",")



#P3.2
df2 = full_data %>% 
  select(all_of(main_question4), "Q2.1") %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) %>%
  filter_all(any_vars(. == 1)) %>%
  melt(id.vars = c("gender")) %>%
  filter(!is.na(value),
         !is.na(variable),
         !is.na(gender),
         value == 1) %>%
  count(gender, variable)

df_labels = df2 %>%
  group_by(variable) %>%
  summarise(n = sum(n))


ggplot() + theme_void() +
  add_common_layout(fontsize = 15) +
  coord_flip() +
  geom_bar(data = df2, stat = "identity", color = "white",
           aes(x = variable, y = n, fill = gender)) +
  geom_text(data = df_labels, color = "grey20", size = 5,
            aes(x = variable, y = n+5,  label = n)) +
  theme(legend.position = "None",
        axis.text = element_text(color = "grey20"),
        axis.line = element_line()) +
  scale_x_discrete(expand = c(0,0), name = "", 
                   labels =str_wrap(str_replace_all(long_names, "foo" , " "), width = 50)) + #induce line break
  scale_y_continuous(expand = c(0,0), limits = c(0, 160), name = "Anzahl Studierende",
                     breaks = c(50, 100, 150)) +
  scale_fill_manual(values = values_gender, name = "")

ggsave("reports/figures/P3.2.png", dpi = 700, width = 16, height = 11)

df2 %>%
  rename("Geschlecht" = gender, "Frage" = variable, "Studierende" = n)  %>%
  write_delim("data/final/P3.2.csv", delim = ",")
#P3.3

q3q4_paare = read.csv("data/processed/Q3_Q4_Paare.csv")

df2_join = df2 %>%
  rename("Q4_n" = n,
         "variable_Q4" = variable)

df = df1 %>%
  rename("Q3_n" = n, 
         "variable_Q3" = variable) %>%
  mutate(variable_Q4 = q3q4_paare$Q4[match(variable_Q3, q3q4_paare$Q3)],
         name = q3q4_paare$gesehen[match(variable_Q3, q3q4_paare$Q3)]) %>%
  left_join(df2_join) %>%
  mutate(Q3_n = -Q3_n) %>%
  filter(!is.na(Q4_n))

df_labels = df %>%
  group_by(name) %>%
  summarize(Q3_n = sum(Q3_n),
            Q4_n = sum(Q4_n)) %>%
  melt(measure.vars = c("Q4_n", "Q3_n")) %>%
  mutate(text = abs(value))

# Barplot

(ggplot(data = df, aes(x = reorder(name, Q3_n, sum))) + 
    theme_void() +
    coord_flip() +
    add_common_layout(fontsize=15) +
    geom_bar(color = "white", stat = "identity", width = 0.6,
             aes(y = Q3_n, fill = gender)) +
    geom_bar(color = "white", stat = "identity", width = 0.6,
             aes(y = Q4_n, fill = gender)) +
    geom_text(aes(y = 0, label =name, x = factor(name)), size = 5,
              hjust = .5, nudge_x = -0.45, check_overlap = TRUE) +
    geom_text(data = df_labels, color = "grey20", size = 4,
              aes(x = name, y = value + 5 * sign(value),  label = text)) +
    scale_x_discrete(name = "") +
    scale_y_continuous(expand = c(0,0), name = "",
                       limits = c(-155, 170), 
                       breaks = c(-150, -100, -50, 0, 50, 100, 150),
                       labels = c(150, 100, -50, 0, 50, 100, 150)) +
    scale_fill_manual(values = values_gender,
                      name = "") +
    theme(axis.text.x = element_text(color = "grey20"),
          axis.line.x = element_line(color = "grey20"),
          legend.position = "None") +
    guides(fill = guide_legend(reverse=T)))


ggsave("reports/figures/P3.3.png", dpi = 700,  width = 15, height = 10)


#P3.4
df = full_data %>%
  select(Q2.1,  all_of(main_question3), all_of(main_question4)) %>%
  mutate(q3 = ifelse(if_any(main_question3, ~ .x == 1),  1, 0),
         q4 = ifelse(if_any(main_question4, ~ .x == 1),  1, 0),
         gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) 

ggplot() + theme_void() +
  add_common_layout() +
  coord_equal() +
  geom_point(data = df, position = position_jitter(),
             aes(x = q3, y = q4, fill = gender), 
             color = "white", shape = 21, stroke = .75, size = 3.5, alpha = .9) +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = .5) +
  scale_x_reverse(name = "", position = "top",
                  breaks = c(1, 0), labels = c("betroffen", "nicht betroffen")) +
  scale_y_continuous(breaks = c(1, 0), name = "",
                     labels = c("beobachtet", "nicht beobachtet")) +
  scale_fill_manual(values = values_gender,
                    name = "Mit welchem Geschlecht \nidentifizieren Sie sich?") +
  theme(axis.text = element_text(),
        axis.text.y = element_text(angle = 90))

ggsave("reports/figures/P3.4.png", dpi = 700)

df %>% 
  mutate(Kategorie = case_when(q3 == q4 & q3 == 1 ~ "betroffen, beobachtet",
                               q3 == q4 & q3 == 0 ~ "nicht betroffen, nicht beobachtet",
                               q3 != q4 & q3 == 1 ~ "betroffen, nicht beobachtet",
                               q3 != q4 & q3 == 0 ~ "nicht betroffen, beobachtet")) %>%
  select(Kategorie, gender) %>%
  count(Kategorie, gender) %>%
  filter(!is.na(Kategorie)) %>%
  rename("Geschlecht" = gender, "Studierende" = n) %>%
  write_delim("data/final/P3.4.csv", delim = ",")

full_data %>% 
  select(all_of(main_question3), "Q2.1", "Q2.2") %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)],
         faculty = codes_faculty$faculty[match(Q2.2, codes_faculty$code)]) %>%
  select(-Q2.1, -Q2.2) %>%
  filter_all(any_vars(. == 1)) %>%
  melt(id.vars = c("gender", "faculty")) %>%
  filter(!is.na(value),
         !is.na(variable),
         !is.na(gender),
         value == 1) %>%
  count(gender, variable, faculty) %>%
  write_delim("data/final/P3.1_per_Fakultät.csv", delim = ",")

