source("code/utils.R")

#This script creates a plot that shows how uncomfortable situation

df = full_data %>%
  select(Q2.1,  all_of(scale_q3)) %>%
  filter_at(vars(scale_q3), any_vars (!is.na(.))) %>%
  mutate(gender = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select(-Q2.1) 

names(df) = c(main_question3, "gender")

df = df %>%
  melt(id.vars = "gender") %>%
  filter(!is.na(value),
         !is.na(gender)) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(variable, gender) %>%
  summarize(mean = mean(value, na.rm = FALSE),
            sd = sd(value, na.rm = FALSE)) %>%
  filter(gender %in% c("männlich", "weiblich"),
         !is.na(variable),
         !is.na(mean))

ggplot(data = df, aes(x = variable, y = mean, color = gender)) + 
  theme_void() +
  add_common_layout(13) +
  coord_flip() +
  geom_pointrange(aes(ymin = mean-sd, ymax = mean +sd), size = 1) +
  scale_color_manual(values = values_gender,
                     name = "") +
  scale_y_continuous(name = "Als wie unangenehm wurde das Erlebnis empfunden?") +
  scale_x_discrete(expand = c(0.05,0.05), name = "",
                   labels =str_wrap(str_replace_all(long_names3, "foo" , " "), width = 70)) + #induce line break
  theme(axis.line.x = element_line(),
        axis.text = element_text())

ggsave("reports/figures/P6.pdf", dpi = 300, width = 12, height = 9) 

df %>%
  rename("Geschlecht" = gender, "Durchschnitt" = mean, "Frage" = variable) %>%
  write_delim("data/final/P6.csv", delim = ",")
