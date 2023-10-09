setwd("~/03_Outreach/dataviz/konstanz")

source("code/utils.R")

#this creates a series of plots that show properties of active persons

#Gender/Gender
df = full_data %>%
  select(Q2.1, fem_acting, male_acting, other_acting) %>%
  group_by(Q2.1) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  filter(!is.na(Q2.1)) %>%
  mutate("Gesamt" = rowSums(across(fem_acting)) + rowSums(across(male_acting)) + rowSums(across(other_acting)),
         "weiblich" = rowSums(across(fem_acting))/ Gesamt,
         "männlich" = rowSums(across(male_acting))/ Gesamt,
         "keine Angabe" = rowSums(across(other_acting))/ Gesamt,
         "gender_1" = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select("gender_1", "weiblich", "männlich", "keine Angabe", "Gesamt") %>%
  melt(id.vars = c("gender_1", "Gesamt")) %>%
  mutate(text = paste0(value, " %"))

df$variable = factor(df$variable, levels = c("männlich", "weiblich","keine Angabe"))
df$gender_1 = factor(df$gender_1, levels = c("männlich", "weiblich", "keine Angabe"))

(p1 = ggplot() + theme_void() +
    add_common_layout() +
    geom_bar(data = df[df$gender_1 %in% c("weiblich", "männlich"),], stat = "identity", color = "white",
           aes(x = gender_1, y = value, fill = variable)) +
   geom_text(data = df[df$gender_1 %in% c("weiblich", "männlich") & df$variable %in% c("weiblich", "männlich"),], 
              position = position_fill(vjust = 0.75), color = "white", size = 4,
              aes(x = gender_1, y = value, group = variable, label = paste0(round(value*100), " %\n(", value*Gesamt, ")"))) +
    scale_fill_manual(values = values_gender,
                    name = "Handelnde Person") +
    scale_y_continuous(name = "",
                       breaks = c(0, 0.5, 1), 
                       labels = c("0 %", "50 %", "100 %")) +
    scale_x_discrete(name = "Betroffene Person") +
    theme(axis.text = element_text(), 
          legend.position = "None",
          axis.title.x = element_text(),
          legend.box.margin = margin(l = -10, r = 20)))

df %>%
  mutate(abs = value*Gesamt,
         value = round(value*100)) %>%
  select(gender_1, value, abs, variable) %>%
  rename("Geschlecht betroffene Person" = gender_1, "Geschlecht handelnde Person" = variable,
         "%" = value, "Vorfälle" = abs) %>%
  write_delim("data/final/P4.1.csv", deli = ",")

df = full_data %>%
  select(Q2.1, fem_acting, male_acting, other_acting) %>%
  group_by(Q2.1) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  filter(!is.na(Q2.1)) %>%
  mutate("weiblich" = rowSums(across(fem_acting)),
         "männlich" = rowSums(across(male_acting)),
         "keine Angabe" = rowSums(across(other_acting)),
         "gender_1" = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select("weiblich", "männlich", "keine Angabe") %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  melt() %>% 
  mutate(fraction = value/sum(value)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(df_summarized$ymax, n = -1)),
         text = paste0(round(fraction*100), " % \n(", value, ")"))

(p3 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable)) + 
    theme_void() +
    add_common_layout() +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2-.05, label = text), color = "white", size = 4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(name = "", limits = c(1.5, 4)) +
    scale_fill_manual(values = values_gender,
                      name = "Handelnde Person") +
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "vertical"))

(p = plot_grid(p3, p1, nrow = 1))
ggsave("reports/figures/P4.1.png", dpi = 700, width = 12, height = 8)

#Rolle der handelnden Person


df = full_data %>%
  select(Q2.1, stud, doz, ang) %>%
  group_by(Q2.1) %>%
  summarize(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
  filter(!is.na(Q2.1)) %>%
  mutate("Gesamt" = rowSums(across(stud)) + rowSums(across(doz)) + rowSums(across(ang)),
         "Mitstudierende*r" = rowSums(across(stud))/Gesamt,
         "Dozierende*r" = rowSums(across(doz))/Gesamt,
         "HTWG Angestellte*r" = rowSums(across(ang))/Gesamt,
         "gender" = codes_gender$gender[match(Q2.1, codes_gender$code)]) %>%
  select("gender", "Mitstudierende*r", "Dozierende*r", "HTWG Angestellte*r", "Gesamt") %>%
  melt(id.vars = c("gender", "Gesamt"))

df$variable = factor(df$variable, levels = c("Mitstudierende*r", "Dozierende*r",  "HTWG Angestellte*r"))
df$gender = factor(df$gender, levels = codes_gender$gender)

(p1 = ggplot() + 
    theme_void() +
    add_common_layout() +
    geom_bar(data = df[df$gender %in% c("weiblich", "männlich"),], stat = "identity", color = "white",
             aes(x = gender, y = value, fill = variable)) +
    geom_text(data = df[df$gender %in% c("weiblich", "männlich") & df$value > 0.05,], 
              position = position_fill(vjust = 0.5), color = "white", size = 4,
              aes(x = gender, y = value, group = variable, label = paste0(round(value*100), " %\n(", value*Gesamt, ")"))) +
    scale_fill_manual(values = c("Mitstudierende*r" = dark_blue, "HTWG Angestellte*r" = soft_blue, "Dozierende*r" = teal),
                      name = "Handelnde Person") +
    scale_y_continuous(name = "",
                       breaks = c(0, 0.5, 1), 
                       labels = c("0 %", "50 %", "100 %")) +
    scale_x_discrete(name = "Betroffene Person") +
    theme(axis.text = element_text(), 
          legend.position = "None",
          axis.title.x = element_text(),
          legend.box.margin = margin(l = -10, r = 20)))

df %>%
  mutate(value = round(value*100)) %>%
  rename("Geschlecht betroffene Person" = gender, "Vorfälle" = Gesamt, 
         "Rolle handelnde Person" = variable, "%" = value) %>%
  write_delim("data/final/P4.2.csv", delim = ",")

df = full_data %>%
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
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(df$ymax, n = -1)),
         text = paste0(round(fraction*100), " % \n(", value, ")"))

(p3 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=variable)) + 
    theme_void() +
    add_common_layout() +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2, label = text), color = "white", size = 4) +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4)) +
    scale_fill_manual(values = c("Mitstudierende*r" = dark_blue, "HTWG Angestellte*r" = soft_blue, "Dozierende*r" = teal),
                      name = "Handelnde Person") +
    theme(text = element_text(size = 15),
          legend.position = "bottom",
          legend.direction = "vertical"))

(p = plot_grid(p3, p1, nrow = 1))
ggsave("reports/figures/P4.2.png", dpi = 700, width = 12, height = 8) 

