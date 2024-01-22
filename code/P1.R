source("code/utils.R")

#This creates plot P1, answering the question "Mit welchem Geschlecht identifizieren Sie sich?"

#### GESAMT
df_gender = full_data_all_genders %>%
  select(Q2.1) %>%
  filter(!is.na(Q2.1)) %>%
  melt(variable.name = "1") %>%
  mutate(gender = codes_all_gender$gender[match(value, codes_all_gender$code)]) 

df_gender$gender = factor(df_gender$gender,
                          levels = codes_all_gender$gender)

df = table(df_gender$gender) %>%
  melt(value.name = "count", variable.name = "gender") %>%
  rename("gender" = "Var1") %>%
  mutate(fraction = count/sum(count)) %>%
  mutate(ymax = cumsum(fraction)) 

df = df %>%  #this needs to be done in a separate pipe to work for some reason
  mutate(ymin = c(0, head(df$ymax, n = -1)),
         text = paste0(round(fraction*100,1), " % \n(", count, ")"))

(p1 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) + theme_void() +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4), name = "") +
    scale_fill_manual(values = values_gender_all,
                      name = "Mit welchem Geschlecht identifizieren Sie sich?") +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2-.04, label = text), 
              color = "white", size = 5, family = "swiss") +
    geom_segment(aes(x =3.4, xend = 3.6, y = 0.97, yend = 0.995), color = "white", size = .25) +
    geom_segment(aes(x =3.4, xend = 3.6, y = 0.585, yend = 0.565), color = "white", size = .25) +
    add_common_layout(fontsize = 15) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position="top")))
ggsave("reports/figures/P1.1.pdf", dpi = 200)


df %>% 
  select(gender, count, fraction) %>%
  mutate(fraction = round(fraction*100, 2),
         gender = as.character(gender)) %>%
  rename("Geschlecht" = gender, "Anzahl" = count, "%" = fraction) %>%
  write_delim("data/final/P1.1.csv", delim = ",")

### BETROFFEN

df_gender = full_data_all_genders %>%
  filter_at(vars(main_question3), any_vars (. == 1)) %>%
  select(Q2.1) %>%
  filter(!is.na(Q2.1)) %>%
  melt(variable.name = "1") %>%
  mutate(gender = codes_all_gender$gender[match(value, codes_all_gender$code)]) 

df_gender$gender = factor(df_gender$gender,
                          levels = codes_all_gender$gender)

df = table(df_gender$gender) %>%
  melt(value.name = "count", variable.name = "gender") %>%
  rename("gender" = "Var1") %>%
  mutate(fraction = count/sum(count)) %>%
  mutate(ymax = cumsum(fraction)) 

df = df %>%  #this needs to be done in a separate pipe to work for some reason
  mutate(ymin = c(0, head(df$ymax, n = -1)),
         text = paste0(round(fraction*100,1), " % \n(", count, ")"))

(p1 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) + theme_void() +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4), name = "") +
    scale_fill_manual(values = values_gender_all,
                      name = "Mit welchem Geschlecht identifizieren Sie sich?") +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2-.04, label = text), 
              color = "white", size = 5, family = "swiss") +
    geom_segment(aes(x =3.4, xend = 3.6, y = 0.97, yend = 0.995), color = "white", size = .25) +
    geom_segment(aes(x =3.4, xend = 3.3, y = 0.355, yend = 0.33), color = "white", size = .25) +
    add_common_layout(fontsize = 15) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position="top")))
ggsave("reports/figures/P1.2.pdf", dpi = 200)


df %>% 
  select(gender, count, fraction) %>%
  mutate(fraction = round(fraction*100, 2),
         gender = as.character(gender)) %>%
  rename("Geschlecht" = gender, "Anzahl" = count, "%" = fraction) %>%
  write_delim("data/final/P1.2.csv", delim = ",")

### BEOBACHTET

df_gender = full_data_all_genders %>%
  filter_at(vars(main_question4), any_vars (. == 1)) %>%
  select(Q2.1) %>%
  filter(!is.na(Q2.1)) %>%
  melt(variable.name = "1") %>%
  mutate(gender = codes_all_gender$gender[match(value, codes_all_gender$code)]) 

df_gender$gender = factor(df_gender$gender,
                          levels = codes_all_gender$gender)

df = table(df_gender$gender) %>%
  melt(value.name = "count", variable.name = "gender") %>%
  rename("gender" = "Var1") %>%
  mutate(fraction = count/sum(count)) %>%
  mutate(ymax = cumsum(fraction)) 

df = df %>%  #this needs to be done in a separate pipe to work for some reason
  mutate(ymin = c(0, head(df$ymax, n = -1)),
         text = paste0(round(fraction*100,1), " % \n(", count, ")"))

(p1 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) + theme_void() +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4), name = "") +
    scale_fill_manual(values = values_gender_all,
                      name = "Mit welchem Geschlecht identifizieren Sie sich?") +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2-.04, label = text), 
              color = "white", size = 5, family = "swiss") +
    geom_segment(aes(x =3.4, xend = 3.6, y = 0.97, yend = 0.995), color = "white", size = .25) +
    geom_segment(aes(x =3.4, xend = 3.3, y = 0.47, yend = 0.45), color = "white", size = .25) +
    add_common_layout(fontsize = 15) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position="top")))
ggsave("reports/figures/P1.3.pdf", dpi = 200)


df %>% 
  select(gender, count, fraction) %>%
  mutate(fraction = round(fraction*100, 2),
         gender = as.character(gender)) %>%
  rename("Geschlecht" = gender, "Anzahl" = count, "%" = fraction) %>%
  write_delim("data/final/P1.3.csv", delim = ",")
  
