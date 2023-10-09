setwd("~/03_Outreach/dataviz/konstanz")

source("code/utils.R")



#This creates plot P1, answering the question "Mit welchem Geschlecht identifizieren Sie sich?"


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
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(df$ymax, n = -1)),
         text = paste0(round(fraction*100), " % \n(", count, ")"))

(p1 = ggplot(data = df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) + theme_void() +
    scale_y_continuous(name = "") +
    scale_x_continuous(limits = c(1.5, 4), name = "") +
    scale_fill_manual(values = values_gender,
                      name = "Mit welchem Geschlecht identifizieren Sie sich?") +
    coord_polar(theta = "y") +
    geom_rect(color = "white") +
    geom_text(aes(x = 3.5, y = (ymin + ymax)/2-.025, label = text), color = "white", size = 5) +
    add_common_layout(fontsize = 15) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position="top")))
ggsave("reports/figures/P1.png", dpi = 700)


df %>% 
  select(gender, count, fraction) %>%
  mutate(fraction = round(fraction*100, 2),
         gender = as.character(gender)) %>%
  rename("Geschlecht" = gender, "Anzahl" = count, "%" = fraction) %>%
  write_delim("data/final/P1.csv", delim = ",")
  
