library(tidyverse)
library(treemapify)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lingtypology)
library(patchwork)
library(ineq)
library(patchwork)

# read data
df_hf = read_csv("data/processed/hugging_face_models.csv")
glotto = lingtypology::glottolog

glotto = glotto %>%
  filter(level == "language") %>%
  select(glottocode, language, iso, latitude, longitude, area) %>%
  rename("macroarea" = area) %>%
  as_tibble()


# theme
theme_set(theme_bw())



# how many languages have at least model on hugging face?
df_hf = df_hf %>%
  mutate(percent_of_tags = RTC / sum(RTC) * 100)
sum(df_hf$RTC)

nrow(df_hf)


###### Lorenz curve #####
# how equal is the distribution of tags across speakers and languages?

lorenz = df_hf %>%
  pull(RTC) %>%
  Lc()

lorenz_df = data.frame(Languages = lorenz$p,
                       Tags = lorenz$L)

gini = df_hf %>%
  pull(RTC) %>%
  Gini()


Simpson = function(p_vec){
  simpson = sum(p_vec^2)
  return(simpson)
}

LDI = df_hf %>%
  summarize(LDI = 1 - Simpson(RTC/sum(RTC))) %>%
  mutate(inv_simpson = 1/(1-LDI))

lorenz_plot = lorenz_df  %>%
  ggplot(aes(y = Tags, 
             x = Languages)) + 
  geom_line(colour = "purple",
            linewidth = 1) + 
  geom_abline(color = "red",
              linetype = "dashed") +
  annotate("text", label = paste0("Gini Coefficient: ", as.character(round(gini,3)), 
                                  "\n Linguistic Diversity Index: ", as.character(round(LDI$LDI, 3)),
                                  "\n Inverse Simpson: ", as.character(round(LDI$inv_simpson, 3))),
           y = 0.9, x = 0.15)

ggsave("plots/lorenz_curve.png", lorenz_plot, height = 6, width = 8, dpi = 300)

##### maps #######
df_geo = ne_countries(scale = "large", returnclass = "sf")

df_points = df_hf %>% 
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude))

df_points = df_points %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))

ggplot(df_geo) + 
  geom_sf() +
  geom_point(data = df_points, aes(x = longitude, y = latitude, colour = macroarea,
             size = RTC, alpha = RTC)) + 
  labs(x = element_blank(),
       y = element_blank())

## no english
df_points = df_points %>%
  filter(ISO6393 != "eng") %>%
  mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude))

map_no_eng = ggplot(df_geo) + 
  geom_sf() +
  geom_point(data = df_points, aes(x = longitude, y = latitude, colour = macroarea,
                                   size = RTC, alpha = RTC)) + 
  labs(x = element_blank(),
       y = element_blank()) 


ggsave("plots/map_no_eng.png", map_no_eng, width = 12, height = 8, dpi = 300)

##### tree map

tree_proportion = df_hf %>%
  ggplot(aes(area = percent_of_tags, fill = macroarea, label = paste0(language, "\n", round(percent_of_tags,2) , "%"))) + 
  geom_treemap(colour = "black") + 
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE, reflow = TRUE) + 
  labs(fill = "Macroarea")

print(tree_proportion)


ggsave("plots/tree_porportion.png", tree_proportion, dpi = 300, width = 12, height = 6)


