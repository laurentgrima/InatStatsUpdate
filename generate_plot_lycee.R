library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)

project_id <- "test-projet-challenge-lyceen-cnc-2026-marseille"

base_url <- "https://api.inaturalist.org/v1/observations"

res <- GET(
  base_url,
  query = list(
    project_id = project_id,
    per_page = 100,
    order = "desc",
    order_by = "created_at"
  )
)

data_json <- content(res, as = "text", encoding = "UTF-8")
data <- fromJSON(data_json, flatten = TRUE)

obs_df <- data$results %>%
  tibble::as_tibble() %>%
  mutate(
    ton_lycee = map_chr(ofvs, function(ofv) {
      if (length(ofv) == 0) return(NA)
      val <- ofv$value[ofv$name == "Ton lycée"]
      if(length(val) == 0) return(NA)
      val
    }),
    species = taxon.name,
    common_name = taxon.preferred_common_name,
    user = user.login,
    date = observed_on,
    quality = quality_grade,
    obs_id = id
  ) %>%
  select(obs_id, date, species, common_name, user, ton_lycee, quality)


###

library(ggplot2)
library(dplyr)
library(tidyr)

obs_df_clean <- obs_df %>%
  mutate(ton_lycee = ifelse(is.na(ton_lycee), "Inconnu", ton_lycee))



summary_df <- obs_df_clean %>%
  group_by(ton_lycee) %>%
  summarise(
    n_observations = n(),
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(n_observations, n_species),
               names_to = "type",
               values_to = "count") %>%
  mutate(type = recode(type,
                       n_observations = "Total des observations",
                       n_species = "Nombre d'espèces"))

colors_elegant <- c("Total des observations" = "#6baed6", "Nombre d'espèces" = "#fd8d3c")

p <- ggplot(summary_df, aes(x = reorder(ton_lycee, -count), y = count, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "gray30", alpha = 0.85) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 4, 
            fontface = "bold") +
  labs(
    title = "Nombre d'observations et nombre d'espèces différentes par lycée",
    x = "",
    y = "",
    fill = ""  
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "gray20"),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "italic")
  ) +
  scale_fill_manual(values = colors_elegant) +
  ylim(0, max(summary_df$count) * 1.15) +
    annotate("text",
           x = Inf, y = -Inf,                 # coins du graphique
           label = format(Sys.time(), "%Y-%m-%d %H:%M"),
           hjust = 1.1, vjust = 0.0,        # ajustement pour coller aux bords
           size = 4.5,                        # taille du texte
           color = "black")

ggsave("barplot_lycee_valeurs.png", plot = p, width = 10, height = 6, dpi = 300)
