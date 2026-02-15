# generate_plot.R
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

project_id <- "test-projet-challenge-lyceen-cnc-2026-marseille"
per_page <- 200
page <- 1
all_obs <- list()

repeat {
  url <- paste0(
    "https://api.inaturalist.org/v1/observations?",
    "project_id=", project_id,
    "&per_page=", per_page,
    "&page=", page
  )
  
  res <- GET(url)
  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  obs <- data$results
  if (length(obs) == 0) break
  
  all_obs <- append(all_obs, obs)
  page <- page + 1
}

# Convertir en data.frame et récupérer le champ "Ton lycée"
obs_df <- bind_rows(lapply(all_obs, function(x) {
  # Champ personnalisé
  lycee <- NA
  if(length(x$observation_field_values) > 0) {
    field <- x$observation_field_values
    if(any(sapply(field, function(f) f$field$name) == "Ton lycée")) {
      lycee <- sapply(field, function(f) if(f$field$name == "Ton lycée") f$value else NA)
      lycee <- lycee[!is.na(lycee)][1]
    }
  }
  
  data.frame(
    user_login = x$user$login,
    taxon_id = ifelse(is.null(x$taxon$id), NA, x$taxon$id),
    lycee = lycee,
    stringsAsFactors = FALSE
  )
}))

# Statistiques par utilisateur et par lycée
user_stats <- obs_df %>%
  group_by(user_login, lycee) %>%
  summarise(
    n_observations = n(),
    n_species = n_distinct(taxon_id),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observations))

# Barplot : observations par utilisateur, couleur = lycée
p <- ggplot(user_stats, aes(x = reorder(user_login, -n_observations), 
                            y = n_observations, fill = lycee)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n_species), vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(
    title = paste("Observations et espèces par utilisateur -", project_id),
    x = "Utilisateur",
    y = "Nombre d'observations",
    fill = "Lycée"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plot.png", plot = p, width = 12, height = 6, dpi = 300)
