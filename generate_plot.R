library(ggplot2)

set.seed(as.numeric(Sys.time()))

df <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)

p <- ggplot(df, aes(x, y)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  theme_minimal() +
  ggtitle(paste("Graphique aléatoire", Sys.time()))

ggsave(
  filename = "plot.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 100
)
