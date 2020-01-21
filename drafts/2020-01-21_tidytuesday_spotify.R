# load libraries
library(tidyverse)
library(cowplot)
library(umap)

# import data
tuesdata <- tidytuesdayR::tt_load('2020-01-21') 

dat <- tuesdata$spotify_songs %>% 
  mutate(track_album_release_date = as.Date(track_album_release_date))

glimpse(dat)


p_length <- ggplot(dat, aes(x = track_album_release_date, 
                            y = duration_ms / 60000)) +
  geom_point(aes(color = playlist_genre), alpha = 0.3) + 
  theme_cowplot() + 
  geom_smooth() +
  facet_wrap(~playlist_genre) +
  theme(legend.position = "none") +
  labs(x = "Track Release Date",
       y = "Song Length (min)",
       title = "Length of Songs by Genre",
       subtitle = "Souce: Spotify with help from {spotifyr}",
       caption = "#TidyTuesday | Plot by @JamesHWade")


umap_dat <- umap::umap(scale(dat[, 12:23]))

umap_dims <- data.frame(umap_dat$layout)

dat_2 <- bind_cols(dat, umap_dims)


# create list of features to use for color
feature_names <- names(dat[, c(4, 12:23)])

# function to plot umap results coloring by feature
umap_plot <- function(feature_name, data) {
  p <- ggplot(data) + 
    geom_point(aes(x = X1, y = X2, color = .data[[feature_name]]), alpha = 0.4) +
    theme_cowplot() +
    scale_color_viridis_c() +
    facet_wrap(~playlist_genre) +
    labs(x = "UMAP Dimension 1", 
         y = "UMAP Dimension 2",
         color = feature_name,
         title = "Dimension Reduction on Spotify Data with UMAP",
         subtitle = "Souce: Spotify with help from {spotifyr}",
         caption = "#TidyTuesday | Plot by @JamesHWade"
    )
  
  ggsave(plot = p,
         filename = paste0("umap_", feature_name, ".png"), 
         path = "drafts",
         dpi = "retina")
  
}

# map over feature names and save plot
walk(feature_names, umap_plot, data = dat_2)

# create and save separate umap plot colored by genre
ggplot(dat_2) + 
  geom_point(aes(x = X1, y = X2, color = playlist_genre), alpha = 0.2) +
  theme_cowplot() +
  theme(legend.position = "none") +
  labs(x = "UMAP Dimension 1", 
       y = "UMAP Dimension 2",
       color = "Genre",
       title = "Dimension Reduction on Spotify Data with UMAP",
       subtitle = "Souce: Spotify with help from {spotifyr}",
       caption = "#TidyTuesday | Plot by @JamesHWade"
  ) +
  facet_wrap(~playlist_genre)

ggsave(filename = paste0("umap_by_genre.png"), 
       path = "drafts",
       dpi = "retina")
