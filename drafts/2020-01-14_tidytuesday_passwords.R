# load libraries
library(tidyverse)
library(cowplot)
library(ggimage)

# import data with {tidytuesdayR} package 
dat <- tidytuesdayR::tt_load('2020-01-14')$passwords %>% 
  filter(complete.cases(.)) %>% # remove NA rows
  mutate(
    pass_length = str_length(password),
    pass_type = case_when(
      grepl('[[:digit:]]', password) & grepl('[[:alpha:]]', password) ~ 'both',
      grepl('[[:alpha:]]', password) ~ 'letters',
      grepl('[[:digit:]]', password) ~ 'numbers'),
    icon = case_when(
      pass_type == 'both' ~ 'ios-lock',
      pass_type == 'letters' ~ 'ios-book',
      pass_type == 'numbers' ~ 'ios-calculator'
    )
  )

category <- dat %>% group_by(category) %>% tally()

# pie chart

ggplot(category) + 
  geom_col(aes(x = "", y = n, fill = category), width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_cowplot() +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Passwords by Category",
       fill = "Category",
       subtitle = "Source: Information is Beautiful",
       caption = "#TidyTuesday | @JamesHWade") -> p1

# bar chart

ggplot(category) + 
  geom_col(aes(x = fct_reorder(category, n), y = n/sum(n), fill = category)) +
  theme_cowplot() +
  theme(legend.position = "none") +
  labs(x = "Category", y = "Proportion",
       title = "Passwords by Category",
       subtitle = "Source: Information is Beautiful",
       caption = "#TidyTuesday | @JamesHWade") +
  coord_flip() -> p2

# time to crack vs length

ggplot(dat) +
  geom_icon(aes(x = pass_length, y = log10(offline_crack_sec), 
                 image = icon, fill = pass_type)) +
  theme_cowplot() +
  labs(x = "Password Length", y = "Time to Crack  ( log10(sec) )",
       title = "Time to Crack vs Password Length",
       subtitle = "Book = Letters Only  |  Calculator = Numbers Only  |  Lock = Both",
       caption = "#TidyTuesday | @JamesHWade") -> p3

ggsave(p1, filename = "password_plot_pie.png", dpi = "retina")
ggsave(p2, filename = "password_plot_bar.png", dpi = "retina")
ggsave(p3, filename = "password_plot_type.png", dpi = "retina")


# print session info
sessionInfo()
