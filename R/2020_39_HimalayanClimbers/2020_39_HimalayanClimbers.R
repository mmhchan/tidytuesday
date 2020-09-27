library(tidyverse)
library(ggtext)
library(pdftools)

expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

year_range <- 2000:2019

top_n_freq_peaks <-
  expeditions %>%
  filter(year %in% year_range) %>%
  group_by(peak_name) %>%
  summarise(total_expeditions = n()) %>%
  top_n(10, total_expeditions) %>%
  arrange(desc(total_expeditions)) %>%
  mutate(peak_name = reorder(peak_name, rev(total_expeditions)))

expeditions_daily <-
  expeditions %>%
  filter(peak_name %in% top_n_freq_peaks$peak_name & !is.na(basecamp_date) & year %in% year_range) %>%
  mutate(day = lubridate::yday(basecamp_date),
         success = if_else(str_detect(termination_reason, "Success"), TRUE, FALSE)) %>%
  group_by(peak_name, day, success) %>%
  summarise(num_expeditions = n()) %>%
  mutate(num_expeditions = if_else(success, num_expeditions, -num_expeditions))

# Use this to fill missing days
all_days <- expand_grid(peak_name = top_n_freq_peaks$peak_name, day = c(1:365), success = c(TRUE, FALSE))

expeditions_daily_all <- all_days %>%
  left_join(expeditions_daily) %>%
  mutate(num_expeditions = replace_na(num_expeditions, 0),
         peak_name = factor(peak_name, levels = top_n_freq_peaks$peak_name, ordered = TRUE))

# Plot
expeditions_daily_all %>%
  group_by(peak_name) %>%
  ggplot(aes(x = day, y = num_expeditions)) +
  geom_area(aes(fill = success), alpha = 1) +
  scale_x_continuous(breaks = seq(15, 365, 30), labels = function(x) lubridate::month(as.Date(x, origin = '2019-01-01'), label = TRUE)) +
  scale_fill_manual(labels = c("terminated expeditions", "successful expeditions"), values = c("#1a1a1a", "#b5b4bf")) +
  facet_grid(rows = vars(peak_name), switch = "y") +
  labs(title = "Expeditions are most frequent around April and October for the top 10 travelled peaks 2000-2019",
       caption = "Visualisation: Michael Chan • Data: The Himalayan Database") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "#818aa1", fill = "#818aa1"),
        plot.title = element_text(size = 16, face = "bold", color = "grey80"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = "grey80"),
        axis.text.y = element_blank(),
        strip.text.y.left = element_text(size = 16, angle = 0, vjust = 0.4, colour = "grey80"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid = element_blank(),
        panel.spacing.y = unit(-8, "lines")) +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE))

path <- here::here("plots", "2020_39", "2020_39_HimalayanClimbers")

ggsave(glue::glue("{path}.pdf"), width = 12, height = 8, device = cairo_pdf)

pdf_convert(pdf = glue::glue("{path}.pdf"),
            filenames =  glue::glue("{path}.png"),
            format = "png",
            dpi = 350)
