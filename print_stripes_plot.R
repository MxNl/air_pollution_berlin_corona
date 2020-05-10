print_plot <- station_data %>%
  select(-stickstoffdioxid, -stickstoffmonoxid) %>%
  pivot_longer(cols = one_of(variables)) %>%
  group_by(name, year, yearday) %>%
  summarise(mean_year_yearday = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(yearday, year, group = name, fill = mean_year_yearday)) +
  geom_tile() +
  # scale_x_datetime(date_breaks = "month") +
  # scale_fill_gradient(low = 'darkgreen', high = 'red') +
  scale_fill_viridis_c() +
  scale_x_continuous(
    breaks = seq(0, 365, 10),
    guide = guide_axis(n.dodge = 2)
  ) +
  scale_y_reverse(breaks = unique(pull(station_data, year))) +
  labs(fill = "mean") +
  theme_void() +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = 'gray10'))


print_plot %>% 
  ggsave("print_plot.png", ., width = 60, height = 40, units = "cm")
