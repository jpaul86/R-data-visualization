```{r}
# Prepare a clean, beautifully simplistic data frames with just totals and greens for our plot
green_selection_plot_3 <- selection_plot_3

green_selection_plot_3 <- green_selection_plot_3 %>%
  filter(geo_code %in% c("EU27_2020", "DE", "FR", "IT", "ES")) %>% # select further
  group_by(geo_code) %>%
  mutate(greens = values - lag(values, default = first(values))) %>% 
  filter(siec == "Total") %>% 
  select(geo_code, geo, time, values, greens) %>% 
  rename(total = "values") %>% 
  mutate(geo=replace(geo, geo_code=="DE", "Germany")) %>%
  mutate(geo=replace(geo, geo_code=="EU27_2020", "Average - 27 EU countries")) %>%
  mutate(total=replace(total, geo_code=="EU27_2020", (total/27))) %>% 
  mutate(greens=replace(greens, geo_code=="EU27_2020", (greens/27))) #%>%   
#as.data.frame()
```


plot_2_1 <- ggplot(merged_selection_plot_2, aes(GWh, price, size = price, colour = geo_code)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  #scale_size(range = c(2, 12)) +
  #scale_colour_manual(values = country_colors) +
  scale_x_log10() +
  facet_wrap(~geo) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(time) +
  ease_aes('linear')