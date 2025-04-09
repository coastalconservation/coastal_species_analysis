lat_min <- 34
lat_max <- 35

longterm_danger <- filter(longterm_clean, between(latitude, lat_min, lat_max))

longterm_danger %>% 
  count(marine_site_name, target_assemblage, quadrat_code) %>% 
  view()

longterm_presence <- longterm_danger %>% 
  group_by(marine_site_name, year, lumping_code) %>% 
  summarize(present = any(percent_cover > 0),
            .groups = "drop")

ggplot(longterm_presence, aes(year, present)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
  facet_wrap(~lumping_code)

# FUCGAR
longterm_presence %>% 
  filter(lumping_code == "FUCGAR") %>% 
  ggplot(aes(year, present)) +
  geom_jitter(width = 0.2, height = 0.2) +
  theme_classic()
