# one year
library(tidyverse)
agaei_2001 <- biodiv_df %>% filter(species_lump == "Agathistoma eiseni",
                              year == 2001)
ggplot(agaei_2001) +
  geom_point(aes(x=latitude, y=density_per_m2))

agaei_2001 <- agaei_2001 %>% 
  arrange(latitude) %>% 
  mutate(
    cum_den = cumsum(density_per_m2),
    cum_den_norm = cum_den/ max(cum_den)
  )

ggplot(agaei_2001) +
  geom_point(aes(x=latitude, y=cum_den_norm))

agaei_logit <- glm(cum_den_norm ~ latitude, binomial(link="logit"), agaei_2001)

agaei_pred <- tibble(latitude = seq(33, 47, length.out=1000)) %>% 
  mutate(
    cum_den_norm = predict(agaei_logit, newdata = ., type="response")
  ) 

northern_extend <- approx(agaei_pred$cum_den_norm, agaei_pred$latitude, xout=0.95)$y

ggplot(agaei_2001, aes(x=latitude, y=cum_den_norm)) +
  geom_point() +
  geom_line(data=agaei_pred, color="blue")


### all years

agaei <- biodiv_df %>% filter(species_lump == "Agathistoma eiseni")

ggplot(agaei) +
  geom_point(aes(x=latitude, y=density_per_m2, color=year))

agaei <- agaei %>% 
  group_by(year) %>% 
  arrange(latitude) %>% 
  mutate(
    cum_den = cumsum(density_per_m2),
    cum_den_norm = cum_den/ max(cum_den)
  )

ggplot(agaei) +
  geom_point(aes(x=latitude, y=cum_den_norm, color=year))

agaei_logit <- glm(cum_den_norm ~ latitude * year, binomial(link="logit"), agaei)

agaei_pred <- expand_grid(latitude = seq(25, 40, length.out=1000),
                          year = 2000:2024) %>% 
  mutate(
    cum_den_norm = predict(agaei_logit, newdata = ., type="response")
  ) 

northern_extend <- agaei_pred %>% 
  group_by(year) %>% 
  summarise(
    max_lat = approx(cum_den_norm, latitude, xout=0.95)$y
  )

ggplot(agaei, aes(x=latitude, y=cum_den_norm, color=year)) +
  geom_point() +
  geom_line(aes(group = year), data=agaei_pred) +
  xlim(25, 40)

ggplot(northern_extend, aes(year, max_lat)) +
  geom_point()
