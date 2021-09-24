
projsums <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_TOT.csv") %>%
  filter(YEAR == 2100) %>%
  mutate(dif = mean_mig - mean_base,
         rel = dif / mean_base) %>%
  mutate(dif = ifelse(dif == 0 , NA, dif),
         rel = ifelse(rel == 0, NA, rel))

## Getting the natural breaks and then slightly alterting them
# getJenksBreaks(projsums$dif, k = 8)
# getJenksBreaks(projsums$rel, k = 8)
breaksdif <- c(-Inf, -500000, -150000, -25000, 0, 15000, 35000, 100000, Inf)
breaksrel <- c(-Inf, -0.5, -0.20, -0.05, 0, 0.05, 0.15, 0.5, Inf)
projsums$groups_dif <- factor(
  cut(projsums$dif, breaksdif),
  labels = c("< -500K", "-499K to -150K", "-149K to -25K", "-24K to -1", "1 to 15K",
  "16K to 35K", "35K to 100K", "100K+"))
projsums$groups_rel <- factor(
  cut(projsums$rel, breaksrel),
  labels = c("< -50%", "-50% to -20%", "-20% to -5%", "-5% to -1%", "1% to 5%",
             "5% to 15%", "15% to 50%", "50%+"))

mapdat <- get_acs("county",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE) %>%
  left_join(., projsums)



states <- get_acs("state",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE)

a<- ggplot(mapdat, aes(fill=groups_dif)) +
  geom_sf() +
  scale_fill_brewer(type = "div", palette = "RdYlBu", na.value="grey50") +
  geom_sf(data=states, aes(fill=NULL),alpha=0, color="black") +
  coord_sf(datum = NA) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(fill = "Numeric Difference")

b<- ggplot(mapdat, aes(fill=groups_rel)) +
  geom_sf() +
  scale_fill_brewer(type = "div", palette = "RdYlBu", na.value="grey50") +
  geom_sf(data=states, aes(fill=NULL),alpha=0, color="black") +
  coord_sf(datum = NA) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(fill = "Percent Difference")

comb <- plot_grid(a, b, nrow=2, labels = "auto")

ggsave("countymap.pdf", comb, height = 12)
