source('./R/SCRIPTS/001-fipscodes.R')

# projsums <- test2 %>%
projsums<-read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled_MSP.csv") %>%
  # mutate(rel = diff / SSP2_BASE) %>%
  left_join(., fipslist) %>%
  mutate(#displaced = (1-Inundated)* SSP2_BASE,
         absdiff = ifelse( abs(SSP2_MIG - SSP2_BASE)<1, 
                           NA,
                           SSP2_MIG - SSP2_BASE),
         GEOID = case_when(
           GEOID == "46113" ~ "46102",
           TRUE ~ GEOID
         )) %>%
  filter(YEAR == 2100,
         prob == "p50") %>%
  dplyr::select(YEAR, GEOID, absdiff)

mapdat <- get_acs("county",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE) %>%
  left_join(., projsums)

states <- get_acs("state",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE) 

# BAMMtools::getJenksBreaks(projsums$absdiff, k = 9)
breaksrel <- c(-Inf, -250000, -100000, -50000, -10000, 0, 10000, 100000, 500000, Inf)

labels1 <-  c("< -250K", "-249K to -100K", "-99K to -50K", "-49K to -10K", "-10K to 0",
              "0 to 10K", "11K to 100K", "101K to 500K", "500K+")
projsums$groups_dif <- factor(
  cut(projsums$absdiff, breaksrel),
  labels = labels1)

pal <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffd4", #  "#ffffff", 
         "#bdc9e1", "#67a9cf", "#1c9099", "#016c59"
         # RColorBrewer::brewer.pal(4, "Blues") 
         )



a<- tm_shape(mapdat) +
  tm_polygons("absdiff",
              title = "",
              # midpoint = 0,
              showNA = FALSE,
             colorNA ="white",
              textNA = "NA",
              border.alpha =0.5,
              breaks= breaksrel,
              palette = pal,
              legend.is.portrait = F,
              labels = labels1
              # style ="fisher",
              # n = 8
  ) +
  tm_shape(states) +
  tm_borders(lwd=1, col="black", alpha = 0.5) +
  tm_layout(legend.position = c("right", "bottom"),
            legend.title.size = 0.6,
           # main.title = "\u0394 Median Age in 2100",
            legend.width = 0.52,
            legend.stack = "horizontal",
            #  legend.outside =TRUE,
            # legend.outside.position = "bottom",
            legend.text.size = 0.6,
            frame = F)
# cairo_pdf("./MANUSCRIPT/MainDocument/FigMedAgeMap2.pdf", width = 7,
#           height = 5)
tmap_save(a, "./MANUSCRIPT/MainDocument/FigMedAgeMap3.pdf", width = 7,
          height = 5)