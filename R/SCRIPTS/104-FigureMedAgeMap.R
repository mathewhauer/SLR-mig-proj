projsums <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_TOT.csv") %>%
  filter(YEAR == 2100) %>%
  mutate(dif = mean_mig - mean_base,
         rel = dif / mean_base) %>%
  mutate(dif = ifelse(dif == 0 , NA, dif),
         rel = ifelse(rel == 0, NA, rel)) %>%
  filter(!is.na(dif))


proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled.csv")

proj2 <- proj %>%
  filter(GEOID %in% projsums$GEOID,
         YEAR == 2100) %>%
  group_by(GEOID, YEAR, AGE) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  group_by(GEOID, YEAR) %>%
  mutate(csum = cumsum(mean_mig),
         tot = sum(mean_mig),
         mid = if_else((csum/tot)<=0.5,1,0),
         midpoint = tot/2,
         medage_mig = (midpoint-csum)/mean_mig*5 + (AGE*5-5),
         
         csum_base = cumsum(mean_base),
         tot_base = sum(mean_base),
         mid_base = if_else((csum_base/tot_base)<=0.5,1,0),
         midpoint_base = tot_base/2,
         medage_base = (midpoint_base-csum_base)/mean_base*5 + (AGE*5-5),) %>%
  filter(mid == 1) %>%
  filter(AGE == max(AGE)) %>%
  mutate(medagediff = round(medage_mig - medage_base,2)) %>%
  filter(YEAR == 2100) %>%
  dplyr::select(GEOID, YEAR, medage_mig, medage_base, medagediff)

# proj2$medagediff <- ifelse(proj2$medagediff==0, -0.0001, proj2$medagediff)

# BAMMtools::getJenksBreaks(proj2$medagediff, k = 8)
# breaksrel <- c(-Inf, -0.73, -0.1, 0, 1, 2.5, 4.5, 10, Inf)
# proj2$groups_dif <- factor(
#   cut(proj2$medagediff, breaksrel),
#   labels = c("< -500K", "-499K to -150K", "-149K to -25K", "-24K to -1", "1 to 15K",
#              "16K to 35K", "35K to 100K", "100K+"))
mapdat <- get_acs("county",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE) %>%
  left_join(., proj2)


states <- get_acs("state",
                  "B19013_001",
                  geometry=TRUE,
                  shift_geo=TRUE) 

pal <- c("#4575b4", "#91bfdb", "#ffffff", RColorBrewer::brewer.pal(6, "Reds") )
labels1 <- c("< -0.50",
            "-0.50 - 0.01",
            "0",
            "0.01 - 0.75",
            "0.76 - 1.5",
            "1.51 - 2.0",
            "2.01 - 3.0",
            "3.01 - 7.0",
            "> 7.01")
a <- tm_shape(mapdat) +
  tm_polygons("medagediff",
              title = "",
              # midpoint = 0,
              showNA = TRUE,
              textNA = "NA",
              border.alpha =0.5,
              breaks= c(-Inf, -0.5, 0, 0.011, 0.75, 1.5, 2, 3, 7, Inf),
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
              main.title = "\u0394 Median Age in 2100",
               legend.width = 0.52,
              legend.stack = "horizontal",
              #  legend.outside =TRUE,
              # legend.outside.position = "bottom",
              legend.text.size = 0.6,
              frame = F)
cairo_pdf("./MANUSCRIPT/MainDocument/FigMedAgeMap.pdf", width = 7,
          height = 5)
a
dev.off()
# tmap_save(a, "FigMedAgeMap1.png")
# tmap_save(a, "FigMedAgeMap1.pdf")
# 
# ggsave("./MANUSCRIPT/MainDocument/FigProjLines.pdf", width=7, height=4.5)
# 
# ggplot(mapdat, aes(fill=medagediff)) +
#   geom_sf()
# 
# # knitr::include_graphics("../countymap.pdf")
# 
# projsums <- read_csv("../R/DATA-PROCESSED/PROJECTIONS/projections_TOT.csv") %>%
#   filter(YEAR == 2100) %>%
#   mutate(dif = mean_mig - mean_base,
#          rel = dif / mean_base) %>%
#   mutate(dif = ifelse(dif == 0 , NA, dif),
#          rel = ifelse(rel == 0, NA, rel))
# 
# ## Getting the natural breaks and then slightly alterting them
# # getJenksBreaks(projsums$dif, k = 8)
# # getJenksBreaks(projsums$rel, k = 8)
# breaksdif <- c(-Inf, -500000, -150000, -25000, 0, 15000, 35000, 100000, Inf)
# breaksrel <- c(-Inf, -0.5, -0.20, -0.05, 0, 0.05, 0.15, 0.5, Inf)
# projsums$groups_dif <- factor(
#   cut(projsums$dif, breaksdif),
#   labels = c("< -500K", "-499K to -150K", "-149K to -25K", "-24K to -1", "1 to 15K",
#              "16K to 35K", "35K to 100K", "100K+"))
# projsums$groups_rel <- factor(
#   cut(projsums$rel, breaksrel),
#   labels = c("< -50%", "-50% to -20%", "-20% to -5%", "-5% to -1%", "1% to 5%",
#              "5% to 15%", "15% to 50%", "50%+"))
# 
# mapdat <- get_acs("county",
#                   "B19013_001",
#                   geometry=TRUE,
#                   shift_geo=TRUE) %>%
#   left_join(., projsums)
# 
# 
# 
# states <- get_acs("state",
#                   "B19013_001",
#                   geometry=TRUE,
#                   shift_geo=TRUE)
# addSmallLegend <- function(myPlot, pointSize = 2, textSize = 8, spaceLegend = 1) {
#   myPlot +
#     guides(shape = guide_legend(override.aes = list(size = pointSize)),
#            color = guide_legend(override.aes = list(size = pointSize))) +
#     theme(legend.title = element_text(size = textSize),
#           legend.text  = element_text(size = textSize),
#           legend.key.size = unit(spaceLegend, "lines"))
# }
# a<- ggplot(mapdat, aes(fill=groups_dif)) +
#   geom_sf() +
#   scale_fill_brewer(type = "div", palette = "RdYlBu", na.value="grey50") +
#   geom_sf(data=states, aes(fill=NULL),alpha=0, color="black") +
#   coord_sf(datum = NA) +
#   theme_bw() +
#   theme(legend.direction="horizontal") +
#   theme(legend.position = c(0.7, 0.05)) +
#   labs(fill = "Difference")
# addSmallLegend(a, pointSize = 1.5, textSize = 6, spaceLegend = 0.5)