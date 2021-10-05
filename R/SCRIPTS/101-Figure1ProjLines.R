###------Figure Proj Lines-----
## @knitr FigProjLines
projsums<-read_csv("../R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled.csv") %>%
  mutate(rel = diff / SSP2_BASE)
# source('../R/SCRIPTS/001-fipscodes.R')

demotrapped <- projsums %>%
  filter(diff <0,
         YEAR==2100,
         SSP2_MIG >= SSP2_inun)

top2 <- projsums[which(projsums$YEAR==2100),] %>%
  top_n(., -6, diff) %>%
  arrange(diff)

top3 <- projsums[which(projsums$YEAR==2100),] %>%
  top_n(., -4, rel)%>%
  arrange(rel)

# cnty <- "12086"
figure_countyproj <- function(cnty){
proj_cnty <- projsums[which(projsums$GEOID == cnty),] 

nam <- paste0(fipslist[which(fipslist$GEOID==cnty),]$NAME, ", ",
             fipslist[which(fipslist$GEOID==cnty),]$state)

suf <- if(max(proj_cnty$SSP2_BASE)/1000000 >1){" M"} else {" K"}
sca <- if(max(proj_cnty$SSP2_BASE)/1000000 >1){1e-6} else {1e-3}
percent(min(proj_cnty$rel))
ggplot(data=proj_cnty, aes(x=YEAR, y = SSP2_MIG)) +
  # geom_ribbon(aes(ymin= SSP3_MIG, ymax=SSP5_MIG), fill="red", alpha=0.3, color=NA) +
  geom_line(aes(x=YEAR, y = SSP2_MIG), color = "red") +
  # annotate("text", x=2081, y=max(proj_cnty$SSP2_MIG)*0.98, label= "Migration", color="red") +
  # geom_ribbon(aes(ymin= SSP3_BASE, ymax=SSP5_BASE), fill="black", alpha=0.3, color=NA) +
  geom_line(aes(x=YEAR, y = SSP2_BASE), color = "black") +
  # # annotate("text", x=2086, y=max(proj_cnty$SSP2_BASE)*0.95, label= "Base", color="black") +
  geom_line(aes(x=YEAR, y = SSP2_inun), color = "deepskyblue3") +
  # # annotate("text", x=2093, y=max(proj_cnty$mean_inun)*0.92, label= "Inundation", color="deepskyblue3") +
  # scale_y_continuous(labels = scales::comma) +
  annotate("text", x=2025, y=0.25, label = percent(min(proj_cnty$rel))) +
  scale_y_continuous(labels = label_number(suffix = suf, scale = sca),
                     limits = c(0,NA)) +
  # ylim(0,NA) +
  labs(title = nam,
       y = "Population",
       x = "Year")+
  theme_bw()+
  NULL
}

a <- figure_countyproj("12086")
# b <- figure_countyproj("48167")
# c <- figure_countyproj("37095")
d <- figure_countyproj("51735")
e <- figure_countyproj("22019")
f <- figure_countyproj("37013")

# com <- 
  plot_grid(e,a, f,d,
          ncol=2)

# ggsave("countyproj.pdf")

