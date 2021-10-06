###------Figure MedianAge-----
## @knitr MedAge

proj <- read_csv("../R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled.csv")

proj2 <- proj %>%
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
  mutate(medagediff = medage_mig - medage_base)

proj3 <- proj2 %>% ungroup() %>% filter(YEAR == 2100) %>%top_n(., 10, medagediff) %>%
  dplyr::select(GEOID, medage_base, medage_mig, medagediff) %>%
  mutate(groupa = "Top")
proj4 <-  proj2 %>% ungroup() %>% filter(YEAR == 2100) %>%top_n(., -10, medagediff) %>%
  dplyr::select(GEOID, medage_base, medage_mig, medagediff) %>%
  mutate(groupa = "Bot")
proj3 <- rbind(proj3, proj4) %>%
  group_by(groupa) %>%
  # pivot_longer(cols = c(medage_base, medage_mig)) %>%
  arrange(medage_mig, .by_group = TRUE) %>%
  left_join(., fipslist) %>%
  mutate(ID = paste0(NAME,", ", state))
proj3$ID <- str_replace(proj3$ID, " County", "")
proj3$ID <- str_replace(proj3$ID, " Parish", "")
proj3$name <- factor(proj3$ID, levels=as.character(proj3$ID))


lollipop <-    ggplot(proj3, aes(x= `medage_mig`, xend = `medage_base`, y = name, group = name)) +
  geom_dumbbell(color="#b2b2b2", 
                colour_x = "red", colour_xend = "blue",
                size=2) +
  theme_bw() +
  labs(x = "Median Age",
       y = "") +
  annotate("text", y = "Allen, LA", x = 57.5, label = "• Migration", color = "red",
           hjust = 0) +
  annotate("text", y = "Effingham, GA", x = 57.5, label = "• Base", color = "blue",
           hjust = 0) +
  NULL



proj_growdec <- proj %>%
  group_by(YEAR, GEOID) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  mutate(diff = mean_mig / mean_base,
  ) %>%
  filter(YEAR == 2100) %>%
  dplyr::select(GEOID, 
                diff) %>%
  right_join(proj2, .) %>%
  
  dplyr::select(GEOID, YEAR, medage_mig, medage_base, diff)

a <- proj_growdec[which(proj_growdec$YEAR == 2100),] %>%
  mutate(diff2 = medage_mig - medage_base,
         diff = diff-1)

# plot(a$diff, a$diff2)

b<- ggplot(a, aes(x= diff, y = diff2)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x= "\u0394 % Population",
       y="\u0394 Median Age",
       title = "2100")

projsums<-read_csv("../R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled.csv") %>%
  mutate(rel = diff / SSP2_BASE) %>%
  left_join(., fipslist)

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
    # annotate("text", x=2025, y=0.25, label = percent(min(proj_cnty$rel))) +
    scale_y_continuous(labels = label_number(suffix = suf, scale = sca),
                       # limits = c(0,NA)
    ) +
    # ylim(0,NA) +
    labs(title = nam,
         y = "Population",
         x = "Year") +
    theme_bw()+
    theme(plot.title = element_text(size=7),
          axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          axis.title.x = element_text(size=8),
          axis.title.y = element_text(size=8)) +
    NULL
}

c<- figure_countyproj("22023")

right <- plot_grid(b + theme(
  axis.text.x = element_text(size=6),
  axis.text.y = element_text(size=6),
  axis.title.x = element_text(size=7),
  axis.title.y = element_text(size=7)),c,ncol=2,labels=c("b", "c"))

plot_grid(lollipop, right, ncol=1, labels = c("a", ""), rel_heights=c(1.5, 1))