###------Figure Proj Lines-----
## @knitr FigProjLines
projsums<-read_csv("../R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled.csv") %>%
  mutate(rel = diff / SSP2_BASE) %>%
  left_join(., fipslist)
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
# figure_countyproj("06001")
# figure_countyproj("37183")

loss_miami <- figure_countyproj("12086")
loss_Poqu <- figure_countyproj("51735")
loss_Calic <- figure_countyproj("22019")
loss_Beau <- figure_countyproj("37013")

vul_Baton <- figure_countyproj("22121")
vul_Chesa <- figure_countyproj("51550")
vul_Browa <- figure_countyproj("12011")
vul_Queen <- figure_countyproj("36081")

dest_Orla <- figure_countyproj("12095")
dest_Lafa <- figure_countyproj("22055")
dest_Stan <- figure_countyproj("06099")
dest_wake <- figure_countyproj("37183")

titlecol <- "Dark Green"
losstitle <- ggplot() + 
  labs(title = "Vulnerable Counties") +
  theme_void() +
  theme(plot.title = element_text(color = titlecol,
                                  hjust = 0.5))
losses <- plot_grid(loss_miami, loss_Beau, 
                    #loss_Calic, 
                    loss_Poqu,
                    ncol=3)
losses <- plot_grid(losstitle, losses, ncol=1, rel_heights = c(0.15, 1))

dest1title <- ggplot() + 
  labs(title = "Recipient Counties") +
  theme_void() +
  theme(plot.title = element_text(color = titlecol,hjust = 0.5))
dest1 <- plot_grid(vul_Baton, #vul_Chesa,
                   vul_Browa, vul_Queen, ncol=3)
dest1 <- plot_grid(dest1title, dest1, ncol=1, rel_heights = c(0.15, 1))

dest2title <- ggplot() + 
  labs(title = "Climate Destinations") +
  theme_void() +
  theme(plot.title = element_text(color = titlecol,hjust = 0.5))
dest2 <- plot_grid(dest_Lafa,
                   dest_Orla, dest_wake, ncol=3)
dest2 <- plot_grid(dest2title, dest2, ncol=1, rel_heights = c(0.15, 1))

plot_grid(losses, dest1, dest2,
          ncol=1)