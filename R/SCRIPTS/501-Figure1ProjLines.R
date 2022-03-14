###------Figure Proj Lines-----
## @knitr FigProjLines
# source('./R/SCRIPTS/001-fipscodes.R')
projsums<-read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled_MSP.csv") %>%
  mutate(rel = diff / SSP2_BASE) %>%
  left_join(., fipslist) %>%
  mutate(displaced = (1-Inundated)* SSP2_BASE) %>%
  filter(YEAR <= 2100)
projsums$NAME <- str_replace(projsums$NAME, " County", "")
projsums$NAME <- str_replace(projsums$NAME, " Parish", "")

reductions <- read_csv("./R/DATA-PROCESSED/atriskpops.csv") %>%
  mutate(sex = as.character(sex)) %>%
  dplyr::select(GEOID, year, SEX=sex, ccr = groups, prob2, reduce_in, Inundated) %>%
  group_by(GEOID, SEX, prob2, ccr) %>%
  mutate(step = as.character(row_number()))
reductions$COUNTYRACE <- paste0(reductions$GEOID, "_", reductions$RACE)

red <- reductions %>%
  ungroup() %>%
  dplyr::select(GEOID, prob2, YEAR=year, Inundated) %>%
  unique() %>%
  mutate(Inundated = 1-Inundated,
         prob = prob2) %>%
  filter(YEAR == 2100,
         prob == "p95") %>%
  dplyr::select(-YEAR, -prob, -prob2)

proj2 <- projsums %>%
  dplyr::select(-Inundated) %>%
  left_join(.,red) %>%
  mutate(groupings = case_when(
    Inundated >=0.99 ~ "Inland",
    is.na(Inundated) ~ "Inland",
    Inundated < 0.99 ~ "Coastal"
  )) %>%
  group_by(groupings, prob, YEAR) %>%
  dplyr::summarise(Migration = sum(SSP2_MIG),
                   Migration_Upper = sum(Mig_max),
                   Migration_Lower = sum(Mig_min),
                   Base = sum(SSP2_BASE),
                   Base_Upper = sum(Base_max),
                   Base_Lower = sum(Base_min),
                   Inundation = sum(SSP2_inun),
                   Inundation_Upper = sum(Inundated_max),
                   Inundation_Lower = sum(Inundated_min)) %>%
  filter(groupings == "Coastal")
proj_ssp2 <- proj2 %>%
  filter(prob == "p50") %>%
  ungroup() %>%
  dplyr::select(groupings, YEAR,Migration, Base, Inundation) %>%
  pivot_longer(cols = c(Migration, Base, Inundation), names_to ="SSP2")
proj_min <- proj2 %>%
  # filter(prob == "p5") %>%
  ungroup() %>%
  dplyr::select(groupings, YEAR, Migration_Lower, Base_Lower, Inundation_Lower) %>%
  pivot_longer(cols = c(Migration_Lower, Base_Lower, Inundation_Lower), names_to ="SSP2", values_to = "Lower") %>%
  separate(SSP2, c("SSP2", "drop"), sep = "_") %>%
  dplyr::select(-drop)%>%
  group_by(YEAR, SSP2) %>%
  filter(Lower == min(Lower))
proj_max <- proj2 %>%
  # filter(prob == "p95") %>%
  ungroup() %>%
  dplyr::select(groupings, YEAR, Migration_Upper, Base_Upper, Inundation_Upper) %>%
  pivot_longer(cols = c(Migration_Upper, Base_Upper, Inundation_Upper), names_to ="SSP2", values_to = "Upper") %>%
  separate(SSP2, c("SSP2", "drop"), sep = "_") %>%
  dplyr::select(-drop)    %>%
  group_by(YEAR, SSP2) %>%
  filter(Upper == max(Upper))
proj2 <- left_join(proj_ssp2, proj_min) %>%
  left_join(., proj_max) %>%
  unique()

suf <- " M"
sca <- 1e-6

main <- ggplot(data=proj2, aes(x=YEAR, y = value, color = SSP2, group=SSP2)) +
   geom_ribbon(aes(ymin= Lower, ymax=Upper, fill=SSP2, group = SSP2), color=NA, alpha=0.2) +
  geom_line() +
  # geom_dl(aes(label = SSP2), method = "top.points") +
  scale_color_manual(values = c("black", "deepskyblue3", "red")) +
  scale_fill_manual(values = c("black", "deepskyblue3", "red")) +
  # geom_ribbon(aes(ymin= SSP3_MIG, ymax=SSP5_MIG), fill="red", alpha=0.3, color=NA) +
  # geom_line(aes(x=YEAR, y = mean_mig), color = "red") +
  annotate("text", x=2093, y=53609112*1.04, label= "Amplification", color="red", size = 2) +
  # # geom_ribbon(aes(ymin= SSP3_BASE, ymax=SSP5_BASE), fill="black", alpha=0.3, color=NA) +
  # geom_line(aes(x=YEAR, y = mean_base), color = "black") +
  annotate("text", x=2093, y=63177734*1.1, label= "Base", color="black", size = 2) +
  # geom_line(aes(x=YEAR, y = mean_inun), color = "deepskyblue3") +
  annotate("text", x=2093, y=58328132*1.1, label= "Displacement", color="deepskyblue3", size = 2) +
  # # scale_y_continuous(labels = scales::comma) +
  # annotate("text", x=2025, y=0.25, label = percent(min(proj_cnty$rel))) +
  scale_y_continuous(labels = label_number(suffix = suf, scale = sca),
                     # limits = c(0,NA)
  ) +
  # ylim(0,NA) +
  labs(title = "All Coastal Counties Impacted by SLR",
       y = "Population",
       x = "Year") +
  theme_bw()+
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=10),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8)) +
  NULL




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
  proj_cnty <- projsums[which(projsums$GEOID == cnty),] %>%
    dplyr::select(-Inundated) %>%
    group_by(YEAR, prob) %>%
    dplyr::summarise(Migration = sum(SSP2_MIG),
                     Migration_Upper = sum(Mig_max),
                     Migration_Lower = sum(Mig_min),
                     Base = sum(SSP2_BASE),
                     Base_Upper = sum(Base_max),
                     Base_Lower = sum(Base_min),
                     Inundation = sum(SSP2_inun),
                     Inundation_Upper = sum(Inundated_max),
                     Inundation_Lower = sum(Inundated_min))
  
  proj_ssp2 <- proj_cnty %>%
    filter(prob == "p50") %>%
    ungroup() %>%
    dplyr::select(YEAR,Migration, Base, Inundation) %>%
    pivot_longer(cols = c(Migration, Base, Inundation), names_to ="SSP2")
  proj_min <- proj_cnty %>%
    # filter(prob == "p5") %>%
    ungroup() %>%
    dplyr::select(YEAR, Migration_Lower, Base_Lower, Inundation_Lower) %>%
    pivot_longer(cols = c(Migration_Lower, Base_Lower, Inundation_Lower), names_to ="SSP2", values_to = "Lower") %>%
    separate(SSP2, c("SSP2", "drop"), sep = "_") %>%
    dplyr::select(-drop) %>%
    group_by(YEAR, SSP2) %>%
    filter(Lower == min(Lower))
  proj_max <- proj_cnty %>%
    # filter(prob == "p95") %>%
    ungroup() %>%
    dplyr::select(YEAR, Migration_Upper, Base_Upper, Inundation_Upper) %>%
    pivot_longer(cols = c(Migration_Upper, Base_Upper, Inundation_Upper), names_to ="SSP2", values_to = "Upper") %>%
    separate(SSP2, c("SSP2", "drop"), sep = "_") %>%
    dplyr::select(-drop)  %>%
    group_by(YEAR, SSP2) %>%
    filter(Upper == max(Upper))
  proj2 <- left_join(proj_ssp2, proj_min) %>%
    left_join(., proj_max) %>%
    unique()
  
  nam <- paste0(projsums[which(projsums$GEOID==cnty),]$NAME, ", ",
                projsums[which(projsums$GEOID==cnty),]$state)
  
  suf <- if(max(proj2$Upper, na.rm=T)/1000000 >1){" M"} else {" K"}
  sca <- if(max(proj2$Upper, na.rm=T)/1000000 >1){1e-6} else {1e-3}
  # percent(min(proj_cnty$rel))
  ggplot(data=proj2, aes(x=YEAR, y = value, color = SSP2, group=SSP2)) +
    geom_ribbon(aes(ymin= Lower, ymax=Upper, fill=SSP2, group = SSP2), color=NA, alpha=0.2) +
    geom_line() +
    # geom_dl(aes(label = SSP2), method = "top.points") +
    scale_color_manual(values = c("black", "deepskyblue3", "red")) +
    scale_fill_manual(values = c("black", "deepskyblue3", "red")) +
    scale_y_continuous(labels = label_number(suffix = suf, scale = sca),
                       # limits = c(0,NA)
    ) +
    # ylim(0,NA) +
    labs(title = nam,
         y = "Population",
         x = "Year") +
    theme_bw()+
    theme(legend.position = "none") +
    theme(plot.title = element_text(size=7),
          axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    NULL
}
# figure_countyproj("39049")
# figure_countyproj("37183")

projsums2 <- projsums[which(projsums$YEAR %in% c(2050, 2100)),] %>%
  mutate(absinc = SSP2_MIG - SSP2_BASE)
z <- projsums2 %>% dplyr::select(YEAR, rel, GEOID, NAME, prob, state) %>%
  pivot_wider(names_from = YEAR, values_from = rel)

loss_miami <- figure_countyproj("12086")
loss_Poqu <- figure_countyproj("37055")
loss_Calic <- figure_countyproj("12087")
loss_Beau <- figure_countyproj("12109")

vul_Baton <- figure_countyproj("47149")
vul_Chesa <- figure_countyproj("13191")
vul_Browa <- figure_countyproj("36061")
vul_Queen <- figure_countyproj("51133")

dest_Orla <- figure_countyproj("08035")
dest_Lafa <- figure_countyproj("41067")
dest_Stan <- figure_countyproj("53033")
dest_wake <- figure_countyproj("06105")

side <-plot_grid(vul_Baton, dest_Orla, dest_Lafa, ncol=1, labels = c("b", "c", "d", "h"),
                 label_size = 10)
bot <- plot_grid(loss_miami, loss_Poqu, vul_Chesa, vul_Browa, ncol=4, labels = c("e", "f", "g", "h"),
                 label_size = 10)
com <-plot_grid(main, side, ncol=2, rel_widths = c(1,0.33), labels = c("a", ""),
                label_size = 10)
plot_grid(com, bot, ncol=1, rel_heights = c(1, 0.33))


ggsave("./MANUSCRIPT/MainDocument/FigProjLines2_uncertainty.pdf", width=7, height=4.5)