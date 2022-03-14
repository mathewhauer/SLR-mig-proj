###------Figure MedianAge-----
## @knitr MedAge

source('./R/SCRIPTS/001-fipscodes.R')
proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled_MSP.csv")

proj2 <- proj %>%
  group_by(GEOID, YEAR, AGE, prob) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  group_by(GEOID, YEAR, prob) %>%
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

proj3 <- proj2 %>% ungroup() %>% filter(YEAR == 2100, prob == "p50") %>%top_n(., 10, medagediff) %>%
  dplyr::select(GEOID, medage_base, medage_mig, medagediff) %>%
  mutate(groupa = "Top") %>%
  filter(medagediff <20)
proj4 <-  proj2 %>% ungroup() %>% filter(YEAR == 2100, prob == "p50") %>%top_n(., -10, medagediff) %>%
  dplyr::select(GEOID, medage_base, medage_mig, medagediff) %>%
  mutate(groupa = "Bot")%>%
  filter(medagediff <20)
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
                colour_x = "red", colour_xend = "black",
                size=0.5) +
  theme_bw() +
  labs(x = "Median Age",
       y = "") +
  # annotate("text", y = "York, SC", x = 57.5, label = "• Migration", color = "red",
  #          hjust = 0, size =2) +
  # annotate("text", y = "Pearl River, MS", x = 57.5, label = "• Base", color = "black",
  #          hjust = 0, size=2) +
  theme(axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=5),
        axis.title.y = element_text(size=5)) +
  geom_hline(yintercept= 10.5) +
  NULL



proj_growdec <- proj %>%
  group_by(YEAR, GEOID, prob) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  mutate(diff = mean_mig / mean_base,
  ) %>%
  filter(YEAR == 2100) %>%
  dplyr::select(GEOID, 
                diff, prob) %>%
  right_join(proj2, .) %>%
  ungroup() %>%
  dplyr::select(GEOID, YEAR, medage_mig, medage_base, diff, prob)

a <- proj_growdec[which(proj_growdec$YEAR == 2100),] %>%
  mutate(diff2 = medage_mig - medage_base,
         diff = diff-1) %>%
  filter(diff2 <20)

# plot(a$diff, a$diff2)

b<- ggplot(a[which(a$prob=="p50"),], aes(x= diff, y = diff2)) +
  geom_hline(yintercept = 0, color = "dark gray") +
  geom_point(size=0.5, alpha = 0.2) +
  geom_smooth(size = 0.5) +
  coord_cartesian(ylim=c(-7, 20)) +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x= "\u0394 % Population",
       y="\u0394 Median Age",
       title = "2100") +
  theme()

reductions <- read_csv("./R/DATA-PROCESSED/atriskpops.csv") %>%
  mutate(sex = as.character(sex)) %>%
  dplyr::select(GEOID, year, SEX=sex, ccr = groups, reduce_in, Inundated, prob = prob2) %>%
  group_by(GEOID, SEX,ccr) %>%
  mutate(step = as.character(row_number()))
reductions$COUNTYRACE <- paste0(reductions$GEOID, "_", reductions$RACE)

red <- reductions %>%
  ungroup() %>%
  dplyr::select(GEOID, YEAR=year, Inundated, prob) %>%
  unique() %>%
  mutate(Inundated = 1-Inundated)

proj <- read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_AS_controlled_MSP.csv")
proj2 <- proj %>%
  group_by(GEOID, YEAR, AGE, prob,SEX) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  group_by(GEOID, YEAR, prob) %>%
  mutate(counterfact = mean_base/sum(mean_base) * sum(mean_mig),
         diff = mean_mig - counterfact) %>%
  left_join(.,red) %>%
  mutate(groupings = case_when(
    Inundated >=0 ~ "Coastal",
    is.na(Inundated) ~ "Inland"
  )) %>%
  group_by(AGE, groupings,YEAR,SEX, prob) %>%
  dplyr::summarise(trapped = sum(diff))

figdat <- proj2 %>%
  filter(groupings == "Coastal",
         AGE >= 17) %>%
  # mutate(agegroups = case_when(
  #   AGE >= 17 ~ "age 75+",
  #   AGE %in% c(5,6,7,8,9,10,11,12,13) ~ "age 20-65"
  # )) %>%
  group_by(YEAR, prob, SEX) %>%
  dplyr::summarise(stuck = sum(trapped)) %>%
  mutate(SEX = ifelse(SEX==1, "Men", "Women")) %>%
  na.omit

stucktext <- figdat %>%
  group_by(prob, YEAR) %>%
  dplyr::summarise(stuck = sum(stuck))

c<- 
  ggplot(figdat[which(figdat$prob=="p50"),], aes(x=YEAR, y= stuck, fill= SEX)) +
  geom_bar(stat="identity") +
  # geom_dl(aes(label = agegroups), method= "top.points") +
  scale_y_continuous(labels = scales::comma,
                      breaks = c(seq(0,225000,50000)),
                     limits = c(0,230000) )+
  #                    # limits = c(0,max(figdat$stuck[which(figdat$prob=="p50")]))) +
  scale_x_continuous(limits = c(2020,2107)) +
  scale_fill_manual(values = c("dark blue", "dark red")) +
  annotate("text", x = 2106, y = 200000, label = "Men", color = "dark blue", size=1) +
  annotate("text", x = 2106, y = 100000, label = "Women", color = "dark red",size=1) +
  theme_bw() +
  theme(legend.position="none") +
  # guides()
  labs(x="Year",
       y= "Population",
       title = "'Demographically Stuck' aged 75+")

right <- plot_grid(b + theme(
  axis.text.x = element_text(size=3),
  axis.text.y = element_text(size=3),
  axis.title.x = element_text(size=4),
  axis.title.y = element_text(size=4),
  plot.title = element_text(size=5)),
  c + theme(
    axis.text.x = element_text(size=3),
    axis.text.y = element_text(size=3),
    axis.title.x = element_text(size=4),
    axis.title.y = element_text(size=4),
    plot.title = element_text(size=5))
  ,ncol=2,labels=c("b", "c"))

plot_grid(lollipop, right, ncol=1, labels = c("a", ""), rel_heights=c(1.5, 1))

plot_grid(b + theme(
  axis.text.x = element_text(size=3),
  axis.text.y = element_text(size=3),
  axis.title.x = element_text(size=4),
  axis.title.y = element_text(size=4),
  plot.title = element_text(size=5)), c + theme(
  axis.text.x = element_text(size=3),
  axis.text.y = element_text(size=3),
  axis.title.x = element_text(size=4),
  axis.title.y = element_text(size=4),
  plot.title = element_text(size=5),
  panel.grid.minor = element_blank()), ncol=1, labels = c("a", "b"), rel_heights=c(1, 1))

b

ggsave("./MANUSCRIPT/MainDocument/FigAging3.pdf", 
       width = 2.4, height=2.4,
       device=cairo_pdf)