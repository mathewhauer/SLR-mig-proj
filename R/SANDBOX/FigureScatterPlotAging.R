proj2


proj_growdec <- proj %>%
  group_by(YEAR, GEOID) %>%
  dplyr::summarise(mean_mig = sum(SSP2_MIG),
                   mean_base= sum(SSP2_BASE)) %>%
  ungroup() %>%
  mutate(diff = mean_mig / mean_base,
         # groupa = if_else(diff<0.99,"decline", "grow")
         ) %>%
  filter(YEAR == 2100) %>%
  dplyr::select(GEOID, 
                # groupa,
                diff) %>%
  right_join(proj2, .) %>%
  # filter(groupa == "decline") %>%
  dplyr::select(GEOID, YEAR, medage_mig, medage_base, diff, #groupa
                )
  # mutate(onea = if_else(groupa=="decline", medage_mig, medage_base),
  #        twoa = if_else(groupa=="decline", medage_base, medage_mig)) %>%
  # dplyr::select(-groupa)
  # pivot_longer(cols = c(medage_mig, medage_base),
  #              names_to = "groupa",
  #              values_to = "medage")
I()

a <- proj_growdec[which(proj_growdec$YEAR == 2100),] %>%
  mutate(diff2 = medage_mig - medage_base,
         diff = diff-1)

# plot(a$diff, a$diff2)

ggplot(a, aes(x= diff, y = diff2)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
labs(x= "Change in Population due to SLR in 2100",
     y="Change in Median Age in 2100")


trace2020 <- 
  as.data.frame(t(unlist(
  t.test(medage ~ groupa, data = proj_growdec[which(proj_growdec$YEAR == 2100),]))))
