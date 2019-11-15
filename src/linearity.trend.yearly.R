load("data/yearly/trend.linearityrmout.y.rda")
colNamesls <- colnames(lmres_acf1.linearityrmout.y)[27:36]

keep.modelnames <- c("ARIMA", "ARMA.AR.MA", "ETS.dampedtrend", "ETS.notrendnoseasonal",
                     "ETS.trend", "nn", "rw", "rwd", "theta", "wn")
keepy <- c(keep.modelnames, c("lmres_acf1", "linearity"))
lmres_acf1.linearityrmout.y <- lmres_acf1.linearityrmout.y[, names(lmres_acf1.linearityrmout.y) %in% keepy]
lmres_acf1.linearityrmout.y.long <- gather(lmres_acf1.linearityrmout.y, class, probability, "ARIMA":"wn", factor_key = TRUE)
lmres_acf1.linearityrmout.y.long <- lmres_acf1.linearityrmout.y.long %>%
  mutate(class = recode(class, nn="nn",
                        theta = "theta",
                        wn = "wn",
                        "ARMA.AR.MA" = "ARMA",
                        ARIMA = "ARIMA",
                        "ETS.notrendnoseasonal" = "ETS_NTNS",
                        "ETS.dampedtrend" = "ETS_DT",
                        "ETS.trend" = "ETS_T",
                        "rwd" = "rwd",
                        "rw" = "rw" ))
lmres_acf1.linearityrmout.y.long$class <- factor(lmres_acf1.linearityrmout.y.long$class,
                                              levels = c("rw", "rwd", "ETS_T", "ETS_DT", "ETS_NTNS",
                                                         "ARIMA", "ARMA", "wn", "theta", "nn" ))

lmres_acf1.linearityrmout.y.long %>%
  ggplot(aes(x = lmres_acf1, y = linearity, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=5) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks=c(0,0.45,100),
                       limits=c(0,0.45))+
  theme(strip.text.x = element_text(size = 10))+ylim(-10,10)



