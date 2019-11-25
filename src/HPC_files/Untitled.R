
load("data/HPCfiles/stability.trend.m.rda")
colNamesds <-colnames(stability.trend.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "trend"))
stability.trend.m <- stability.trend.m[, names(stability.trend.m) %in% keepm]
stability.trend.m.long <- gather(stability.trend.m, class, probability, "ETS.notrendnoseasonal":"theta", factor_key = TRUE)
stability.trend.m.long$class <- factor(stability.trend.m.long$class,levels = c("theta"))
m22d <- stability.trend.m.long %>%
  ggplot(aes(y = stability, x = trend, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.2,100),
                       limits=c(0,0.2), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))
m22d

load("data/HPCfiles/stability.N.m.rda")
colNamesds <-colnames(stability.N.m)[32:48]
keep.modelnames <- c("ETS.notrendnoseasonal","nn", "rw","snaive","stlar","tbats","theta")
keepm <- c(keep.modelnames, c("stability", "N"))
stability.N.m <- stability.trend.m[, names(stability.N.m) %in% keepm]
stability.N.m.long <- gather(stability.N.m, class, probability, "theta", factor_key = TRUE)

stability.N.m.long$class <- factor(stability.N.m.long$class,levels = c( "theta"))
m22d <- stability.N.m.long %>%
  ggplot(aes(y = stability, x = N, fill = probability)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~class, ncol=8) +
  scale_fill_viridis_c(breaks=c(0,0.1,100),
                       limits=c(0,0.1), option = "A", direction = -1)+
  theme(strip.text.x = element_text(size = 10))
m22d
