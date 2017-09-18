test <- select(DO_data, -Mdate_met, -X9)

test <- gather(test, site, DO, -decday)
test <- subset(test, site != "TT_DO")


ggplot(test, aes(x=site, y=DO)) +
  geom_boxplot()
