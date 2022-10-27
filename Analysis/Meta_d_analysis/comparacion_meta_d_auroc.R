summary(lm(d1$mc ~ meta_d$V1))

library(ggplot2)

plot(meta_d$V1, d1$mc)

df <- data.frame(meta_d = meta_d$V1,
                 auroc = d1$mc)

ggplot(df, aes(x= meta_d, y=auroc))+
  geom_point() +
  geom_smooth(method = "lm")
