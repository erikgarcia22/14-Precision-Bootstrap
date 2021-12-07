# EJG trying to get real gradient
# Nov 23 2021


fig1.1 <- ggplot(real_stats, aes(x=name, y=Mean, fill=name))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.5)+
  scale_x_discrete(name= "Sample Size",
                   labels = c("1", "3", "10", 
                              "30", "100", "300"))+
  scale_y_continuous(name= "Bootstrap mean +/- SD")+
  ggtitle("Effects of Sample Size on Bootstrap Mean Estimation", )+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5))+  #hjust centers the title
  scale_fill_brewer(type = "seq",
                    palette = 1,
                    direction = 1,
                    aesthetics = 'fill')

fig1.1    # print object fig1
ggsave(filename = "purplegradientcontinuous_real_stats.png") # save last ggplot figure
