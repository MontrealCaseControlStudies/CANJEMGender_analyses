##################################################
#
#  ECDFs for proportion above threshold for FWI
#
##################################################



#### libraries

library(ggplot2)
library(ggthemes)



####### data

FWI1 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_1_by_agent_gender.RDS")
FWI2.5 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_2_5_by_agent_gender.RDS")
FWI5 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_5_by_agent_gender.RDS")



#### tech

p <- ggplot() 

p <- p +  stat_ecdf( data = FWI1[!is.na(FWI1$p_F),]  , geom = "step" , aes(  x = p_F , color = "Women") )

p <- p +  stat_ecdf( data = FWI1[!is.na(FWI1$p_M),] , geom = "step" , aes( x = p_M , color = "Men")  )

p <- p +  geom_hline( yintercept = 0.1, linetype="dashed", color = "black")

p <- p +  geom_hline( yintercept = 0.2, linetype="dashed",color = "black")

p <- p +  geom_hline( yintercept = 0.3, linetype="dashed", color = "black")


#### beauty

p <- p + theme_solarized()


p <- p +  xlab('Proportion of jobs with FWI>=1') + ylab('Cumulative probability')+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
  theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))

p <- p + scale_color_manual(name = "Data",
                            breaks = c("Women", "Men"),
                            values = c("Women" = "red", "Men" = "blue") )

p <- p  + theme(legend.position = c(0.8, 0.6) , legend.key = element_rect(colour = "transparent"))



p