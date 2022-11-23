#' ---
#' title: "ECDFs for proportion above certain thresholds for FWI"
#' author: "Jérôme Lavoué"
#' date: "November 2022"
#' output: github_document
#' ---
#' 
#' 

#+ r setup, include=FALSE, cache = FALSE,warning=FALSE, message = FALSE

    require("knitr")
    
    ## setting working directory
    
    opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


#+ libraries, echo = FALSE,warning=FALSE, message = FALSE
    
    #### libraries
    
    library(ggplot2)
    library(ggthemes)
    

#+ data, echo = FALSE,warning=FALSE, message = FALSE
    
    
    ####### data
    
    FWI1 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_1_by_agent_gender.RDS")
    FWI2.5 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_2_5_by_agent_gender.RDS")
    FWI5 <- readRDS( "SubstantialThresholdAnalysis/data/FWI_ge_5_by_agent_gender.RDS")

#+ plot1, echo = FALSE,warning=FALSE, message = FALSE
    
    
    #### plot for greater than 1
    
        p <- ggplot() 
        
        p <- p +  stat_ecdf( data = FWI1[!is.na(FWI1$p_F),]  , geom = "step" , aes(  x = p_F , color = "Women") )
        
        p <- p +  stat_ecdf( data = FWI1[!is.na(FWI1$p_M),] , geom = "step" , aes( x = p_M , color = "Men")  )
        
        p <- p +  geom_vline( xintercept = 0.1, linetype="dashed", color = "black")
        
        p <- p +  geom_vline( xintercept = 0.2, linetype="dashed",color = "black")
        
        p <- p +  geom_vline( xintercept = 0.3, linetype="dashed", color = "black")
        
        
        #### beauty
        
        p <- p + theme_solarized()
        
        
        p <- p +  xlab('Proportion of jobs with FWI>=1') + ylab('Cumulative proportion of agents')+
          theme(axis.title.x=element_text(size=14))+
          theme(axis.title.y=element_text(size=14))+
          theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
          theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))
        
        p <- p + scale_color_manual(name = "Data",
                                    breaks = c("Women", "Men"),
                                    values = c("Women" = "red", "Men" = "blue") )
        
        p <- p  + theme(legend.position = c(0.8, 0.6) , legend.key = element_rect(colour = "transparent"))
        
        p <- p + scale_x_continuous(expand = c(0 , 0.00) , 
                                    limits = c(0,1),
                                    labels = scales::number_format(accuracy = 0.1,
                                                                   decimal.mark = '.'),
                                    breaks = seq( from = 0 , to = 1 , by = 0.1))
        
        p <- p + scale_y_continuous(expand = c(0 , 0.00) , 
                                    limits = c(0,1),
                                    labels = scales::number_format(accuracy = 0.01,
                                                                   decimal.mark = ','),
                                    breaks = seq( from = 0 , to = 1 , by = 0.2))
        
        p
        
#+ plot2, echo = FALSE,warning=FALSE, message = FALSE
        
    #### plot for greater than 2.5

    p <- ggplot() 
    
    p <- p +  stat_ecdf( data = FWI2.5[!is.na(FWI2.5$p_F),]  , geom = "step" , aes(  x = p_F , color = "Women") )
    
    p <- p +  stat_ecdf( data = FWI2.5[!is.na(FWI2.5$p_M),] , geom = "step" , aes( x = p_M , color = "Men")  )
    
    p <- p +  geom_vline( xintercept = 0.1, linetype="dashed", color = "black")
    
    p <- p +  geom_vline( xintercept = 0.2, linetype="dashed",color = "black")
    
    p <- p +  geom_vline( xintercept = 0.3, linetype="dashed", color = "black")
    
    
    #### beauty
    
    p <- p + theme_solarized()
    
    
    p <- p +  xlab('Proportion of jobs with FWI>=2.5') + ylab('Cumulative proportion of agents')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))
    
    p <- p + scale_color_manual(name = "Data",
                                breaks = c("Women", "Men"),
                                values = c("Women" = "red", "Men" = "blue") )
    
    p <- p  + theme(legend.position = c(0.8, 0.6) , legend.key = element_rect(colour = "transparent"))
    
    p <- p + scale_x_continuous(expand = c(0 , 0.00) , 
                                limits = c(0,1),
                                labels = scales::number_format(accuracy = 0.1,
                                                               decimal.mark = '.'),
                                breaks = seq( from = 0 , to = 1 , by = 0.1))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.00) , 
                                limits = c(0,1),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ','),
                                breaks = seq( from = 0 , to = 1 , by = 0.2))
    
    p
    

#+ plot3, echo = FALSE,warning=FALSE, message = FALSE
    
        
  #### plot for greater than 5
    
    p <- ggplot() 
    
    p <- p +  stat_ecdf( data = FWI5[!is.na(FWI5$p_F),]  , geom = "step" , aes(  x = p_F , color = "Women") )
    
    p <- p +  stat_ecdf( data = FWI5[!is.na(FWI5$p_M),] , geom = "step" , aes( x = p_M , color = "Men")  )
    
    p <- p +  geom_vline( xintercept = 0.1, linetype="dashed", color = "black")
    
    p <- p +  geom_vline( xintercept = 0.2, linetype="dashed",color = "black")
    
    p <- p +  geom_vline( xintercept = 0.3, linetype="dashed", color = "black")
    
    
    #### beauty
    
    p <- p + theme_solarized()
    
    
    p <- p +  xlab('Proportion of jobs with FWI>=5') + ylab('Cumulative proportion of agents')+
      theme(axis.title.x=element_text(size=14))+
      theme(axis.title.y=element_text(size=14))+
      theme(axis.text.x=element_text(size=12 , hjust = 0.5))+
      theme(axis.text.y=element_text(size=12 ,  hjust = 0.5))
    
    p <- p + scale_color_manual(name = "Data",
                                breaks = c("Women", "Men"),
                                values = c("Women" = "red", "Men" = "blue") )
    
    p <- p  + theme(legend.position = c(0.8, 0.6) , legend.key = element_rect(colour = "transparent"))
    
    p <- p + scale_x_continuous(expand = c(0 , 0.00) , 
                                limits = c(0,1),
                                labels = scales::number_format(accuracy = 0.1,
                                                               decimal.mark = '.'),
                                breaks = seq( from = 0 , to = 1 , by = 0.1))
    
    p <- p + scale_y_continuous(expand = c(0 , 0.00) , 
                                limits = c(0,1),
                                labels = scales::number_format(accuracy = 0.01,
                                                               decimal.mark = ','),
                                breaks = seq( from = 0 , to = 1 , by = 0.2))
    
    p    