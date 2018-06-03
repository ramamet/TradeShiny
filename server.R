##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Copyright 2018, Ramanathan Perumal, All rights reserved.
## ramamet4@gmail.com
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load the required packages
library(shiny)
require(shinydashboard)
library(shinyjs)
library(ggplot2)
library(dplyr)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions

library(ggthemes)
library(editheme)
library(RColorBrewer)
suppressMessages(library(DT))
#library(readr)

pal <- get_pal(theme = "Dracula")

##----------------
#dd <- read.csv('tradeSub.csv',stringsAsFactors = F,header=T)
#df <- read_csv("tradeSub.gz")
#dd <- data.frame(df)
#colnames(dd)[1] <- "country_or_area"
#dd$trade_usd <-  as.numeric(as.character(dd$trade_usd))  

dd <- readRDS("tradeSub.rds")

# create the server functions for the dashboard  
server <- function(input, output) { 
  
    Year <- reactive({
    yr <-input$variable
    as.numeric(yr)
    })

    df<-reactive({
     dd %>% filter(year == Year())
     })
    
 
     
  ##-------------------------------------------------------------
   ### export revenue by country
   
   expp <-reactive({      
    dd%>%  filter(year== Year()) %>%
           filter(flow=="Export") %>%
           group_by(country_or_area) %>% 
           summarise(cost = sum(trade_usd)) #%>%
          # arrange(desc(cost))
                  
   
   })
   
   
   exp.top <- reactive({
   Exp <- expp()       
   Exp.top <- arrange(Exp,desc(cost))              
   Exp.top %>% mutate(norm.cost=cost/(10^9)) %>%
               top_n(10, norm.cost)

   })               
   
   ###--------------------------------------------------------------  
   
   ##-------------------------------------------------------------
   ### import revenue by country
   
   impp <- reactive({
    dd%>%  filter(year == Year()) %>%
           filter(flow=="Import") %>%
           group_by(country_or_area) %>% 
           summarise(cost = sum(trade_usd)) #%>%
           #arrange(desc(cost))
        })          
                  
   
   
   imp.top <- reactive({
   Imp <- impp()
   #Imp.top <- Imp[1:10,]   
   Imp.top <- arrange(Imp,desc(cost))
   #Imp.top <- Imp.top[1:10,]  
                 
   Imp.top %>% mutate(norm.cost=cost/(10^9)) %>%
               top_n(10, norm.cost)
   
   })
               
   ###--------------------------------------------------------------
   ## total countries participating trade   
   memb <- reactive({ 
   Expp.N <-expp()
   nrow(Expp.N)
   })
     
   ## category traded   
   cate <- reactive({
   Df <- df()       
   ct <- unique(Df$category)
   length(ct)
   })
   
   ## total items be traded,
   comm <- reactive({
   Df <- df()       
   cm <- unique(Df$comm_code)
   length(cm)
   })
   
   ##########
   ## selected country ; import
   
    Place <- reactive({
    place <-input$var2
    place
    #as.numeric(yr)
    })
   
    cnt1.Imp <- reactive({
             dd %>% 
             filter(year==Year() & 
             country_or_area== Place() & 
             flow=="Import")
             })
             
     cnt1.ImpSub <- reactive({ 
            Cnt1 <- cnt1.Imp()
            Cnt1 %>%
            filter(category!= "all_commodities") %>%
            group_by(category)  %>%
            summarise(cate_cost=sum(trade_usd)) %>%
            arrange(desc(cate_cost))
            })
             
            
     cnt1.Imp.top <- reactive({
     Cnt1Sub < cnt1.ImpSub()
     Cnt1Sub[1:10,]  
     })
     
   Ind.imp.top <- reactive({
   impSel <- cnt1.ImpSub()       
   impSel.top <- impSel[1:10,]                 
   impSel.top %>% 
    mutate(cate_cost=cate_cost/(10^9)) %>%
    mutate(costPerc=100*(cate_cost/sum(cate_cost)))
    
   })  
   
   
   ##
   
    ## selected country ; export
   
    cnt1.Exp <- reactive({
             dd %>% 
             filter(year==Year() & 
             country_or_area== Place() & 
             flow=="Export")
             })
             
     cnt1.ExpSub <- reactive({ 
            Cnt2 <- cnt1.Exp()
            Cnt2 %>%
            filter(category!= "all_commodities") %>%
            group_by(category)  %>%
            summarise(cate_cost=sum(trade_usd)) %>%
            arrange(desc(cate_cost))
            })
             
            
     cnt1.Exp.top <- reactive({
     Cnt2Sub < cnt1.ExpSub()
     Cnt2Sub[1:10,]  
     })
     
   Ind.Exp.top <- reactive({
   expSel <- cnt1.ExpSub()       
   expSel.top <- expSel[1:10,]                 
   expSel.top %>% 
    mutate(cate_cost=cate_cost/(10^9)) %>%
    mutate(costPerc=100*(cate_cost/sum(cate_cost)))
    
   })  
     

   ##-----------------------    
   ## category top countries
   
         Cat1 <- reactive({
         c1 <-input$var3
         c1
         })
   
       ### Export
      grp.Exp <- reactive({
	       dd %>%  filter(year== Year()) %>%
			filter(flow=="Export") %>%
			filter(category==Cat1()) %>%
			group_by(country_or_area) %>% 
			summarise(cost = sum(trade_usd)) %>%
			mutate(cate_cost=cost/(10^9)) %>%
			top_n(10, cate_cost)
                })
                
                
       ### Import         
       grp.Imp <- reactive({
	       dd %>%  filter(year== Year()) %>%
			filter(flow=="Import") %>%
			filter(category==Cat1()) %>%
			group_by(country_or_area) %>% 
			summarise(cost = sum(trade_usd)) %>%
			mutate(cate_cost=cost/(10^9)) %>%
			top_n(10, cate_cost)
                })
                
    ###########################
    ## year wise total trade
                        
      yrWise.Exp <- reactive({
              dd %>%        
              filter(country_or_area== Place()) %>%
              group_by(year,flow) %>%  
              summarise(cost = sum(trade_usd)) %>%
              mutate(cate_cost=cost/(10^9))   %>%
              filter(flow=="Export")     
              })   
              
      yrWise.Imp <- reactive({
              dd %>%        
              filter(country_or_area== Place()) %>%
              group_by(year,flow) %>%  
              summarise(cost = sum(trade_usd)) %>%
              mutate(cate_cost=cost/(10^9)) %>%
              filter(flow=="Import")             
              })              
   
   ############################
   ## export to import ratio 
   
         tech1 <- reactive({
         
            dd %>%  filter(year== Year()) %>%
			group_by(country_or_area,flow) %>% 
			summarise(cost = sum(trade_usd)) %>%
			mutate(cate_cost=cost/(10^9)) %>%
			arrange(desc(cate_cost))
		})	
		
		
	tech2 <- reactive({
	
	 data <- tech1()
		
         df1 <- data %>% filter(flow=="Export") %>% dplyr::select(-(cost))
         df2 <- data %>% filter(flow=="Import") %>% dplyr::select(-(cost))
         
         colnames(df1) <- c("area","expFlow","expCost")
         colnames(df2) <- c("area","impFlow","impCost")
         
         Df <- merge(df1,df2,id="area")
          
         Df %>%
               top_n(100, expCost) %>%
               mutate(rat=expCost/impCost)
     
       })
       
   #############################
   ## data table
     df.sub <-reactive({
     dd %>% filter(year == Year()) %>%
            filter(country_or_area== Place())
     })
    
    
     ## data.table
     output$tbl<- DT::renderDataTable({ 
     Df.sub <- df.sub()
    # datatable(Df.sub, style = 'bootstrap')
     datatable(Df.sub)%>%formatStyle(1, color = "#f7f7f7", backgroundColor = "#282a36", target = "row")
     #DT:::DT2BSClass(c('compact', 'cell-border'))
     })
    
   
   ################
               
  #creeating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(memb(), format="d", big.mark=',')
      ,paste('Trade member countries')
      ,icon = icon("globe",lib='glyphicon')
      ,color = "purple")
    
    
  })
    
  
  output$value2 <- renderValueBox({
      valueBox(
      formatC(cate(), format="d", big.mark=',')
      ,paste('Category')
      ,icon = icon("grain",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
      valueBox(
      formatC(comm(), format="d", big.mark=',')
      ,paste('Commodity codes')
      ,icon = icon("shopping-cart",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  ## graph1
  output$revenuebyPrd <- renderPlot({
      Exp.top <- exp.top()
     colourCount = length(unique(Exp.top$country_or_area))
     #getPalette = colorRampPalette(brewer.pal(9, "blues"))
      
        ggplot(data = Exp.top,
       aes(x = reorder(country_or_area, norm.cost),
        y= norm.cost,
        fill= country_or_area)) +       
        #fill= getPalette(colourCount))) + 
      geom_bar(stat = "identity", 
       width=.6,
       color = "#3b444b")  +
       scale_color_gradientn(colours = rainbow(5))+
        coord_flip()+
         theme_editor("Dracula")+
         #   theme_solarized_2(light = FALSE) +
         #scale_colour_solarized("blue")+
        # theme_bw()+
        xlab("country")+
        ylab("Export (Billion USD)")+
        scale_y_continuous(expand = c(0, 0))+
        #scale_y_log10(expand = c(0, 0),breaks = trans_breaks("log10", function(x) 10^x),
         #     labels = trans_format("log10", math_format(10^.x))) +
        theme(plot.title = element_text(size=15, face="bold"),
        text = element_text(size=12,face="bold"),
        axis.title.y=element_blank(),
        legend.position="none") 
       # ggtitle("Revenue by Export")+
       # scale_fill_brewer(palette="Dark2")
    
        
  })
  
  ## graph2
  output$revenuebyRegion <- renderPlot({
     
     Imp.top <- imp.top()
     colourCount = length(unique(Imp.top$country_or_area))
     getPalette = colorRampPalette(brewer.pal(9, "Blues"))
     
      ggplot(data = Imp.top,
       aes(x = reorder(country_or_area, norm.cost),
        y= norm.cost,fill= getPalette(colourCount))) + 
        geom_bar(stat = "identity",
          width=.6,
        color = "#3b444b")  +
        coord_flip()+
         #theme_bw()+
          theme_editor("Dracula")+
         #theme_solarized_2(light = FALSE) +
         #scale_colour_solarized("blue")+
        xlab("country")+
        ylab("Import (Billion USD)")+
        scale_y_continuous(expand = c(0, 0))+
        #scale_y_log10(expand = c(0, 0),breaks = trans_breaks("log10", function(x) 10^x),
         #     labels = trans_format("log10", math_format(10^.x))) +
        theme(plot.title = element_text(size=15, face="bold"),
         text = element_text(size=12,face="bold"),
         axis.title.y=element_blank(),
        legend.position="none") 
        #ggtitle("Cost of Import")+
       
        
 
   })
   
   
      ## third row: graph3
     output$countrySpec1 <- renderPlot({
      Cnt1.Exp.Top <- Ind.Exp.top()
      
       colourCount = length(unique(Cnt1.Exp.Top$category))
       getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      
      ggplot(data = Cnt1.Exp.Top,
       aes(x = reorder(category, cate_cost),
           y = cate_cost,
           fill=getPalette(colourCount)
           )) + 
       geom_segment( aes(x=reorder(category, cate_cost), xend=reorder(category, cate_cost),
        y=0, yend=cate_cost, color="red4", linetype="dotted" )) +
      geom_point(pch=21,size=4,stroke=1,color="#3b444b")  +
       scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
        coord_flip()+
        #theme_bw()+
         theme_editor("Dracula")+
          #     theme_solarized_2(light = FALSE) +
        # scale_colour_solarized("blue")+
        xlab("Category")+
        ylab("Export (Billion USD)")+
       # scale_y_continuous(expand = c(0, 0))+
        theme(plot.title = element_text(size=10, face="bold"),
         text = element_text(size=9#,face="bold"
         ),
        axis.title.y=element_blank(),
        legend.position="none") + 
         ggtitle(paste0(Place()," ",sep=""))
         
        
  })
   
   
   
 ## third row: graph4
     output$countrySpec2 <- renderPlot({
      Cnt1.Imp.Top <- Ind.imp.top()
      
       colourCount = length(unique(Cnt1.Imp.Top$category))
       getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      
      ggplot(data = Cnt1.Imp.Top,
       aes(x = reorder(category, cate_cost),
           y = cate_cost,
          fill=getPalette(colourCount)
           )) + 
       geom_segment( aes(x=reorder(category, cate_cost), xend=reorder(category, cate_cost),
        y=0, yend=cate_cost, color="darkgreen", linetype="dotted" )) +     
      geom_point(pch=21,size=4,stroke=1,color="#3b444b")  +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
        coord_flip()+
         theme_editor("Dracula")+
       # theme_bw()+
            #   theme_solarized_2(light = FALSE) +
         #scale_colour_solarized("blue")+
        xlab("Category")+
        ylab("Import (Billion USD)")+
       # scale_y_continuous(expand = c(0, 0))+
        theme(plot.title = element_text(size=10, face="bold"),
              text = element_text(size=9 #face="bold"
              ),
        axis.title.y=element_blank(),
        legend.position="none") + 
        ggtitle(paste0(Place()," ",sep=""))
         
        
  })
  
  ### Row4: Graph5

     output$catSpec1 <- renderPlot({
     
      Grp.Exp <- grp.Exp()
      
       colourCount = length(unique(Grp.Exp$country_or_area))
       getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      
      ggplot(data = Grp.Exp,
       aes(x = reorder(country_or_area, cate_cost),
           y = cate_cost,
          fill=getPalette(colourCount)
           )) + 
       geom_segment( aes(x=reorder(country_or_area, cate_cost), xend=reorder(country_or_area, cate_cost),
        y=0, yend=cate_cost, color="darkgreen", linetype="dotted" )) +     
      geom_point(pch=21,size=4,stroke=1,color="#3b444b")  +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
        coord_flip()+
         theme_editor("Dracula")+
        xlab("country")+
        ylab("Export (Billion USD)")+
       # scale_y_continuous(expand = c(0, 0))+
        theme(plot.title = element_text(size=10, face="bold"),
              text = element_text(size=9,face="bold"
              ),
        axis.title.y=element_blank(),
        legend.position="none") + 
        ggtitle(paste0(Cat1()," ",sep=""))
         
        
  })
  
  
  ## Row4: Graph6
 
     output$catSpec2 <- renderPlot({
     
      Grp.Imp <- grp.Imp()
      
       colourCount = length(unique(Grp.Imp$country_or_area))
       getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      
      ggplot(data = Grp.Imp,
       aes(x = reorder(country_or_area, cate_cost),
           y = cate_cost,
          fill=getPalette(colourCount)
           )) + 
       geom_segment( aes(x=reorder(country_or_area, cate_cost), xend=reorder(country_or_area, cate_cost),
        y=0, yend=cate_cost, color="darkgreen", linetype="dotted" )) +     
      geom_point(pch=21,size=4,stroke=1,color="#3b444b")  +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
        coord_flip()+
         theme_editor("Dracula")+
        xlab("country")+
        ylab("Import (Billion USD)")+
       # scale_y_continuous(expand = c(0, 0))+
        theme(plot.title = element_text(size=10, face="bold"),
              text = element_text(size=9,face="bold"
              ),
        axis.title.y=element_blank(),
        legend.position="none") + 
        ggtitle(paste0(Cat1()," ",sep=""))
         
        
  })
  
  ## Row5: Graph7
  
    output$yrSpec1 <- renderPlot({
     
     YrWise <- yrWise.Exp()
     ggplot(YrWise,aes(x=year,y=cate_cost))+
     geom_point(size=4,stroke=1.5,color="#ce2b37",pch=21)+
     geom_line(color="#ce2b37",alpha=0.5,size=1)+
     #scale_color_manual(values=c("#ce2b37", "#009246"))+
     #scale_shape_manual(values=c(21, 22))+
     theme_editor("Dracula")+
      ylab("Export (Billion USD)")+
      scale_x_continuous(breaks = c(2014, 2015, 2016))+
         ggtitle(paste0(Place()," ",sep=""))
      
      })
      
      
     output$yrSpec2 <- renderPlot({
     
     YrWise <- yrWise.Imp()
     ggplot(YrWise,aes(x=year,y=cate_cost))+
     geom_point(size=4,stroke=1.5,color="#009246",pch=22)+
     #scale_color_manual(values=c("#ce2b37", "#009246"))+
     #scale_shape_manual(values=c(21, 22))+
      geom_line(color="#009246",alpha=0.5,size=1)+
     theme_editor("Dracula")+
      ylab("Import (Billion USD)")+
       scale_x_continuous(breaks = c(2014, 2015, 2016))+
          ggtitle(paste0(Place()," ",sep=""))
      
      })
      
      
      ## graph8
      
             output$compPlot <- renderPlot({
             		 
             Df <- tech2()	 
            
      
	   # Set a number of 'empty bar'
		empty_bar=5
		# Add lines to the initial dataset
		to_add = matrix(NA, empty_bar, ncol(Df))
		colnames(to_add) = colnames(Df)
		Df=rbind(Df, to_add)
		Df$id=seq(1, nrow(Df))

				# ----- This section prepare a dataframe for labels ---- #
		# Get the name and the y position of each label
		label_data=Df
		 
		# calculate the ANGLE of the labels
		number_of_bar=nrow(label_data)
		angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
		
		########++++
			
		# calculate the alignment of labels: right or left
		# If I am on the left part of the plot, my labels have currently an angle < -90
		label_data$hjust<-ifelse( angle < -90, 1, 0)
		 
		# flip angle BY to make them readable
		label_data$angle<-ifelse(angle < -90, angle+180, angle)
	
		########### ggplot2
		
		 cols <- (brewer.pal(40, 'Set1'))
		 cl1 <- "#f7f7f7"
		 a1 <- 0.3
				
		# Start the plot
		ggplot(Df, aes(x=as.factor(id), y=rat, fill=rat)) +   
		  theme_editor("Dracula")+
		  geom_bar(stat="identity") +
		  scale_fill_gradientn(colours = cols)+
		   geom_hline(yintercept=0,color=cl1,linetype="dashed",alpha=a1)+
		   geom_hline(yintercept=0.5,color=cl1,linetype="dashed",alpha=a1)+
		   geom_hline(yintercept=1,color=cl1,linetype="dashed",alpha=a1)+
		   geom_hline(yintercept=1.5,color=cl1,linetype="dashed",alpha=a1)+
		   geom_text(x=-1.5, y=1.05, label="1.0",color=cl1,size=4,alpha=0.5)+
		    geom_text(x=-1.5, y=0.51, label="0.5",color=cl1,size=4,alpha=0.5)+
		     geom_text(x=-1.5, y=1.52, label="1.5",color=cl1,size=4,alpha=0.5)+
		     ylim(-1.25,2.25) +
		  
		  # Custom the theme: no axis title and no cartesian grid
		  #theme_minimal() +
		  theme(
		    axis.text = element_blank(),
		    axis.title = element_blank(),
		    panel.grid = element_blank(),
		    plot.margin = unit(rep(-1,6), "cm"),      # Adjust the margin to make in sort labels are not truncated!
		   legend.position="none") +
		  
		  # This makes the coordinate polar instead of cartesian.
		  coord_polar(start = 0) +
                   # Add the labels, using the label_data dataframe that we have created before
		  geom_text(data=label_data, aes(x=id, y=rat+0.05, label=area, hjust=hjust), color="#f9f9f9", 
		  #fontface="bold",
		  alpha=0.8, size=3, angle= label_data$angle, inherit.aes = FALSE ) 
		  
		  
		  })
  
  
 
}
