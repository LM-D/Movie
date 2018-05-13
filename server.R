#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
  
#====================================================================      
#                         CORRELATION
#====================================================================      
  
  
   output$numcor<-renderPlot({
    corrplot(cor(as.matrix(movie_num)),method="number",is.corr=T,addCoefasPercent=T)
      })
   
   output$charcor1<-renderPlot({
     pairs.panels(movie[c('director_name','duration','facenumber_in_poster','imdb_score','genres')])
   })
   
   output$charcor2<-renderPlot({
     pairs.panels(movie[c('color','actor_1_name','title_year','imdb_score','aspect_ratio','gross')])
   })

#====================================================================      
#                           EXPLORE
#====================================================================      
   
      
     
  output$genplot<-renderPlot({
    
    if (input$graph=="Genre"){

    # generate bins based on input$bins from ui.R
    ggplot(genre_wf, aes(x=reorder(word,-freq), y=freq))+
      geom_point(color="red",position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Movie Genre Frequency Graph")+
      xlab("Genre")+
      ylab("Frequency")

  }
  else if (input$graph=="Association"){
    ggplot(assoc,aes(x=words,y=score))+
      geom_bar(stat="identity")+
      facet_grid(~key,scales = 'free',space="free_x")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ylab("Association Score")+
      xlab("Genre")+
      ggtitle("Genre Associations")
     # +scale_color_brewer(type='qual')

  }
})
  
   # GvisInput <- reactive({
   #   Bubble <- gvisBubbleChart(m3, idvar="actor_1_name", 
   #                             xvar="appear.count", yvar="actor_1_facebook_likes",
   #                             sizevar="appear.count",
   #                             #colorvar="title_year",
   #                             options=list(
   #                               #hAxis='{minValue:75, maxValue:125}',
   #                               width=1000, height=800
   #                             ),chartid = "foo")
   #                             
   #})
     
  output$topplot<-renderGvis({
  Bubble
  })
   

  output$yearplot<-renderPlot({

    ggplot(movie, aes(x=movie$title_year, y=movie$imdb_score))+
      geom_point(aes(color=movie$language),position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Movie Genre Frequency Graph")+
      xlab("Budget")+
      ylab("Country")+
      xlim(input$year[1],input$year[2])
    
  })

  
  
  output$scoplot<-renderPlot({
    ggplot(movie, aes(x=movie$imdb_score, y=movie$country))+
      geom_point(aes(color = movie$title_year),position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Movie Genre Frequency Graph")+
      xlab("Budget")+
      ylab("Country")+
      xlim(input$IMDB[1],input$IMDB[2])+
      theme_igray()
  })
  
  output$proplot<-renderPlot({
    ggplot(movie, aes(x=movie$imdb_score))+
      geom_histogram(aes(fill=..count..),binwidth = 0.5)+
      scale_x_continuous(name = "IMDB Score",breaks = seq(input$IMDB[1],input$IMDB[2]),limits =c(input$IMDB[1],input$IMDB[2]))+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Movie Sore")+
      scale_fill_gradient("Count", low = "blue", high = "red")
  })
  
  # output$proplot<-renderPlot({
  #   ggplot(movie, aes(x=(movie$gross-movie$budget), y=movie$budget,color = (movie$profit_flag)+1))+
  #     geom_point(position="dodge", stat="identity")+
  #     theme(axis.text.x=element_text(angle=45, hjust=1))+
  #     ggtitle("Movie Profit")+
  #     xlab("Profit")+
  #     ylab("Budget")+
  #     xlim(input$pro[1],input$pro[2])
  # })
  
  
  output$value <- renderPrint({ input$date })
 
  
#====================================================================      
#                             SELECT
#====================================================================      
  
   
  # Filter data based on selections
  output$view <- renderTable({
    data <- movie_yds
    if (input$y != "All") {
      data <- data[data$title_year == input$y,]
    }
    if (input$d != "All") {
      data <- data[data$director_name == input$d,]
    }
    if (input$s != "All") {
      data <- data[data$imdb_score == input$s,]
    }
    data
  })
  
  
  
  
})
