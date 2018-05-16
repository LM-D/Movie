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
   
   output$w <- renderPlot({ 
     set.seed(10)
     pal2 <- brewer.pal(8,"Dark2")
     wordcloud(genre_wf$word,genre_wf$freq,random.order=FALSE,
               rot.per=.15, colors=pal2,scale=c(4,1),
               title="WordCloud: Movie Genres")
   })
   
   output$p_3d <- renderPlotly({ 
     plot_ly(movie, x = ~imdb_score, y = ~budget/1000000, z = ~gross/1000000, 
             color = ~profit_flag, colors = c('#BF382A', '#0C4B8E'),size = I(3)) %>%
       add_markers() %>%
       layout(scene = list(xaxis = list(title = 'IMDB Score'),
                           yaxis = list(title = 'Budget (M$)'),
                           zaxis = list(title = 'Revenue (M$)')),
              title = "INTERACTIVE 3D Scatter plot: IMDB Score vs Revenue vs Budget",
              showlegend = FALSE)
     })
   
  
#=====================================================================   
   output$topplot<-renderGvis({

     if (input$top == "Director"){

       d1 = movie %>% select(director_name, director_facebook_likes) %>%
         group_by(director_name) %>% summarize(appear.count_d=n())

       d2 = left_join(movie, d1, by="director_name")
       d3 = d2 %>% select(director_name, director_facebook_likes, appear.count_d) %>%
         distinct %>% arrange(desc(appear.count_d))


       Bubble_d <- gvisBubbleChart(d3, idvar="director_name",
                                   xvar="appear.count_d", yvar="director_facebook_likes",
                                   sizevar="appear.count_d",
                                   #colorvar="title_year",
                                   options=list(
                                     #hAxis='{minValue:75, maxValue:125}',
                                     width=1000, height=800
                                   ), chartid = "foo"
       )

     }
     else if (input$top=="Actor 1"){
       a11 = movie %>% select(actor_1_name, actor_1_facebook_likes) %>%
         group_by(actor_1_name) %>% summarize(appear.count_a1=n())

       a12 = left_join(movie, a11, by="actor_1_name")
       a13 = a12 %>% select(actor_1_name, actor_1_facebook_likes, appear.count_a1) %>%
         distinct %>% arrange(desc(appear.count_a1))

       Bubble_a1 <- gvisBubbleChart(a13, idvar="actor_1_name",
                                    xvar="appear.count_a1", yvar="actor_1_facebook_likes",
                                   sizevar="appear.count_a1",
                                    #colorvar="title_year",
                                    options=list(
                                      #hAxis='{minValue:75, maxValue:125}',
                                      width=1000, height=800
                                    ), chartid = "foo"
       )


     }
   })
   
#=====================================================================   
  
  output$yearplot<-renderPlot({

    ggplot(movie, aes(x=movie$title_year, y=movie$imdb_score))+
      geom_point(aes(color=movie$language),position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("IMDB Score ~ Year")+
      xlab("Year")+
      ylab("IMDB Score")+
      xlim(input$year[1],input$year[2])
    
  })

#=====================================================================   
  
  output$scoplot<-renderPlot({
    ggplot(movie, aes(x=movie$imdb_score, y=movie$country))+
      geom_point(aes(color = movie$title_year),position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Country ~ IMDB Score")+
      xlab("IMDB Score")+
      ylab("Country")+
      xlim(input$IMDB[1],input$IMDB[2])+
      theme_igray()
  })

#=====================================================================   
  
    
  output$proplot<-renderPlot({
    ggplot(movie, aes(x=movie$budget, y=movie$gross))+
      geom_point(aes(color=movie$profit_flag),position="dodge", stat="identity")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      ggtitle("Gross ~ Budget")+
      xlab("Budget")+
      ylab("Gross")+
      xlim(input$pro[1],input$pro[2])
  })
 
#====================================================================      
#                             SELECT
#====================================================================      
  
   
  # # Filter data based on selections
  # output$view <- renderTable({
  #   data <- movie_yds
  #   if (input$p != "All") {
  #     data <- data[data$title_year == input$y,]
  #   }
  #   if (input$d != "All") {
  #     data <- data[data$director_name == input$d,]
  #   }
  #   if (input$a1 != "All") {
  #     data <- data[data$imdb_score == input$s,]
  #   }
  #   data
  # 
  # })

   
   # Filter data based on selections
   output$gtop <- renderTable({
        data<-dplyr::slice(gt,1:input$ran_g)
        data
     })
     
     output$dtop <- renderTable({
         data<-dplyr::slice(dt,1:input$ran_d)
     })
     
       output$a1top <- renderTable({
         data<-dplyr::slice(a1t,1:input$ran_a1)
       })
       
       
#====================================================================      
#                           END
#====================================================================      
      
})
