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




       Bubble<- gvisBubbleChart(d3, idvar="director_name",
                                   xvar="appear.count_d", yvar="director_facebook_likes",
                                   sizevar="appear.count_d",
                                   #colorvar="title_year",
                                   options=list(
                                     #hAxis='{minValue:75, maxValue:125}',
                                     width=780, height=520
                                   ), chartid = "foo"
       )

     }
     else if (input$top=="Actor 1"){
 
       Bubble<- gvisBubbleChart(a13, idvar="actor_1_name",
                                    xvar="appear.count_a1", yvar="actor_1_facebook_likes",
                                   sizevar="appear.count_a1",
                                    #colorvar="title_year",
                                    options=list(
                                      #hAxis='{minValue:75, maxValue:125}',
                                      width=780, height=520
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
#                         Documentation
#====================================================================      
       
       output$cor_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('cor', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200, 
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$cor_code <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('cor_code', '.jpg', sep='')))
         list(src = filename,width = 380, height = 100, 
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
#====================================================================      
       output$gen_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('gen', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)

       output$word_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('word', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)

       output$assoc_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('assoc', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
#====================================================================      

        output$act_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('act', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)

       output$direct_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('direct', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
#====================================================================    

       output$year_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('year', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$budget_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('budget', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$tD_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('3D', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)

       
#====================================================================    
       
       output$m_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('tm', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$d_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('td', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$a_image <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('ta', '.jpg', sep='')))
         list(src = filename,width = 380, height = 200,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       output$t_code <- renderImage({
         filename <- normalizePath(file.path('./www',
                                             paste('top_code', '.jpg', sep='')))
         list(src = filename,width = 380, height = 100,
              alt = "Image correlation")
       }, deleteFile = FALSE)
       
       
#====================================================================      
#                           END
#====================================================================      
      
})
