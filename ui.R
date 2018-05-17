#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(
  
  tagList(
    navbarPage(
      theme = shinythemes::shinytheme("sandstone"),
      "Movie",
      
#====================================================================      
#                         CORRELATION
#====================================================================      
      
      
      tabPanel("Correlation",
        tabsetPanel(
          tabPanel(
              "Numeric correlation",
            
              fluidRow(
                column(width=4,
               br(),
               h3("Correlation Variable:"),
               code("Num critic for reviews"),br(),code("Duration","Director facebook likes"), 
               br(),
               code("Actor 3 facebook likes"),br(),code("Actor 1 facebook likes") ,br(),code("Gross"),                    
               br(),
               code("Num voted users"), br(),code("Cast total facebook likes"),br(),code("Facenumber in poster"),     
               br(),
               code("Num user for reviews"),br(),code("Budget"),br(),code("Title year"), 
               br(),
               code("Actor 2 facebook likes"),br(),code("Imdb score"),br(),code("Aspect ratio"),             
               br(),
               code("Movie facebook likes") ,br()),
               
             column(width = 6,
                      plotOutput("numcor"),
                    "From the correlation plot, we can tell that: 
                      Face number in poster has negative correlation with all other predictors. 
                      Cast total facebook likes and actor 1 facebook likes has a stronger positive correlation. 
                      Budget and gross have strong correaltion which is not surprising. 
                      Interestingly, IMDB scores has strong positive corrlation with number of critics for review, which means the more the critics review, the higher the score.
                      Duration and number of voted users also have strong positive correlation with IMDB scores."
                    )
              )
             
        ),
        
#====================================================================      
        
        tabPanel("Character correlation",
                 fluidRow(
                   column(width=4,
                          br(),
                          h3("Correlation Variable"),
                          code("Director name"),br(),code("Duration"), 
                          br(),
                          code("Face number in poster"),br(),code("IMDB score") ,br(),code("Genres"),                    
                          br()),
                 column(width = 6,
                        plotOutput("charcor1"),
                        "From the plot, only duration and IMDB score hace a high correlation. 
                        Face number in posters has a negative correlation with IMDB score. 
                        Genre has little correlation with score Interesting, director name has no correlation with IMDB score."
                        )
                 ),
                 br(),
                 fluidRow(
                   column(width=4,
                          br(),
                          h3("Correlation Variable"),
                          code("Color"),br(),code("Actor 1 name"), 
                          br(),
                          code("Title year"),br(),code("IMDB score") ,br(),code("Aspect ratio"),br(),
                          code("gross"),br()),
                   column(width = 6,
                          plotOutput("charcor2"),
                          "Color and title year has highly positive correlation. 
                            Color and aspect ratia,gross has smaller positive correlations. 
                          Actor 1 name has very small positive correlation with gross, meaning who plays the movies does not have impact on the gross. 
                        Title year and aspect ratio and color are highly positively correlated.
                        IMDB score has very small positive correlation with actor 1 name ,which means who was the actor 1 does not make the movie has a higher score. 
                        Interestingly, IMDB score has a negative correlation with title year,which means the old movies seems to have a higher score. 
                          The result agrees with out observation from the scatter plot. 
                          IMDB and aspect ratio has small positive correlation. 
                          IMDB has a strong positive correlation with gross."                   )
                   
                 ),br()
        )
              
      )
      ), 
#====================================================================      
#                           EXPLORE
#====================================================================      
      
      
            
      tabPanel("Explore",
               sidebarPanel(
                 selectInput("graph",
                             label = "Choose a graph",
                             choices = c("Genre","Association"),
                             selected = "Genre"),
                 selectInput("top",
                             label = "Top graph",
                             choices = c("Director","Actor 1"),
                             selected = "Actor 1")
                 ),
              
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graph",  
                            plotOutput("genplot"),br() ),
                   tabPanel("WordCloud",
                            plotOutput("w"), br() ),
                   tabPanel("Top",htmlOutput("topplot"),br() )
                   )),
               
#====================================================================      
   

                sidebarPanel(
                            sliderInput("year", "Year released",
                                        min(movie$title_year), 
                                        max(movie$title_year), 
                                        value = c(1980, 2013)),
                            sliderInput("pro",
                                        "Budget",
                                        min=floor(min(movie$budget)/100)*100,
                                        max=floor(max(movie$budget)/100000000)*100000000,
                                        value = c(min,max),
                                        step = 100000 )),
                mainPanel(
                    tabsetPanel(
                      tabPanel("Year", plotOutput("yearplot"),
                               "There are many outliers for title year. ",br(),
                               "The mojority of data points are around the year of 2000 and later, which make sense that this is less movies in the early years.", br(), 
                               "Also, an interesting notice is that movies from early years tend to have higher scores.",br()
                               ),
                      tabPanel("Profit",  plotOutput("proplot"), br(),
                               "The relationship between budget and gross, 
                               investigating whether more budget can bring out higher gross."),
                      tabPanel("3D plot",
                               plotlyOutput("p_3d"), br() )
                      
                      
                      ))),
       
     
#====================================================================      
#                             SELECT
#====================================================================      

    tabPanel("Select", 
             
             tabsetPanel(
               tabPanel("Top gross",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("ran_g",
                              "Choose range",
                              c(10,100,500))
                 ),
                 mainPanel(
                   
                   tableOutput("gtop")
                   
                 ))),
                 
#====================================================================      
                 
                
                   tabPanel("Top director",
                     sidebarLayout(
                sidebarPanel(
                   selectInput("ran_d",
                               "Choose range",
                               c(10,100,500))
                 ),
                 mainPanel(
                   
                   tableOutput("dtop")
                   
                
                 )
                 )),
#====================================================================      

                  tabPanel("Top actor",
                           sidebarLayout(
                             sidebarPanel(
                               
                  selectInput("ran_a1",
                              "Choose range",
                              c(10,100,500))
                ),
                mainPanel(
                  
                  tableOutput("a1top")
                  
                )
               )
               ) 
             

)),
                 


#====================================================================      
#                         Documentation
#====================================================================      

tabPanel("Documentation", 
         
         h1("Introduction to this Shiny APP"),
         br(),
         "This APP not only help people find a good movie, but also give information to movie producer to find the good team before making the movie.",
         br(),br(),
         
              h3("Part 1: CORRELATION"),
             "For this part, we use the cor() founction for the 16 Numeric variables to find the possible correlation.",br(),
             "And the pairs.panels() founction to see if there are anyother variables (not just numeric) correlated to each other.",br(),
             br(),
             fluidRow(   column(width=6,imageOutput("cor_image")),      
               column(width=6,br(),br(),br(),imageOutput("cor_code"))),
         
             h3("Part 2: EXPLORE"),
             "In this part, we creat some plot for better understand the movie dataset.",br(),
             "Here we have two Panel, each contain of three plot.",br(),
             "The first Panel we have the Genre analysis, as we can see, we have two option: 
             Genre and Association. Genre shows the frequency of the genre in movie, along with the wordcloud, which give a better visualisation. 
             Inside Association we can see the related genre group.",br(),
             br(),
             fluidRow(   column(width=4,imageOutput("gen_image")),      
                         column(width=4,imageOutput("word_image")),
                         column(width=4,imageOutput("assoc_image"))),
             "And then we use the numer of facebook likes for Actor 1 and Director and plot them to see who are the most popular.",br(),br(),
             fluidRow(   column(width=6,imageOutput("act_image")),      
                         column(width=6,imageOutput("direct_image"))),
            "The second Panel we based on the year of the movie to see how it relate to the IMDB score and country. 
             And the other is about Budget and Gross, which we can see the relation of them. And the 3D plot for better visualisation.",br(),
             br(),
             fluidRow(  column(width=4,imageOutput("year_image")),      
                        column(width=4,imageOutput("budget_image")),
                        column(width=4,imageOutput("tD_image"))),
            
             h3("Part 3: SELECT"),
             "This part we put 3 table for the top movie, top director and top actor. We use the packages (dplyr) to select and rearrange the dataset, so that we can see the top 10 or 100 or 500 of the whole data.",br(),
            br(),
            fluidRow(  column(width=6,imageOutput("m_image")),      
                       column(width=6,imageOutput("d_image"))),
            fluidRow(  column(width=6,imageOutput("a_image")),
                       column(width=6,br(),br(),imageOutput("t_code")))
             )


#====================================================================      
#                           END
#====================================================================      

  ))
  )
