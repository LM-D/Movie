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
                          h3("Correlation Variable:"),
                          code("Director name"),br(),code("Duration"), 
                          br(),
                          code("Face number in poster"),br(),code("IMDB score") ,br(),code("Genres"),                    
                          br()),
                 column(width = 6,
                        plotOutput("charcor1"),
                        "From the plot, only duration and IMBD score hace a high correlation. 
                        Face number in posters has a negative correlation with IMBD score. 
                        Genre has little correlation with score Interesting, director name has no correlation with IMDB score."
                        )
                 ),
                 br(),
                 fluidRow(
                   column(width=4,
                          br(),
                          h3("Correlation Variable:"),
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
                             selected = "Genre")
                 ),
              
               mainPanel(
                 tabsetPanel(
                   tabPanel("Graph",  
                            plotOutput("genplot"),br() ),
                   tabPanel("WordCloud",
                            plotOutput("w"), br() )
                   )),
               
#====================================================================      
               
               sidebarPanel(
                 selectInput("top",
                             label = "Top graph",
                             choices = c("Director","Actor 1"),
                             selected = "Actor 1")
               ),

               mainPanel(
                 tabsetPanel(
                   tabPanel("Top actors",htmlOutput("topplot"),br() ))),
                   
#====================================================================      

                sidebarPanel(
                            sliderInput("year", "Year released",
                                        min(movie$title_year), 
                                        max(movie$title_year), 
                                        value = c(1980, 2013))),
                mainPanel(
                    tabsetPanel(
                      tabPanel("Year", plotOutput("yearplot"),
                               "There are many outliers for title year. ",br(),
                               "The mojority of data points are around the year of 2000 and later, which make sense that this is less movies in the early years.", br(), 
                               "Also, an interesting notice is that movies from early years tend to have higher scores.",br()
                               ))),
       
    
#====================================================================      

                  sidebarPanel(
                            sliderInput("IMDB",
                                        "IMDB Score:",
                                        min = 0,
                                        max = 10,
                                        value = c(5,8),
                                        step = 0.1)),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Score", plotOutput("scoplot") ,br()))),
                  
#====================================================================      
# 
#                     sidebarPanel(
#                             sliderInput("pro",
#                                         "Profit",
#                                         min=floor((min(movie$gross)-min(movie$budget))/100)*100,
#                                         max=floor((max(movie$gross)-max(movie$budget))/10000000)*10000000,
#                                         value = c(min,max),
#                                         step = 100000 )),
#                     mainPanel(
#                       tabsetPanel(
#                         tabPanel("Profit",  plotOutput("proplot"), br())))
#             ),

                  
                  sidebarPanel(
                    sliderInput("pro",
                                "Budget",
                                min=floor(min(movie$budget)/100)*100,
                                max=floor(max(movie$budget)/100000000)*100000000,
                                value = c(min,max),
                                step = 100000 )),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Profit",  plotOutput("proplot"), br(),
                               "The relationship between budget and gross, 
                               investigating whether more budget can bring out higher gross.")))
                        ),
     
#====================================================================      
#                             SELECT
#====================================================================      

    tabPanel("Select", 
             
             tabsetPanel(
               tabPanel("Top gross",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("ran_g",
                              "Choose range:",
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
                               "Choose range:",
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
                              "Choose range:",
                              c(10,100,500))
                ),
                mainPanel(
                  
                  tableOutput("a1top")
                  
                )
               )
               ) 
             

))
                 
                 # sidebarPanel(
                 #   
                 #   # Input: Text for providing a caption ----
                 #   # Note: Changes made to the caption in the textInput control
                 #   # are updated in the output area immediately as you type
                 #   selectInput("y",
                 #               "Year:",
                 #               c("All",sort(unique(movie_yds$title_year)))),
                 #   
                 #   # Input: Selector for choosing dataset ----
                 #   selectInput("d",
                 #               "Director:",
                 #               c("All",sort(unique(as.character(movie_yds$director_name))))),
                 #   
                 #   # Input: Numeric entry for number of obs to view ----
                 #   selectInput("s",
                 #               "IMDB Score:",
                 #               c("All",sort(unique(as.character(movie_yds$imdb_score)))))
                 # ),
                 
                 
                 
                 
                 # Main panel for displaying outputs ----
            

             
        
    #      tabPanel("Select", 
    #        fluidPage(
    #           titlePanel("Year-Director-Score"),
    #     
    #           # Create a new Row in the UI for selectInputs
    #           fluidRow(
    #             column(4,
    #                    selectInput("y",
    #                          "Year:",
    #                          c("All",sort(unique(movie_yds$title_year))))
    #              ),
    #              column(4,
    #                    selectInput("d",
    #                          "Director:",
    #                          c("All",
    #                            sort(unique(as.character(movie_yds$director_name)))))
    #                 ),
    #                column(4,
    #                        selectInput("s",
    #                               "IMDB Score:",
    #                          c("All",
    #                            sort(unique(as.character(movie_yds$imdb_score)))))
    #                )
    #                ),
    #     # Create a new row for the table.
    #     fluidRow(
    #       DT::dataTableOutput("table")
    #     )
    #   )
    #   
    # )



#====================================================================      
#                           END
#====================================================================      

  )
  )
  
)
  
  
  
  
  
  
  
  
  
  
  
  
 #======================================================================== 
#   fluidPage(
#   
#   # Application title
#   titlePanel("Movie Analysis"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("graph",
#                   label = "Choose a graph",
#                   choices = c("Genre","Association"),
#                   selected = "Genre"),
#       
#      
#       sliderInput("year", "Year released",
#                   min(movie$title_year), 
#                   max(movie$title_year), 
#                   value = c(1980, 2013)),
# 
#        sliderInput("IMDB",
#                    "IMDB Score:",
#                    min = 0,
#                    max = 10,
#                    value = 8,step = 0.1),
#        dateRangeInput("date",
#                       label = "Choise a date")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       
#       plotOutput("plot1"),
#        verbatimTextOutput("value")
#     )
#   )
# ))
