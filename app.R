##------Required libraries for the visualization task--------------


######Please Note: You would need to update all your R packages as that will affect the working of the code

#For being able to install the wordcloud2 package from github as mentioned below we need devtools library
require(devtools)

#Do not use the normal wordcloud2 library as it might not be updated with the latest patch
#Have picked this from the actual makers and installed it from github
##devtools::install_github("lchiffon/wordcloud2")
#Without using this updated patch if there are two worclouds on the same page the hovering does not work properly


# for creating the shiny application
require(shiny)
# for creeating the coral visualization
require(ggplot2)
# for creating the map visualization
require(leaflet)
# for using the shinydashboard template
require(shinydashboard)

# Sparse Lightweight Arrays and Matrices used in case of creating matrix for the wordcloud
require(slam)

# To make interactive graphs
require(plotly)

#require(lubridate)

#For filtering data from date range
require(tidyverse)

#For network plot
require(data.table)
require(sqldf)
require(networkD3)


#for data manipulation
require(plyr)
require(dplyr)
require(reshape2)
require(stringr)
require(tidyr)
require(scales)
require(viridis)
require(leaflet)

# To read shape files
require(rgdal)

#For creating worclouds/text-mining/text-stemming
require(tm)
require(textstem)
require(wordcloud2)
require(RColorBrewer)

########Note: Please update the packages with the below command##########
#update.packages()


# References used
# 1. https://community.rstudio.com/t/align-four-elements-on-shiny-dashboard/7527
# 2. https://rstudio.github.io/shinydashboard/structure.html#mixed-row-and-column-layout
# 3. https://rstudio.github.io/shinydashboard/appearance.html


##-------------------------------------------------------------------------

## Function for reading the data into R

## Read data into a dataframe

#Reading the data required for visualization-------------------------



imdb_movies <- read.csv("imdb movies.csv")

ratings_overall <- read.csv("imdb_ratings_overall.csv")

movietrend_without <- read.csv("overall_movies.csv")

crew_percentage <- read.csv("crew_percentage.csv")

country_actor <- read.csv("country_crew_percentage.csv")

genre_nodes <- read.csv("genre_nodes.csv")

####Please note there is another read.csv function used inside the code, please find using the term 'read.csv' to find it. 



wordcloud_generator <- function(data, num_words = 100, background = "white"){
  
  
  if(is.character(data)){
    
    
    corpus <- Corpus(VectorSource(data))
    
    
    rm_spec_char <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    
    # Removing some special characters
    corpus <- tm_map(corpus, rm_spec_char, "/")
    corpus <- tm_map(corpus, rm_spec_char, "@")
    corpus <- tm_map(corpus, rm_spec_char, "\\|")
    
    
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    # Remove numbers
    corpus <- tm_map(corpus, removeNumbers)
    # Remove english common stopwords
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    # Removing some common terms
    corpus <- tm_map(corpus, removeWords, c("one", "two","three","jr.")) 
    # Remove punctuations
    corpus <- tm_map(corpus, removePunctuation)
    # Eliminate extra white spaces
    corpus <- tm_map(corpus, stripWhitespace)
    # Text stemming
    #corpus <- tm_map(corpus, stemDocument)
    
    tdm <- DocumentTermMatrix(corpus)
    
    colTotals <-  col_sums(tdm)
    
    remove(corpus)
    temp_data <- sort(colTotals, decreasing = TRUE)
    temp_data <- data.frame(word = names(temp_data), freq = as.numeric(temp_data))
    
    #Lemmatizing the words
    temp_data$word <- lemmatize_words(temp_data$word)
    
    #removing rows with words as numbers
    temp_data <- transform(temp_data[grep("[^0-9]", temp_data$word),,drop=F])
    
    #Regrouping again after lemmatizing
    temp_data <- sqldf("select word,freq from temp_data group by word")
    
    #Arranging the data
    temp_data <- temp_data[order(-temp_data$freq),]
    
    
  }
  
  if(!is.numeric(num_words) || num_words < 3){
    
    num_words <- 3
    
  }
  
  temp_data <- head(temp_data, n = num_words)
  
  if(nrow(temp_data) == 0){
    
    return(NULL)
    
  }
  
  wordcloud2(temp_data, backgroundColor = background, color=brewer.pal(8, "Dark2"), )
  
}





#Color scale for the network plot
colourScale <- 'd3.scaleOrdinal()
                    .domain(["Selected Genre" ,"Very Popular Genre", "Popular Genre", "Thriving Genre", "Less popular Genre", "Unpopular Genre"])
                    .range(["black","#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"]);'


# Spatial Analysis to see which parts of the world make the most movies
imdb_movies_overall <- imdb_movies[,c("imdb_title_id","country","year","genre")] %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country)

imdb_movies_overall <- imdb_movies_overall[,c("imdb_title_id","country","year","genre")] %>% 
  mutate(genre = strsplit(as.character(genre), ",")) %>% 
  unnest(genre)

movies_spatial <- imdb_movies_overall

movies_spatial$country <- trimws(movies_spatial$country, which = c("both"))
movies_spatial$genre <- trimws(movies_spatial$genre, which = c("both"))


##Function for breaks#########################################

only_ints <- function(t, i = 5) {
  temp <- pretty(t, i)
  temp[abs(temp %% 1) < .Machine$double.eps ^ 0.5] 
}

##############################################################


###### Leaftlet choropleth map of countries to show movie numbers and then click events on it---------------------

# Spatial Analysis to see which parts of the world make the most movies
map_movies <- imdb_movies[,c("imdb_title_id","country")] %>% 
  mutate(country = strsplit(as.character(country), ",")) %>% 
  unnest(country)

map_movies$country <- trimws(map_movies$country, which = c("both"))

movie_country_count <- as.data.frame(table(map_movies$country))

remove(map_movies)

names(movie_country_count) <- c("NAME","NumofMovies")

# Read this shape file with the rgdal library. 
world_spdf <- readOGR(dsn = "TM_WORLD_BORDERS_SIMPL-0.3",
                      layer = "TM_WORLD_BORDERS_SIMPL-0.3"
                      
)

# Clean the data object
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

movie_country_count$NAME <-  gsub("USA","United States", movie_country_count$NAME)
movie_country_count$NAME <-  gsub("UK","United Kingdom", movie_country_count$NAME)
movie_country_count$NAME <-  gsub("Soviet Union","Russia", movie_country_count$NAME)

# Merge does not work spatial data as it will make some changes internally ehich somehow breaks the slot relationships
#world_spdf@data <- merge(world_spdf@data, movie_country_count, by.x = "NAME" ,all.x = T)

world_spdf@data <- data.frame(world_spdf@data , movie_country_count[match(world_spdf@data$NAME, movie_country_count$NAME),])

world_spdf@data <- world_spdf@data[,c("FIPS","ISO2","ISO3","UN","NAME","AREA","POP2005",
                                      "REGION","SUBREGION","LON","LAT","NumofMovies")]



mybins <- c(0,500,1000,1500,3000,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=world_spdf@data$NumofMovies, na.color="transparent", bins=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>",
  "Number of Movies: ", round(world_spdf@data$NumofMovies, 2), 
  sep="") %>%
  lapply(htmltools::HTML)



#####------------------------------------------------------------------------------------------------------------


# Defining UI for the shiny application to draw the appropriate plots and leaflet maps
ui <- shinyUI(
  
  dashboardPage(
    
    dashboardHeader(title = "Data VIsualization Project - R", titleWidth = 400),
    
    
    
    # This is the sidebar of the UI application
    dashboardSidebar(width = 200,
                     sidebarMenu(id = "menu",
                       menuItem("Movies Overall", tabName = "overall", icon = icon("dashboard")),
                       menuItem("Actors and Actresses", tabName = "crew", icon = icon("dashboard")),
                       menuItem("Age Group and Genre", icon = icon("dashboard"), tabName = "textgen"),
                       menuItem("Genre Group Analysis", icon = icon("dashboard"), tabName = "netgen"),
                       menuItem("Genre Analysis", icon = icon("th"), tabName = "genre"),
                       menuItem("Genre Analysis by Country", icon = icon("th"), tabName = "genre_country")
                     )
    ),
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "overall",
                h2("Genesis of movies and overall analysis", style = "text-align:center"),
                tags$br(),
                fluidRow(
                  h4(htmlOutput("Intro_Page1"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                          
                           htmltools::div( plotlyOutput("overallmovietrend", height = 600, width = 1000), align="center" )
                    
                        ),
                tags$br(),
                fluidRow(
                  
                  h4(htmlOutput("insight_1"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;",)
             
                ),
                hr(),
                hr(),
                fluidRow(
                column(width = 1, offset = 11, align = "right",
                       
                       actionButton("tab1to2", label = icon("arrow-right"), width = 80)
                       
                )
                )
                
        ),
        
        tabItem(tabName = "crew",
                h2("Who dominates the industry actors or actresses?", style = "text-align:center"),
                tags$br(),
                tags$br(),
                fluidRow(
                  h4(htmlOutput("Intro_Page2"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                  
                ),
                hr(),
                hr(),
                fluidRow(
                 column(width = 8, offset = 4, align = "center",
                         box(
                           radioButtons(
                             inputId="actor_radio",
                             label="Country Selection Type:",
                             choices=list(
                               "All",
                               "Manual Select"
                             ),
                             selected="Manual Select"),

                           conditionalPanel(
                             condition = "input.actor_radio != 'All'",
                             selectInput("country","Select the Countries",
                                         choices = unique(country_actor$country), 
                                         selected = c("Japan","Sweden","USA","UK","India","Australia","Australia",
                                                    "Canada","Portugal","Iran","Denmark","Switzerland","Brazil",
                                                    "Greece","Philippines","Taiwan","Luxembourg","South Korea",
                                                    "Germany","Russia","Japan") ,multiple = TRUE) #unique(country_actor$country)[1:20]
                           ),
                           radioButtons(
                             inputId="sort_actor",
                             label="Select sorting parameter:",
                             choices=list(
                               "Actress Count",
                               "Actor Count",
                               "Actress %",
                               "Actor %"
                             ),
                             selected="Actress %")
                         )
                )
                ),
                uiOutput("fluid_row_actor"),
                hr(),
                fluidRow(
                  h4(htmlOutput("Insight_Page2"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                  
                ),
                hr(),
                hr(),
                fluidRow(column(width = 1,align = "left",

                                actionButton("tab2to1", label = icon("arrow-left"), width = 80)

                ),
                column(width = 1,offset = 10,align = "right",

                       actionButton("tab2to3", label = icon("arrow-right"), width = 80)

                )

                )

        )
        ,

        tabItem(tabName = "textgen",
                h2("Which Country and Age Group is the most active?", style = "text-align:center"),
                
                tags$br(),
                tags$br(),
                fluidRow(
                  
                  h4(htmlOutput("Intro_Page3"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;",)
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                  
                  h5(htmlOutput("mapheading",align = "center")),
                  htmltools::div( leafletOutput("ChoroLeaftletMap", height = 600, width = 1200), align="center")
                ),
                hr(),
                fluidRow(
                  column(width = 10, offset = 1,
                  uiOutput("level2_genre_UI"),
                  uiOutput("level2_wordcloud_UI")
                  )
                  ),
                  hr(),
                  hr(),
                fluidRow(
                  column(width = 10,offset = 1,
                         uiOutput("level3_age_grp_UI"),
                         uiOutput("level3_wordcloud_UI")
                  )
                  ),
                hr(),
                hr(),
                fluidRow(column(width = 1,align = "left",

                       actionButton("tab3to2", label = icon("arrow-left"), width = 80)

                ),
                column(width = 1, offset = 10, align = "right",

                       actionButton("tab3to4", label = icon("arrow-right"), width = 80)

                )
                )

        ),

        tabItem(tabName = "netgen",
                h2("Which Genres go together well?", style = "text-align:center"),
                tags$br(),
                tags$br(),
                fluidRow(
                  h4(htmlOutput("Intro_Page4"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                  htmltools::div(
                         box(
                           title = "Control for Year",
                           sliderInput("net_year_range", "Select year range:", 1925, 2019, c(1975,2019), sep = ""),
                           height = 160, width = 5
                         )
                          ,
                         box(
                           title = "Control for Genre",
                           selectInput(
                             inputId = 'net_genre',
                             label = 'Choose a Genre',
                             choices = unique(genre_nodes$genre),
                             selected = c("Thriller")
                           ),
                           height = 160, width = 5
                         )
                ,align = "center", style = "margin-left: 15%;")
                 
                  ),

                fluidRow(
                  htmltools::div(
                    box( title = textOutput("netheading"),
                    forceNetworkOutput("net_genre_plot", width = "100%", height = "1000px"),
                    width = 12),align = "center", style = "margin-left: 5%; margin-right: 5%;")
                ),
                hr(),
                fluidRow(
                  h4(htmlOutput("Insight_Page4"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                  
                ),
                hr(),
                hr(),
                fluidRow(column(width = 1,align = "left",

                                actionButton("tab4to3", label = icon("arrow-left"), width = 80)

                ),
                column(width = 1, offset = 10, align = "right",

                       actionButton("tab4to5", label = icon("arrow-right"), width = 80)

                )

                )


        ),

        tabItem(tabName = "genre",
                h2("In-depth Best/Worst Genre Analysis", style = "text-align:center"),
                tags$br(),
                tags$br(),
                fluidRow(
                  h4(htmlOutput("Intro_Page5"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;")
                  
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                  column(width = 8,offset = 4,align="centre",
                         box(
                           title = "Controls for Year",
                           sliderInput("year_range", "Select year range:", 1925, 2019, c(1975,2019), sep = "")
                         ),
                         br()
                  )),
                fluidRow(
                  column(width = 10, offset = 1,
                         box(plotlyOutput("topgenre_overall", height = 400),plotlyOutput("topgenreTrend_overall")),#,conditionalPanel("input.location_select == true",
                         
                         box(plotlyOutput("bottomgenre_overall", height = 400),plotlyOutput("botgenreTrend_overall"))#,conditionalPanel("input.location_select == true",
                         
                  )
                ),
                hr(),
                hr(),
                hr(),
                fluidRow(column(width = 1,align = "left",

                  actionButton("tab5to4", label = icon("arrow-left"), width = 80)

                ),
                column(width = 1, offset = 10, align = "right",

                  actionButton("tab5to6", label = icon("arrow-right"), width = 80)

                )

                )


        ),
        
        tabItem(tabName = "genre_country",
                h2("In-depth Best/Worst Genre Analysis by Country", style = "text-align:center"),
                tags$br(),
                tags$br(),
                fluidRow(
                  h4(htmlOutput("Intro_Page6"), align='justify', style = "font-family: 'times';color: #000000; margin-left:80px; margin-right:80px;",)
                  
                ),
                tags$br(),
                tags$br(),
                fluidRow(
                  column(width = 8,offset = 4,align="centre",
                         box(
                           title = "Controls for Year and Country",
                           sliderInput("country_year_range", "Select year range:", 1925, 2019, c(1975,2019), sep = ""),
                           selectInput(
                             inputId = 'country_page3',
                             label = 'Choose a Country',
                             choices = unique(movies_spatial$country)
                           )
                         ),
                         br()
                  )),
                fluidRow(
                  column(width = 10, offset = 1,
                         box(plotlyOutput("topgenre_country", height = 400),plotlyOutput("topgenreTrend_country", height = 400)),#,conditionalPanel("input.location_select == true",
                         
                         box(plotlyOutput("bottomgenre_country", height = 400),plotlyOutput("botgenreTrend_country", height = 400))#,conditionalPanel("input.location_select == true",
                         
                  )
                ),
                hr(),
                fluidRow(column(width = 1,align = "left",

                                actionButton("tab6to5", label = icon("arrow-left"), width = 80)

                )

                )


        )
      )
    )
  )
)


# Defining the server logic in the shiny sever function
server <- shinyServer(function(input, output, session) {
  
 
  ###################### Text for Page 1 ##########################################################
  
  output$Intro_Page1 <- renderText(paste0("The <B>Entertaiment Industry</B> has been giving us movies from a long time ago. However, in this application we would focus on the movies from
  1925 all the way upto 2019. As the time passed the entertainment industry grew and developed many more movies as compared to ealier years and so from the below line graph
  it is evident that industry thrived more during the period between <B>1975 and 2019</B> then in earlier years (between 1925-1974). Also at the beginning the lesser genres existed but as
  time went by the cinematography capabilities increased and also the genres which the movies were based on.",br(),br(),
  "The data here is from <B>IMDb (Internet Movie Database)</B>, which is a source through which people can explore and learn about movies and other aspects of the entertainment industry like 
  TV Serials, Web Series, etc. They can also provide ratings/votes to the movies/serials based on which the opinion of the masses can be given a calculated
  guess."))
  
  
  output$insight_1 <- renderText(paste0("The above line-chart tells us that the number of movies getting released each year started rising from 1950s-1970s but started increasing rapidly from 1990s 
  which can be connected to the cinematography gaining popularity/maturity with the passing digital revolution(started from 1950s and continued to the late 1970s)"))
 
  
  observeEvent(input$tab1to2, {
    updateTabItems(session, "menu",selected = "crew")
  })
  
  ###################### Text for Page 1 Finished #################################################

  
  ###################### Visualizations for Page 1 ##########################################################
  
  output$overallmovietrend <- renderPlotly({
    
    
    ggplotly(
      
      ggplot(data=movietrend_without, aes(x=Year, y=MovieCount, group=1)) +
        geom_area(color = "purple", fill = "darkred", aes(text = paste("Year:", Year,
                                                     "<br>Movies Released:", MovieCount))) +
        #geom_point(aes(x = Year, y = MovieCount),  color = "blue")
        geom_point(color = "black", aes(text = paste("Year:", Year,
                                                     "<br>Movies Released:", MovieCount))) +
        scale_x_continuous() +
        labs(title = "Area Chart for Number of movies released vs Year",
             x = "Year",
             y = "Number of Movies Released"
        ), tooltip = "text") %>% config(displayModeBar = F)
    
    
  })

  
  
  ###################### Visualizations for Page 1 Finished ##########################################################
  
  
  
  
  
  ##################################### Text for Page 2 ##########################################################
  
  output$Intro_Page2 <- renderText(paste0(
    
    "One of the most crucial parts of a movie is the <b>crew</b> that works hard to make the movie a success, the <b>actress, actor, director, producer</b> to name a few 
    but there are a lots of people behind the success of a movie. In this page we have explored the number of actresses and actors in different countries 
    and their percentages. This would help to understand who domainates in different parts of the world actors or actresses? This page is not only specific to the percentages of actors and actresses in each of the countries but here we can also see the trend of the number of actors 
    and actresses, in which specific years they increase/decrease. The <b>target audience</b>, for this page is the general public.<br><br>

    <B>Guidelines for the user to explore the page</B><br>
       We can see here that The first visualization used here is the dumbell plot or the lollipop graph in which the percentages of actors and actresses can be compared within a set of 
    countries or all of them. The radio button at the top allows to either select all the countries with one-click or to select the countries to compare manually. 
    After this the used can also click on the circles of a particular country to see the trend closely for this the area-chart is used. The colors used for the actors 
    is green and red for actresses for the circles and the same color is used for the area-charts.",br(),br(),
    
    "<B>Some selection options for the user to explore</B><br>
        Country selection 1 : <B>Switzerland</B><br>
        Country selection 2 : <B>Japan</B><br><br>
          
          <B>Insights from the lollipop graph</B><br>
          It is clearly evident from the lollipop graph that all the red circles are on the left and green circles are on the right, which means that atleast in the listed 
          countries there are no such examples where the actress percentage is greater than the actor percentage. If we switch to the all countries view and sort by actress 
          percentage, we can see that places like <b>Laos, Fiji, Samoa, Cayman Islands</b> have a greater acctress percentage than the actor percentage. (Please see below the graph 
          for more insights)."
  ))
  
  output$Insight_Page2 <- renderText(paste0(
    
    "<B>Insights from the first country selection</B><br>
        Though its close but in Switzerland for 2001 there are 26 actresses and 22 actors, similar is the case for 2016 where the count of actresses 
        is 21 and count of actors is 15. If you would explore you would see that the other cases also would generally come after 2000 so as the 
        awareness in people increased the difference between the number of actresses and actors decreased.<br><br>
        
     <B>Insights from the second country selection</B><br>
        In case of <b>Japan</b> the difference between the number of actresses and actors is less at the starting and then increases till 
        <b>1970s</b> after which the difference again decreases and then finally increases between <b>2000-2019</b>, which is different from what we 
        saw in the case of Switzerland.<br><br>"
  ))
  
  ##################################### Text for Page 2 Finished ##################################################
  
  
  ###################### Buttons for Page 2 ##################################################################
  
  observeEvent(input$tab2to3, {
    updateTabItems(session, "menu",selected = "textgen")
  })
  
  observeEvent(input$tab2to1, {
    updateTabItems(session, "menu",selected = "overall")
  })
  
  ###################### Buttons for Page 2 Finished ##########################################################
  
  
  
  ##################################### Visualizations for Page 2 ##################################################
  
  
  
  actorplotheight <- reactive({
    
    
    
    if(input$actor_radio == "All"){
      120
    } else{
      length(input$country)
      
    }
    
  })
  
  
  observe({
    
    output$fluid_row_actor <- renderUI({
      
      if(!is.null(event_data("plotly_click", source = "actorPlot", priority = "event"))){
        
        fluidRow(
          column(width = 6,
                 htmltools::div(plotlyOutput("actorPlot", height = max(800, (actorplotheight()*30)), width = 800), align = "left")
          ),
          column(width = 6,
                 htmltools::div(plotlyOutput("actor_country_trend", height = 600, width = 800), align = "right", style = "margin-top: 100px")#,
                 
          )
          
        )
  
      } else{
        
        fluidRow(
          
          htmltools::div(plotlyOutput("actorPlot", height = max(800, (actorplotheight()*30)), width = 800), align = "center"),
          
        )
        
      }
    })
    
    actorData <- reactive({
      
      if(input$actor_radio == "All"){
        country_actor_use <- country_actor
        
      } else{
        country_actor_use <- country_actor[country_actor$country %in% input$country,]
        
        
      }
      
      if(input$sort_actor == "Actor Count"){
        
        country_actor_use$country <- factor(country_actor_use$country,levels = unique(country_actor_use$country[order(country_actor_use$male_count,country_actor_use$male_pct)]))
        return(country_actor_use)
        
      } 
      
      if(input$sort_actor == "Actress Count"){
        
        country_actor_use$country <- factor(country_actor_use$country,levels = unique(country_actor_use$country[order(country_actor_use$female_count,country_actor_use$female_pct)]))
        return(country_actor_use)
        
      }
      
      if(input$sort_actor == "Actor %"){
        
        country_actor_use$country <- factor(country_actor_use$country,levels = unique(country_actor_use$country[order(country_actor_use$male_pct,country_actor_use$male_count)]))
        return(country_actor_use)
        
      } 
      
      if(input$sort_actor == "Actress %"){
        
        country_actor_use$country <- factor(country_actor_use$country,levels = unique(country_actor_use$country[order(country_actor_use$female_pct,country_actor_use$female_count)]))
        return(country_actor_use)
        
      }
      
    })
    
    output$actorPlot <- renderPlotly({
      
      
      
      ggplotly(
        ggplot(actorData()) +
          geom_segment(data = actorData(),aes(x = country, xend = country, y = male_pct, yend = female_pct, text = paste("Country :", country,
                                                                                                                         "<br>Actor % :", male_pct,
                                                                                                                         "<br>Actor Count :", male_count,
                                                                                                                         "<br>Actress % :", female_pct,
                                                                                                                         "<br>Actress Count :", female_count))) + #country_actor_use$Type
          geom_point( aes(x = country, y = male_pct, text = paste("Country :", country,
                                                                  "<br>Actor % :", male_pct,
                                                                  "<br>Actor Count :", male_count)), color = rgb(0.2,0.7,0.1,0.9), size = 4 ) + #rgb(0.2,0.7,0.1,0.5)
          geom_point( aes(x = country, y = female_pct, text = paste("country :", country,
                                                                    "<br>Actress % :", female_pct,
                                                                    "<br>Actress Count :", female_count)), color = rgb(0.7,0.2,0.1,0.9), size = 4 ) + #rgb(0.7,0.2,0.1,0.5)
          scale_y_continuous(breaks=seq(0, 100, 10)) +
          coord_flip()+
          
          labs(title = paste('Actor vs Actress % comparison for countries')) +
          xlab("Country") +
          ylab("Percentage"), tooltip = "text",source = "actorPlot") %>% config(displayModeBar = F)
      
    })
    
    
    selected_country <- reactive({
      
      actorData_genre <- event_data("plotly_click", source = "actorPlot", priority = "event")[2]
      
      data <- actorData()$country[(as.numeric(actorData_genre) + 1)]
      
      validate(
        need((!is.na(data) && length(data)== 1), "There is no country selected or you have clicked on the wrong point. Please select the country from the lollipop graph on the left. Click on the circles please, do not click on the line segment endings!")
      )
      
      data
      
    })
    
    
    output$actor_country_trend <- renderPlotly({
      
      selected <- crew_percentage[which(crew_percentage$country == as.character(selected_country())),]
      
      selected <- selected[order(selected$year),]
      
      ggplotly(
        
        ggplot(data=selected, aes(group=1)) +
          geom_area(fill = rgb(0.2,0.7,0.1,0.9), aes(x=year, y=male_count,text = paste("Year:", year,
                                                                                       "<br>Actor count:", male_count))) +
      
          geom_area(fill = rgb(0.7,0.2,0.1,0.9), aes(x=year, y=female_count,text = paste("Year:", year,
                                                                                         "<br>Actress count:", female_count))) +
          
          labs(title = paste("Area Chart for Number of actors and actresses in", as.character(selected_country())),
               x = "Year",
               y = "Number of Actors"
          )+
          scale_fill_discrete(name = "Crew Type")+
          scale_y_continuous(breaks = only_ints)+
          scale_x_continuous(breaks = only_ints), tooltip = "text") %>% config(displayModeBar = F)
      
      
    })
  
  })
  ################################ Visualizations for Page 2 Finished #############################################
  
  
  
  
  
  ###################### Text for Page 3 ##########################################################
  
  output$Intro_Page3 <- renderText(paste0(
  
  "The <B>movies, languages and likings for genres depend on the demographics and age groups of people</B> watching 
  and that is why this page has been dedicated towards that. The <b>target audience</b> for this page is the <b>Upcoming Directors</b> 
  as through this page they can analyse the country that they should be targetting and the age-group that they should be targetting as per the 
  genre which they want their movie to be based on.<br><br>
  
  <B>Guidelines for the user to explore the page</B><br>
  So basically this page has all the information that you can drill down starting from a 
  map with countries. The darker color in the countries shows more movies have been released there and hovering over them would show information like 
  country name and number of movies that have been released in the country overall. Clicking on the country shape would make a bar chart and a workdcloud 
  pop-up just below the map. The bar graph would show the top 8 genres of that country and the wordcloud would be based on the movie titles in that country 
  which would convey what the movies are centered around in that demographic.",br(),br(),
  "Further clicking on each of the genre bars would make another bar chart and wordcloud pop-up below it. The bar chart in this case would be based on the 
  age groups that have voted fo that genre and would convey that among which age group is the genre most popular.The wordcloud in this case would give us an idea 
  that what are the movies of that particular genre centered around in the selected country from the map.",br(),br(),
  
  "<B>Some selection options for the user to explore</B><br>
    <b>Selection options list 1:</b> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <b>Selection options list 2:</b><br>
    Country selection : <B>India</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp; Country selection : <B>Russia</B><br>
    Genre Selection 1 : <B>Comedy</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;            Genre Selection 1 : <B>Comedy</B><br>
    Genre Selection 2 : <B>Action</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;            Genre Selection 2 : <B>Romance</B><br>
    Genre Selection 3 : <B>Romance</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;                       Genre Selection 3 : <B>Adventure</B><br>
    Genre Selection 4 : <B>Family</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;                  Genre Selection 4 : <B>History</B><br><br>
    
    <B>Insights from the first list of selections</B><br>
    As it would be evident that in India, <B>Comedy and Action</B> genres are more popular within the <B>18-29</B> age group and the movies centered around words 
    like 'Raja'(means King), 'Veera'(means Strong) and 'chor'(means thief) attracts them.<br><br>
    
    <B>Romance and Family</B> genres are evidently more popular within the <B>30-44</B> age group and the movies centered around words 
    like 'Love', 'India' and 'Pyar'(means Love) attracts them."
  ))

  output$mapheading <- renderText(paste0("<b>Choropleth world map showing number of movies released</b>"))
  
  
  ###################### Text for Page 3 Finished ##########################################################
  
  
  ###################### Buttons for Page 3 ##########################################################
  
  observeEvent(input$tab3to4, {
    updateTabItems(session, "menu",selected = "netgen")
  })
  
  observeEvent(input$tab3to2, {
    updateTabItems(session, "menu",selected = "crew")
  })
  
  
  ###################### Buttons for Page 3 Finished ##########################################################
  
  
  #######################---------------------Page 3 Started----------------------------------#############################
  
  
  
  output$ChoroLeaftletMap <- renderLeaflet({
    
    
    # Final Map
    leaflet(world_spdf) %>%
      addTiles()  %>%
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(NumofMovies), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 0.5, weight = 2, sendToBack = FALSE, color = "black"),
        weight=0.3,
        label = mytext,
        layerId = world_spdf@data$NAME,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~NumofMovies, opacity=0.9, title = "Number of Movies", position = "bottomleft")
    
  })
  
 
  
  observe(
    {  
      
    click = input$ChoroLeaftletMap_shape_click
    if(is.null(click))
      return()
    
    
    genre_data_page2 <- reactive({

          tempdf <- unique(ratings_overall[which(ratings_overall$country == click$id),])

          genre_count_wo <- as.data.frame(table(tempdf$genre))

          names(genre_count_wo) <- c("genre","count")

          genre_count_wo <- genre_count_wo[order(-genre_count_wo$count),]

          genre_count_wo <- head(genre_count_wo,8)

          validate(
            need(nrow(genre_count_wo) > 0, "Selected values have no rows to show, please select other option from the drop down menu!")
          )

          genre_count_wo

      })
    
    country_data <- reactive({

      tempdf_country <- unique(ratings_overall[which(ratings_overall$country == click$id),])

      validate(
        need(nrow(tempdf_country) > 0, "Selected values have no rows to show, please select other option from the drop down menu!")
      )

      tempdf_country

    })
  
    if(!is.null(click$id)){
    
    output$level2_genre_UI <- renderUI({
      
      
             box(title = paste("Bar Chart for Top 8 Genres for", click$id),
             plotlyOutput("topgenre_country_bar_page2"),
             br()
             )
      
    })
    
    output$level2_wordcloud_UI <- renderUI({
      
     
             box(title = paste("Wordcloud for movie titles in",click$id),
             wordcloud2Output("wordcloud_country"),
             br()
        )
    
      
    })
    
    }
      
    
    
    output$topgenre_country_bar_page2 <- renderPlotly({
      
      ggplotly(
          
          ggplot(genre_data_page2(), aes(x=reorder(genre, -count), y = count, fill = genre,group=1)) +
            geom_bar(stat="identity",aes(text = paste("Genre:",genre,
                                                      "<br>Number of movies:",count))) +
            theme(axis.text.x = element_text(angle = 60))+
            labs(
                 x = "Genres",
                 y = "Number of Movies"
            )+scale_fill_discrete(name = "Genres"), source = "topgenre_country_bar_page2")
      
      })
      
      
      output$wordcloud_country <- renderWordcloud2({
        
        data <- as.character(country_data()$title)
        
        wordcloud_generator(data,100,'white')
        
   })
      
      
      genre_data_age_grp <- reactive({

         barData_genre <- event_data("plotly_click", source = "topgenre_country_bar_page2", priority = "event")
         
         i_genre <- genre_data_page2()$genre[(as.numeric(barData_genre[3]))]

         genre_data_df  <- unique(country_data()[which(country_data()$genre == as.character(i_genre)),])
         
         genredf_votes <- genre_data_df[,c("age_grp","total_avg_vote","total_votes")]
         
         validate(
           need(nrow(genredf_votes) > 0, "")
         )
         
         genredf_votes <- aggregate(genredf_votes[,c("total_avg_vote","total_votes")], by = list(genredf_votes$age_grp), FUN = mean, na.rm = TRUE)
         
         names(genredf_votes) <- c("age_grp","total_avg_vote","total_votes")

         genredf_votes

       })
      
      genre_data <- reactive({
        
        barData_genre <- event_data("plotly_click", source = "topgenre_country_bar_page2", priority = "event")
        
        i_genre <- genre_data_page2()$genre[(as.numeric(barData_genre[3]))]
        
        genre_data_df  <- unique(country_data()[which(country_data()$genre == as.character(i_genre)),])
        
        validate(
          need(nrow(genre_data_df) > 0, "")
        )
        
        genre_data_df
        
      })
      
      genre_click <- reactive({
        
        barData_genre <- event_data("plotly_click", source = "topgenre_country_bar_page2", priority = "event")
        
        i_genre <- genre_data_page2()$genre[(as.numeric(barData_genre$x))]
        
        i_genre
        
      })
      
      if(!is.null(event_data("plotly_click", source = "topgenre_country_bar_page2", priority = "event"))){
        
        output$level3_age_grp_UI <- renderUI({
          
          
          box(title = paste("Bar Chart for Age Groups for", genre_click(),"genre in",click$id),
              plotlyOutput("age_bar_votes_genre_page2"),
              br()
          )
          
          
        })
        
        output$level3_wordcloud_UI <- renderUI({
          
          
          box(title = paste("Wordcloud for movie titles for",genre_click(),"genre in",click$id),
              wordcloud2Output("wordcloud_genre"),
              br()
          )

          
        })
        
      }

      
      output$age_bar_votes_genre_page2 <- renderPlotly({
        
        ggplotly(
          
          ggplot(genre_data_age_grp(), aes(x=reorder(age_grp, -total_votes), y = total_votes, fill = age_grp)) +
            geom_bar(stat="identity") +
            theme(axis.text.x = element_text(angle = 60))+labs(
                                                               x = "Age Groups",
                                                               y = "Number of Votes"
            )+ scale_fill_discrete(name = "Age Groups"), source = "age_bar_votes_genre_page2")
        
      })
      
      
      output$wordcloud_genre <- renderWordcloud2({
        
        data_genre <- as.character(unique(genre_data()$title))
        
        wordcloud_generator(data_genre,100,'white')
        
        
      })
      
    })
     
      
      #######################---------------------Page 3 Finished----------------------------------#############################
      
  
  ###################### Text for Page 4 ##########################################################
  
  
  output$Intro_Page4 <- renderText(paste0(
    
    "The genres are very the basis of movies and are very important from a director's points of view to implement it properly.This page has the 
    network plot for the genres and explains how often the genres are clubbed together for movies which in-turn says how popular is the pairing. 
    The target audience for this page can the <b>Upcoming directors</b>, who are looking for which genres should they club to improve the chances of the 
    success of their movies. They can also study the trend from previous years, where a certain genre was being paired with another genre but as the time 
    progressed it was no longer being paired. This helps in understanding how the perspective of the viewers evolves over time. Another set of audience 
    can be <b>general public, Entertainment Provider companies(like Netflix, Amazon, etc.)</b><br><br>

     <B>Guidelines for the user to explore the page</B><br>
        To check that which genres a certain genre should most likely be paired with the below network plot can help. The radius of the nodes depends on the popularity 
         of the particular genre. The link-width between the nodes depends on the how many times the pair have been clubbed, the more the number of times the thicker the 
         link. The genre can be selected from the genre drop down menu and the year range can be selected from the year slider on the left. Also the nodes can be dragged 
         and to zoom  the network plot one can use scroll over the white box area of the network plot.",br(),br(),
    
    "<B>How has the plot been prepared</B><br>
    To give and idea about how to make sense of the network plot, for example: If a movie has three genres clubbed together like <b>Comedy, Thriller, Adventure</b> 
    then there would be a link from Comedy => Thriller and Comedy => Adventure, which will make a chain of <b>Thriller => Comedy => Adventure</b>.",br(),br(),
    
    "<B>Some selection options for the user to explore</B><br>
          Genre selection 1 : <B>Thriller</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp; Period selection 3 : <B>1970-1990</B><br>
          Period selection 1 : <B>1925-1950</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;Period selection 4 : <B>1990-2019</B><br>
          Period selection 2 : <B>1950-1970</B><br>
          (Look below the graph for insights.)"
  ))
  
  output$Insight_Page4 <- renderText(paste0(
    
    
     "<B>Insights from the first list of selections</B><br>
          Between <b>1925-1950</b>, Thriller genre has been clubed together with many genres like Action, Drama, Film-Noir, Mystery, etc. but as we move to the next year 
          selection range i.e. <b>1950-1970</b> it can be seen that there have some new additions that have taken place like clubbing Thriller with Fantasy, Sci-Fi, Musical, 
          Western. Between <b>1970-1990</b> the genre clubbing with Musical genre has been dropped because may be viewers did not like it much, the same is the case with 
          Western genre. Between <b>1990-2019</b> we can see another new addition 'Biography', but as the link-width is quite thin it can be said that its not a very prominent 
          connection. The most prominent clubbings with Thriller are with Drama, Comedy, Action, Crime."
  ))
  
  
        ###################### Text for Page 4 Finished #################################################
        
        
        
        ###################### Buttons for Page 4 ##########################################################
        
        observeEvent(input$tab4to3, {
          updateTabItems(session, "menu",selected = "textgen")
        })
        
        observeEvent(input$tab4to5, {
          updateTabItems(session, "menu",selected = "genre")
        })
        
        
        ###################### Buttons for Page 4 Finished #################################################
        
        
        
        
        
        ###################### Visualizations for Page 4 ##########################################################
        
        output$netheading <- renderText(paste("Network graph for", input$net_genre, "paired with other genres"))
        
        genre_nodes_data <- reactive({
          
          genre_nodes_df <- read.csv("genre_nodes.csv")
          
          genre_nodes_df$genre_group[which(genre_nodes_df$genre == as.character(input$net_genre))] <- "Selected Genre"
          
          genre_nodes_df
          
        })
        
        
        genre_data <- reactive({
          
          imdb_genres <- imdb_movies[,c("country","year","genre")]
          
          tempdf <- imdb_genres[imdb_genres$genre %like% input$net_genre,]
          
          source_v <- unlist(lapply(tempdf$genre,function(x){
            
            str_split(x, ", ")[[1]][1]
            
          }))
          
          target_v <- gsub("^[a-zA-Z/-]*[,]?","",tempdf$genre)
          target_v <- gsub(" ","",target_v)
          
          genre_links <- data.frame(c(source_v),c(target_v))
          
          genre_links$year <- NA
          
          genre_links$year <- tempdf$year
          
          names(genre_links) <- c("source","target","year")
          
          genre_links <- genre_links %>% 
            mutate(target = strsplit(as.character(target), ",")) %>% 
            unnest(target)
          
          validate(
            need(nrow(genre_links) > 0, "The selected genre does not have data, kindly select another genre!")
          )
          
          genre_links
          
        })
        
        
        genre_data_with_year <- reactive({
          
          genre_links_use <- genre_data() %>% 
            filter(year>=input$net_year_range[1] & year<=input$net_year_range[2])
         
          
          genre_links_use <- sqldf("select source,target,count(*) as value from genre_links_use group by source,target")
          
          genre_links_use$value <- rescale(genre_links_use$value,to = c(0,50))
          
          genre_links_use <- genre_links_use[which(genre_links_use$value > 0.5),]
          
          genre_nodes_df <- genre_nodes_data()
          
          genre_links_use <- sqldf("select gl.*,gn.rowNum as source_rownum from genre_links_use gl inner join genre_nodes_df gn on gl.source = gn.genre")
          
          genre_links_use <- sqldf("select gl.*,gn.rowNum as target_rownum from genre_links_use gl inner join genre_nodes_df gn on gl.target = gn.genre")
          
          validate(
            need(nrow(genre_links_use) > 0, "The selected genre does not have data, kindly select another genre!")
          )
          
          genre_links_use
          
        })
        
        output$net_genre_plot <- renderForceNetwork({
          
          forceNetwork(Links = genre_data_with_year(), Nodes = genre_nodes_data(),
                       Source = "source_rownum", Target = "target_rownum",
                       Value = "value", NodeID = "genre",
                       Group = "genre_group", opacity = 1, legend = TRUE,
                       zoom = TRUE,Nodesize = "node_radius",linkDistance = 500,
                       opacityNoHover = 1,colourScale = JS(colourScale),
                       width = 1000, height = 1200,fontSize = 14)
          
          
        })
        
        ###################### Visualizations for Page 4 Finished ##########################################################
  
  
  
  
  
      
  
  
      ###################### Text for Page 5 ##########################################################
      
      
        output$Intro_Page5 <- renderText(paste0(
          
          "The basic components of a movie are the genres on which the movie is based on. For the target audience, specifically the <b>Upcoming Directors</b>, this
          page can be used to view the best/worst genres within a specific period to study the different timelines. The best/worst genres and their trends can 
          be seen on this page very clearly.<br><br>

        <B>Guidelines for the user to explore the page</B><br>
           To see the trend of genres in a particular period use the year slider to select the period. After the selection is done the best/worst genres for that
          period would get displayed below the slider. If you want to focus on a particular genre just click the bar of that genre and the line-chart will change 
          for the genre. You can also zoom in on the line-chart for getting a closer look by just left-cliking and dragging to select a particular area to zoom in. 
          Use double left-click to zoom out.",br(),br(),
          
          "<B>Some selection options for the user to explore</B><br>
          <b>Selection options list 1:</b> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;&nbsp; <b>Selection options list 2:</b><br>
          Period selection 1 : <B>1925-1950</B> &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp Period selection 1 : <B>1925-2000</B><br>
          Period selection 2 : <B>1950-1970</B>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&nbsp;&nbsp;&nbsp;     Period selection 2 : <B>2000-2019</B><br>
          Period selection 3 : <B>1970-1990</B><br><br>
          
          <B>Insights from the first list of selections</B><br>
          Between <b>1925-1950, Horror genre</b> was amongst the worst 8 genres overall. After the digital revolution hit the market and the visual effects started getting
          better and better Horror started gaining momentum and came into the list of top 8 genres between the period <b>1950-1970</b>, further in the period <b>1970-1990</b>
          Horror started coming in the top 5 genres. On the other hand Crime and Adventure were comparitively more popular in the last century than in this century.<br><br>
          
          <B>Insights from the second list of selections</B><br>
          Here, we are comparing the famous genres between the two centuries, so in the period of <b>1925-2000</b> the genres like Thriller and Horror are on the 7th and 8th 
          positions but in around the last 20 years(2000-2019) Thriller and Horror have picked up and specially Thriller has become the 3rd most popular genre.The genres like 
          Crime and Adventure have lost their places to become the 7th and 8th best.Documentary and Reality-TV are the worst genres irrespective of the period"
        ))
  
      
      
      ###################### Text for Page 5 Finished #################################################
  
  
  
      ###################### Buttons for Page 5 ##########################################################
          
          observeEvent(input$tab5to4, {
            updateTabItems(session, "menu",selected = "netgen")
          })
          
          observeEvent(input$tab5to6, {
            updateTabItems(session, "menu",selected = "genre_country")
          })
          
          
      ###################### Buttons for Page 5 Finished #################################################
  
  
  
      
      
      ###################### Visualizations for Page 5 ##########################################################
          
          
        year_data <- reactive({
          
          required_df <- movies_spatial[,c("imdb_title_id","year","genre")]
          
          required_df <- required_df %>% 
            filter(year>=input$year_range[1] & year<=input$year_range[2])
          
          required_df <- unique(required_df)
          
          validate(
            need(nrow(required_df) > 0, "Selected values have no rows to show, please select other options please!")
          )
          
          required_df
          
        })
        
        
        year_genre_count <- reactive({
          
          genre_count_wo <- as.data.frame(table(year_data()$genre))
          
          names(genre_count_wo) <- c("genre","count")
          
          genre_count_wo <- genre_count_wo[order(-genre_count_wo$count),]
          
          validate(
            need(nrow(genre_count_wo) > 0, "Selected values have no rows to show, please select other options please!")
          )
          
          genre_count_wo
          
        })
        
        
        # This is where the we are outputing the top 8 genres
        output$topgenre_overall <- renderPlotly({
          
          
          genre_count_wo <- head(year_genre_count(),8)
          
          ggplotly(
            
            ggplot(genre_count_wo, aes(x=reorder(genre, -count), y = count, fill = genre)) +
              geom_bar(stat="identity", aes(text = paste("Genre:", genre,
                                                         "<br>Number of Movies:", count))) +
              theme(axis.text.x = element_text(angle = 60))+
              labs(title = paste("Bar Chart for Top 8 Genres between", input$year_range[1], "and", input$year_range[2]),
                   x = "Genres",
                   y = "Number of Movies"
              )+
              scale_fill_discrete(name = "Genres"), source = "topgenre", tooltip = "text") %>% config(displayModeBar = F)
          
          
          
        })
        
        
        # This is where we are outputing the bottom 8 genres
        output$bottomgenre_overall <- renderPlotly({
          
          genre_count_wo <- tail(year_genre_count(),8)
          
          ggplotly(
            ggplot(genre_count_wo, aes(x=reorder(genre, -count), y = count, fill = genre)) +
              geom_bar(stat="identity", aes(text = paste("Genre: ", genre,
                                                         "<br>Number of Movies: ", count))) +
              theme(axis.text.x = element_text(angle = 60))+
              labs(title = paste("Bar Chart for Worst 8 Genres between", input$year_range[1], "and", input$year_range[2]),
                   x = "Genres",
                   y = "Number of Movies"
              )+
              
              scale_fill_discrete(name = "Genres"),source = "bottomgenre", tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        
        output$botgenreTrend_overall <- renderPlotly({
          
          genre_count_wo <- tail(year_genre_count(),8)
          
          genre_count <- as.data.frame(table(year_data()$genre,year_data()$year))
          
          names(genre_count) <- c("genre","year","count")
          
          genre_count$year <- as.numeric(as.character(genre_count$year))
          
          barData <- event_data("plotly_click", source = "bottomgenre")
          
          i <- genre_count_wo$genre[(as.numeric(barData$x))]
          
          if (is.null(barData)){
            i <- genre_count_wo$genre[1]
          }
          
          ggplotly(
            ggplot(genre_count[which(genre_count$genre == as.character(i)),], aes(x=year,y=count,group=1)) +
              geom_line(color = "black",aes(text = paste("Year:",year,
                                                         "<br>Number of movies:",count)),show.legend = FALSE) +
              geom_point(color = "darkred",aes(text = paste("Year:",year,
                                                            "<br>Number of movies:",count)))+
              labs(title=paste("Line Chart for", as.character(i),"genre between",input$year_range[1], "and", input$year_range[2]),
                   x="Year",
                   y="Number of movies")+
              scale_y_continuous(breaks = only_ints), tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        output$topgenreTrend_overall <- renderPlotly({
          
          genre_count_wo <- head(year_genre_count(),8)
          
          genre_count <- as.data.frame(table(year_data()$genre,year_data()$year))
          
          names(genre_count) <- c("genre","year","count")
          
          genre_count$year <- as.numeric(as.character(genre_count$year))
          
          barData <- event_data("plotly_click", source = "topgenre")
          
          i <- genre_count_wo$genre[(as.numeric(barData$x))]
          
          if (is.null(barData)){
            i <- genre_count_wo$genre[1]
          }
          
          ggplotly(
            ggplot(genre_count[which(genre_count$genre == as.character(i)),], aes(x=year,y=count,group=1)) +
              geom_line(color = "black",aes(text = paste("Year:",year,
                                                         "<br>Number of movies:",count)),show.legend = FALSE) +
              geom_point(color = "darkred",aes(text = paste("Year:",year,
                                                            "<br>Number of movies:",count)))+
              labs(title=paste("Line Chart for", as.character(i),"genre between",input$year_range[1], "and", input$year_range[2]),
                   x="Year",
                   y="Number of movies")+
              scale_y_continuous(breaks = only_ints),tooltip = "text") %>% config(displayModeBar = F)
          
        })
      
        
        
        ###################### Visualizations for Page 5 Finished ##########################################################
      
        
        
        ##################################### Text for Page 6 ##########################################################
        
        output$Intro_Page6 <- renderText(paste0(
          
          "This page is similar to the previous page but it gives more depth to the genre analysis. The genre trend can be seen in a particular period 
          in a particular country.<br><br>

        <B>Guidelines for the user to explore the page</B><br>
           To see the trend of genres in a particular period use the year slider to select the period, further the country can be selected from the available drop 
           down menu. If you want to focus on a particular genre just click the bar of that genre and the line-chart will change 
          for the genre. You can also zoom in on the line-chart for getting a closer look by just left-cliking and dragging to select a particular area to zoom in. 
          Use double left-click to zoom out.",br(),br(),
          
          "<B>Some selection options for the user to explore</B><br>
          <b>Selection options list 1:</b><br>
          Country selection 1 : <B>Japan</B> and Period selection 1 : <b>2000-2019</b><br>
          Country selection 2 : <B>China</B> and Period selection 2 : <b>2000-2019</b><br>
          Country selection 3 : <B>India</B> and Period selection 3 : <b>2000-2019</b><br><br>
          
          <B>Insights from the first list of selections</B><br>
          For the period <b>2000-2019</b>, it is evident that the three countries with major share in the whole entertainment industry have Action as one of the most 
          popular genres as opposed to genre trend of the rest of the world, where Thriller and Romance beat the popularity of Action genre.<br>
          In <b>Japan</b>, we also have <b>Animation</b> taking the 4th place and correctly so because Japan is known for its Animes and therefore the Animation genre is quite popular there. 
          If we see the trend of the Animation genre for Japan we can see that the genre picked only after the late 1970s because the digital effects started getting matured 
          and the Animes obviously depend on a lot of visual effects."
        ))
        
        
        ##################################### Text for Page 6 Finished ##################################################
        
        
        ###################### Buttons for Page 6 ##################################################################
        
        
        observeEvent(input$tab6to5, {
          updateTabItems(session, "menu",selected = "genre")
        })

        ###################### Buttons for Page 6 Finished ##########################################################
        
        
        
        ##################################### Visualizations for Page 6 ##################################################
        
        year_data_country <- reactive({
          
          movies_spatial_country <- movies_spatial %>% 
            filter(year>=input$country_year_range[1] & year<=input$country_year_range[2])
          
          movies_spatial_country <- movies_spatial_country[which(movies_spatial_country$country == input$country_page3),]
          
          movies_spatial_country <- unique(movies_spatial_country)
          
          validate(
            need(nrow(movies_spatial_country) > 0, "Selected values have no rows to show, please select other options please!")
          )
          
          movies_spatial_country
          
        })
        
        
        year_genre_country_count <- reactive({
          
          genre_count_w <- as.data.frame(table(year_data_country()$genre))
          
          names(genre_count_w) <- c("genre","count")
          
          genre_count_w <- genre_count_w[order(-genre_count_w$count),]
          
          validate(
            need(nrow(genre_count_w) > 0, "Selected values have no rows to show, please select other options please!")
          )
          
          genre_count_w
          
        })
        
        
        
        output$topgenre_country <- renderPlotly({
          
          genre_count_w <- head(year_genre_country_count(),8)
          
          ggplotly(
            
            ggplot(genre_count_w, aes(x=reorder(genre, -count), y = count, fill = genre)) +
              geom_bar(stat="identity", aes(text = paste("Genre:", genre,
                                                         "<br>Number of Movies:", count))) +
              theme(axis.text.x = element_text(angle = 60))+
              labs(title = paste("Bar Chart for Top 8 Genres between", input$country_year_range[1], "and",
                                 input$country_year_range[2], "for",input$country_page3),
                   x = "Genres",
                   y = "Number of Movies"
              )+
              scale_fill_discrete(name = "Genres"), source = "topgenre_country", tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        
        
        
        output$bottomgenre_country <- renderPlotly({
          
          genre_count_w <- tail(year_genre_country_count(),8)
          
          ggplotly(
            
            ggplot(genre_count_w, aes(x=reorder(genre, -count), y = count, fill = genre)) +
              geom_bar(stat="identity", aes(text = paste("Genre:", genre,
                                                         "<br>Number of Movies:", count))) +
              theme(axis.text.x = element_text(angle = 60))+
              labs(title = paste("Bar Chart for Worst 8 Genres between", input$country_year_range[1], "and",
                                 input$country_year_range[2], "for",input$country_page3),
                   x = "Genres",
                   y = "Number of Movies"
              )+
              scale_fill_discrete(name = "Genres"),source = "bottomgenre_country", tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        output$botgenreTrend_country <- renderPlotly({
          
          
          required_df_bot <- year_data_country()[,c("imdb_title_id","year","genre","country")]
          
          genre_count_w <- tail(year_genre_country_count(),8)
          
          genre_count <- as.data.frame(table(required_df_bot$genre,required_df_bot$year))
          
          names(genre_count) <- c("genre","year","count")
          
          genre_count$year <- as.numeric(as.character(genre_count$year))
          
          barData <- event_data("plotly_click", source = "bottomgenre_country")
          
          i <- genre_count_w$genre[(as.numeric(barData$x))]
          
          if (is.null(barData)){
            i <- genre_count_w$genre[1]
          }
          
          ggplotly(
            ggplot(genre_count[genre_count$genre == as.character(i),], aes(x=year,y=count,group=1)) +
              geom_line(color = "black",aes(text = paste("Year:",year,
                                                         "<br>Number of movies:",count)),show.legend = FALSE) +
              geom_point(color = "darkred",aes(text = paste("Year:",year,
                                                            "<br>Number of movies:",count)))+
              labs(title=paste("Line Chart for ", as.character(i)," genre",sep = ""),
                   x="Year",
                   y="Number of movies")+
              scale_y_continuous(breaks = only_ints), tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        output$topgenreTrend_country <- renderPlotly({
          
          
          required_df_top <- year_data_country()[,c("imdb_title_id","year","genre","country")]
          
          genre_count_w <- head(year_genre_country_count(),8)
          
          genre_count <- as.data.frame(table(required_df_top$genre,required_df_top$year))
          
          names(genre_count) <- c("genre","year","count")
          
          genre_count$year <- as.numeric(as.character(genre_count$year))
          
          barData <- event_data("plotly_click", source = "topgenre_country")
          
          i <- genre_count_w$genre[(as.numeric(barData$x))]
          
          if (is.null(barData)){
            i <- genre_count_w$genre[1]
          }
          
          ggplotly(
            ggplot(genre_count[genre_count$genre == as.character(i),], aes(x=year,y=count,group=1)) +
              geom_line(color = "black",aes(text = paste("Year:",year,
                                                         "<br>Number of movies:",count)),show.legend = FALSE) +
              geom_point(color = "darkred",aes(text = paste("Year:",year,
                                                            "<br>Number of movies:",count)))+
              labs(title=paste("Line Chart for ", as.character(i)," genre",sep = ""),
                   x="Year",
                   y="Number of movies")+
              scale_y_continuous(breaks = only_ints), tooltip = "text") %>% config(displayModeBar = F)
          
        })
        
        
        ################################ Visualizations for Page 6 Finished #############################################
        
        
})


# Code to launch the shiny application
shinyApp(ui = ui, server = server)