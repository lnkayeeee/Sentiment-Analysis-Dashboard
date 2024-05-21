
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(bslib)
library(plotly)
library(htmltools)
library(bsicons)
library(stm)
library(reshape2)
library(ggplot2)
library(wordcloud2)
library(wordcloud)
library(DT)
library(png)


# data processing 
data <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRX4vtsgxgINoa_2GqAM4-4a2SI9Lp6knwI6-P0P1SAnCoARiYZYuZ0Dl_ztA1wreY9XXcipYJZvVLD/pub?output=csv')
dataset <- data %>%
  mutate(id_num = 1:nrow(data), .after = Email) %>%
  tibble()

stopwords <- get_stopwords(source = 'snowball')

cleaned_text <- dataset %>%
  unnest_tokens(word, Feedback) %>%
  select(id_num, word) %>%
  anti_join(stopwords)

sentiments_data_bing <- cleaned_text %>%
  inner_join(get_sentiments('bing'))

sentiments_data_bing_numeric <- sentiments_data_bing %>%
  mutate(sentiment_value = ifelse(sentiment == 'positive', 1, -1))

results <- sentiments_data_bing_numeric %>%
  group_by(id_num) %>%
  summarise(total = sum(sentiment_value))

results_final <- results %>%
  mutate(overall_score = ifelse(total > 0, 'positive', 
                                ifelse(total < 0, 'negative', 'neutral')))

final_data <- results_final %>% full_join(dataset, by = 'id_num')
final_data <- final_data %>% 
  select(id_num, Timestamp, Email, College, Feedback, overall_score)

final_data <- final_data %>%
  mutate(overall_score = replace_na(overall_score, 'invalid'))

# number of responses
total_responses <- nrow(final_data)

# number of positive in percentage
total_positive <- final_data %>% 
  filter(overall_score == 'positive')
percent_positive <- round((nrow(total_positive) * 100) / total_responses, 0) 

# number of negative in percentage
total_negative <- final_data %>%
  filter(overall_score == 'negative')
percent_negative <- round((nrow(total_negative) * 100) / total_responses, 0)

# number of neutral in percentage
total_neutral <- final_data %>%
  filter(overall_score == 'neutral')
percent_neutral <- round((nrow(total_neutral) * 100) / total_responses, 0)

# topic modelling data
dataset_tm <- dataset %>%
  mutate(id_num = 1:nrow(dataset), .after = Email) %>%
  tibble()

cleaned_text <- dataset_tm %>%
  unnest_tokens(word, Feedback) %>%
  select(id_num, word) %>%
  anti_join(stop_words)

data_tm <- cleaned_text %>%
  count(id_num, word) %>%
  cast_sparse(id_num, word, n)

set.seed(123)

topic_model <- stm(data_tm, K = 30)

topic_gamma <- tidy(
  topic_model, 
  matrix='gamma',
  document_names = rownames(data_tm)
)

dataset_tm <- dataset_tm %>% mutate(id_num = as.character(id_num))

results_tm <- topic_gamma %>%
  left_join(dataset_tm, by = c("document" = "id_num")) %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) %>%
  select(document, topic, gamma)

results_tm <- results_tm %>%
  mutate(document = as.numeric(document)) %>%
  arrange(document)

summary_scores <- summary(topic_model)
scores <- summary_scores$score
scores <- as.data.frame(scores)

scores <- scores %>%
  mutate(topic = 1:nrow(scores), .before = V1)

trending_topics_data <- results_tm %>%
  inner_join(scores, by = 'topic')

trending_topics <- trending_topics_data %>%
  pivot_longer(cols = names(trending_topics_data)[4:10],
               values_to = 'words') %>%
  select(words)

top_topics_final <- trending_topics %>%
  group_by(words) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  head(8)


# per college data
perCollegeFinal <- final_data %>%
  mutate(CollegeAcronym = ifelse(College == 'College of Information and Computing', 'CIC',
                          ifelse(College == 'College of Engineering', 'COE',
                          ifelse(College == 'College of Education', 'CEd',
                          ifelse(College == 'College of Business and Administration', 'CBA',
                          ifelse(College == 'College of Arts and Sciences', 'CAS',
                          ifelse(College == 'College of Applied Economics', 'CAEc', 'CT'))))))
        )


# for sentiment overtime
senti_overtime_data <- final_data
senti_overtime_data$Timestamp <- mdy_hms(senti_overtime_data$Timestamp)

df <- senti_overtime_data %>%
  mutate(year = year(Timestamp),
         month = month(Timestamp),
         day = day(Timestamp),
         hour = hour(Timestamp),
         minute = minute(Timestamp),
         second = second(Timestamp)
        )


positive_data <- df %>%
  filter(overall_score == 'positive')

negative_data <- df %>%
  filter(overall_score == 'negative')

neutral_data <- df %>%
  filter(overall_score == 'neutral')

positive_line <- positive_data %>%
  group_by(month) %>%
  summarise(positive = n())

negative_line <- negative_data %>%
  group_by(month) %>%
  summarise(negative = n())

neutral_line <- neutral_data %>%
  group_by(month) %>%
  summarise(neutral = n())

overall_line <- positive_line %>%
  full_join(negative_line, by = 'month')

overall_line <- overall_line %>%
  full_join(neutral_line, by = 'month')

overall_line <- overall_line %>%
  mutate(positive = replace_na(positive, 0))

overall_line <- overall_line %>%
  mutate(negative = replace_na(negative, 0))

overall_line <- overall_line %>%
  mutate(neutral = replace_na(neutral, 0))


# per college view
caecData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CAEc')

casData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CAS')

cbaData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CBA')

cedData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CEd')

coeData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'COE')

cicData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CIC')

ctData <- perCollegeFinal %>%
  filter(CollegeAcronym == 'CT')


# wordcloud2a function for rendering
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
                                                                                                                       browser.padding = 0, browser.fill = TRUE))
  chart
}


# customize color of header and sidebar
css <- HTML('
        
        /* logo */  
        .skin-blue .main-header .logo {
          background-color: #760102;
           position: fixed;
        }
        
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
          background-color: #760102;
        }
        
        /* navbar when hovered */
        .skin-blue .main-header .navbar {
          background-color: #760102;
           
        }
        
        /* main sidebar */
        .skin-blue .main-sidebar {
          background-color: #760102;
          font-family: "Poppins", sans-serif;
          font-size: 14px;
          
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #B60707;
        }
        
        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
          background-color: #B60707;
         }
        
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #760102;
         }
         
         .skin-blue .main-sidebar {
          position: fixed;
         }
        
        .skin-blue .nav-bar {
          position: fixed;
        }
         
        
      ')


ui <- dashboardPage(
  # title for tab when view
  title = "USeP Student Feedback Dashboard",
  
  # dashboard header - defines header details 
  dashboardHeader(
    
    # set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 60px}"),
            tags$style(".main-header .logo {height: 60px;}"),
            tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:20px !important}")
    ) 
  ),
  
  # dashboard sidebar - defines sidebar details
  dashboardSidebar(
    
    # Adjust the sidebar
    tags$style(".left-side, .main-sidebar {padding-top: 60px}"),
    
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overall"),
      menuItem("Feedback by College", tabName = "perCollege"),
      menuItem("About", tabName = "about")
    )
  ),
  
  # dashboard body - defines dashboard contents
  dashboardBody(
    
    # place toggle button on left side
    tags$head(tags$style(HTML('
    /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                                margin-left: 0px !important;
                                align-items: center;
                                line-height: 58px;
                                position: fixed;
                                width:100%;
                              }
        
       
                              '))),
    tags$style(type="text/css",".sidebar-toggle{position: absolute; left: 0;}"),

    
    # code to add text after header and add logo before and after text
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 23px;
        line-height: 60px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
        align-items: center;
        margin-left: 40px;
      }
      
       .myClass img {
        height: 40px; 
        margin-right: 10px; /* space between the usep logo and title */
       }
       
       .myClass #foi-logo{
       margin-left: 500px; /* space between title and right logo */
       }
       
       .row {
       margin-top: 70px;
       margin-left: -15px;
       margin-right: 2px;
       }

    
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(
        \'<span class="myClass"><img src="logo.png" alt="Logo"> USeP Student Feedback Dashboard <img id="foi-logo" src="foi.png" alt="Logo"><img src="pts.png" alt="Logo"><img src="spjrd.jpg" alt="Logo"><img src="socotec.jpg" alt="Logo"></span>\');
      })
     ')),
    
    tags$head(
      tags$link(href="https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap", rel="stylesheet")
    ),
    
    # customize header and sidebar
    tags$head(tags$style(css)),
    
    tags$style(HTML("
    .custom-box {
      display: inline-block;
      padding: 8px 10px 0px 10px;
    }
    
    .custom-box p {
      font-size: 12px;
      color: #000000;
      font-family: 'Montserrat', sans-serif;
      font-weight: 600;
      margin: 0 0 0px;
    }
    
    .value-total {
      font-size: 47px;
      font-family: 'Helvetica', sans-serif;
      font-weight: 600;
      position: relative;
      display: inline-block;
    
    }
    
    .value-positive {
      font-size: 47px;
      font-family: 'Helvetica', sans-serif;
      font-weight: 600;
      position: relative;
      display: inline-block;
      color: #139D50;
    
    }
    
     .value-negative {
      font-size: 47px;
      font-family: 'Helvetica', sans-serif;
      font-weight: 600;
      position: relative;
      display: inline-block;
      color: #D2222D;
    
     }
    
     .value-neutral{
      font-size: 47px;
      font-family: 'Helvetica', sans-serif;
      font-weight: 600;
      position: relative;
      display: inline-block;
      color: #FEAC00;
    
     }
    
    .percent-sign {
        font-size: 16px;
        position: absolute;
        top: 9px;
        right: -15px;
    }
    
    /* data table customize */
    .dataTables_wrapper .dataTables_length {
        float: right;
    }
    .dataTables_wrapper .dataTables_filter {
        float: left;
        text-align: left;
    }
    
    .dataTables_wrapper .dataTables_filter input {
        width: 350px; /* Increase the width of the search input */
        border-radius: 8px; /* Change the round corners */
        padding: 5px; /* Optional: Add some padding for better look */
        border: 0.5px solid #ccc; /* Optional: Add a border */
    }
    
    .dataTables_wrapper .dataTable tr {
        font-weight: meidium; /* Normal font weight for rows */
    }
    
    .dataTables_wrapper {
      max-height: 397px;
    }
      
    
  ")),
    
    # for input selector customization
    tags$head(
      tags$style(HTML("
      /* Custom styles for input selectors */
      .selectize-input, .selectize-dropdown {
        font-family: 'Montserrat', sans-serif; /* Change to your desired font family */
        font-size: 14px;                /* Change to your desired font size */
        width: 100%;
      }

      .selectize-input {
        font-weight: normal;              /* Change to your desired font weight */
        width: 97%;                   /* Change to your desired width */
        margin-left: 8px;
      }

      .selectize-dropdown-content {
        font-family: 'Montserrat', sans-serif; /* Ensures dropdown items have the same font */
        font-size: 14px;                /* Ensures dropdown items have the same size */
        font-weight: normal;              /* Ensures dropdown items have the same weight */
        width: 100%;
      }
      
      .selectize-dropdown {
        margin-left: 8px;
        width: 97% !important;
        min-width: 200px !important;
        
      }
      
      .box-title {
        font-family: 'Montserrat', sans-serif;
        font-size: 14px !important;
        font-weight: bold;
        margin-left: 12px !important;
        margin-bottom: 0px !important;
        
        
      }
      
      .col-sm-12 {
        padding-right: 8px;
        padding-left: 8px;
        margin-right: 10px;
      }
      
      .col-sm-6 {
        padding-left: 8px;
        padding-right: 8px;
      }
      
      
      /* about page css */
      .p-name {
        font-family: 'Montserrat', sans-serif;
        font-size: 16px;
        font-weight: bold;
        margin: 8px 0 0 0;
      }
      
      .p-email {
        font-family: 'Montserrat', sans-serif;
        font-size: 9px;
        font-weight: normal;
        font-style: italic;
        margin: 0px;
      }
      
      .p-role {
        font-family: 'Montserrat', sans-serif;
        font-size: 9px;
        font-weight: normal;
        font-style: italic;
        margin: 0px;
      }
      
      .p-title {
        font-family: 'Montserrat', sans-serif; 
        font-size: 28px; 
        font-weight: bold
      }
      
      .p-content {
        font-size: 14px;
      }
      
    
      
    "))
    ),
    
    # tabs items
    tabItems(
      
      # tab item for dashboard overview
      tabItem(
        tabName = "overall",
        
        fluidRow(
          
          # for value box and table
          column(style = "margin: 0px !important;",
            width = 6,
            box(
              width = 12,
              div(style="display: flex; justify-content: space-between;",
                  div(class = "custom-box",
                      p("Feedbacks"),
                      tags$div(textOutput("totalResponses"), class="value-total")
                  ),
                  div(class="custom-box custom-box-positive",
                      p("Positive"),
                      div(
                        class="",
                        tags$div(htmlOutput("percentPositive"), class="value-positive"),
                      )
                      
                  ),
                  div(class="custom-box custom-box-negative",
                      p("Negative"),
                      tags$div(htmlOutput("percentNegative"), class="value-negative")
                  ),
                  div(class="custom-box custom-box-neutral",
                      p("Neutral"),
                      tags$div(htmlOutput("percentNeutral"), class="value-neutral")
                  )
              )
            ),
            
            box(
              width = 12,
              div(style = "font-size: 80%; font-family: 'Montserrat', sans-serif; font-weight: 500;",
                DT::dataTableOutput("sentimentTable")
              )
            ),
            
            # word cloud
            box(style = "margin: 0px; padding: 0px;",
              title = 'Word Cloud',
              width = 12,
              column(
                12, align = "center",
                div(
                  wordcloud2Output("wordCloud", height = 275, width = "auto")
                )
              )
            )
          ),
          
          # for graphs 
          column(style = "margin: 0px !important; padding: 0px !important;",
            width = 6,
            
            # feedback per college bar plot
            box(style = "margin: 0px; padding: 0px;",
              title = "Feedback per College",
              width = 12,
              div(
                plotlyOutput("fbPerCollegePlot", height = 275)
              )
            ),
            
            # feedback overtime line plot
            box(style = "margin: 0px !important; padding: 0px !important;",
              title = 'Feedback Overtime',
              width = 12,
              div(
                plotlyOutput("feedbackOvertime", height = 275)
              )
            ),
            
            # top topics bar plot
            box(style = "margin: 0px; padding: 0px;",
              width = 12,
              title = "Top Feedback Topics",
              div(
                plotlyOutput("topTopicsPlot", height = 275)
              )
            )

          )
        ),
        
      
      ),
      
      # tab item for feedback by college
      tabItem(
        tabName = "perCollege",
        
        div(style = "margin-top: 60px; margin-left: 0px;",
          fluidRow(
            
            # drop down, value boxes, and table
            column(style = "margin: 0px !important;",
              width = 6,
              selectInput("collegeChoice", NULL, width = "100%",
                          choices = c("Select College" = "", 
                                      "College of Applied Economics", 
                                      "College of Arts and Sciences",
                                      "College of Business and Administration",
                                      "College of Education",
                                      "College of Engineering",
                                      "College of Information and Computing",
                                      "College of Technology")
              ),
              
              # value box 
              box(
                width = 12,
                div(style="display: flex; justify-content: space-between;",
                    div(class = "custom-box",
                        p("Feedbacks"),
                        tags$div(textOutput("totalResponsesPC"), class="value-total")
                    ),
                    div(class="custom-box custom-box-positive",
                        p("Positive"),
                        div(
                          class="",
                          tags$div(htmlOutput("percentPositivePC"), class="value-positive"),
                        )
                        
                    ),
                    div(class="custom-box custom-box-negative",
                        p("Negative"),
                        tags$div(htmlOutput("percentNegativePC"), class="value-negative")
                    ),
                    div(class="custom-box custom-box-neutral",
                        p("Neutral"),
                        tags$div(htmlOutput("percentNeutralPC"), class="value-neutral")
                    )
                )
              ),
              
              # table per college
              box( 
                width = 12,
                div(style = "font-size: 80%; font-family: 'Montserrat', sans-serif; font-weight: 500;",
                    DT::dataTableOutput("sentimentTablePC")
                )
              )
            ),
            
            # graphs
            column(style = "margin: 0px !important; padding: 0px !important;",
              width = 6,
              
              # donut chart
              box(style = "margin: 0px; padding: 0 0 10px 0;",
                title = "Feedback Summary",
                width = 6,
                div(
                  plotOutput("feedbackSummaryDonut", height = 275)
                )
              ),
              
              # word cloud per college
              box(style = "margin: 0px; padding: 0 0 10px 0;",
                title = 'Word Cloud',
                width = 6,
                column(
                  12, align = "center",
                  div(
                    wordcloud2Output("wordCloudPC", height = 275, width = "auto")
                  )
                )
              ),
              
              # sentiment overtime 
              box(style = "margin: 0px !important; padding: 0px !important;",
                title = 'Feedback Overtime',
                width = 12,
                div(
                  plotlyOutput("feedbackOvertimePC", height = 315)
                )
              )
              
            )
          )
        )
        
      ),
      
      # tab item for about page
      tabItem(
        tabName = "about",
        div(style = "margin-top: 60px; margin-left: 10px;",
          fluidRow(
            box(
              width = 12,
              # authors
              div(style="text-align: center; margin-bottom: 30px; margin-top: 20px;",
                p(class="p-title",
                  "Authors"
                  )
              ),
              
              div(style="display: flex; justify-content: center;",
                
                div(style = "text-align: center;",
                  img(style="height: 150px", src="image1.png", align="center"),
                  p(class="p-name", "Dan Marlou A. Arevalo"),
                  p(class="p-email", "dmaarevalo00078@usep.edu.ph"),
                  p(class="p-role","BSCS - Student")
                ),
                
                # space between
                div(
                  style="width: 100px;"
                ), 
                
                div(style = "text-align: center;",
                  img(style="height: 150px", src="image2.png", align="center"),
                  p(class="p-name", "Ellen Kaye T. Isip"),
                  p(class="p-email", "ektisip01489@usep.edu.ph"),
                  p(class="p-role","BSCS - Student")
                )
              ),
              
              div(style="text-align: center; padding-right: 30px; padding-left: 30px; margin-top: 40px;",
                  p(class="p-content",
                    'The authors are Computer Science Major in Data Science students from the University of Southeastern Philippines - Obrero Campus. This project is a requirement for our learning evidence in CS226 - Data Analytics under Sir Jamal Kay Rogers.'
                  )
              ),
              
              div(style="text-align: center; margin-bottom: 20px; margin-top: 70px;",
                  p(class="p-title",
                    "Description"
                  )
              ),
              
              div(style="text-align: center; padding-right: 30px; padding-left: 30px;",
                p(class="p-content",
                  "The USeP Student Feedback Dashboard aims to provide the administration with an overview of all student feedback. The dashboard features two main tabs: the Dashboard Overview tab and the Feedback by College tab. In the Dashboard Overview tab, the administration can view the general sentiment of students from different colleges, classifying feedback as negative, positive, or neutral. The administration can also access visualizations of the top topics from the feedback through bar charts and word clouds. Additionally, they can track changes in positive, negative, and neutral feedback over time using time series analysis. The Dashboard Overview tab also displays the total number of sentiments per college. On the other hand, in the Feedback by College tab, the administration can view visualizations of the total feedback and its classification for a specific college and observe how feedback changes over time. The primary purpose of this dashboard is to enable the administration to collect and analyze student feedback, allowing them to address potential issues and improve their administration."
                )
              ),
              
              div(style="text-align: center; margin-bottom: 20px; margin-top: 70px;",
                  p(class="p-title",
                    "Process"
                  )
              ),
              
              div(style="text-align: center; padding-right: 30px; padding-left: 30px;",
                  p(class="p-content",
                    'Student feedback was collected via Google Forms and stored in a Google Sheet. The data then underwent text analysis and topic modeling, following guidelines from the book Text Mining with R by Julia Silge and David Robinson. Initially, the data was preprocessed through a comprehensive cleaning process, which included the removal of stopwords to ensure the accuracy and relevance of the analysis. The authors employed the "snowball" library for stopwords removal. For sentiment analysis, the authors utilized the "bing" lexicon, which categorizes words as either positive or negative. To classify the feedback, the number of positive and negative words was summed. If the sum is greater than zero, the feedback is classified as positive. If the sum is less than zero, the feedback is classified as negative. If the sum is equal to zero, the feedback is classified as neutral. Lastly, in the topic modeling phase, the Structural Topic Models (STM) package was utilized. This package was employed to identify and analyze prevalent topics within the feedbacks.'
                  )
              ),
              
              div(style="text-align: center; margin-bottom: 20px; margin-top: 70px;",
                  p(class="p-title",
                    "Limitations"
                  )
              ),
              
              div(style="text-align: center; padding-right: 30px; padding-left: 30px;",
                  p(class="p-content",
                    'Lexicon-based sentiment analysis encounters various challenges, including fluctuations in sentiment influenced by language, context, and domain. Furthermore, it may struggle to discern sarcasm, irony, negation, modifiers, and multi-word expressions or idioms, all of which can significantly affect the perceived sentiment. The "bing" lexicon utilized for analyzing feedback is limited to English word vocabularies. As a result, the dashboard can only classify feedback written in English. Moreover, it\'s important to note that the lexicon does not cover all words found in the English dictionary. In scenarios where stopwords have been removed, and the remaining words are not present in the bing dictionary, the classification of the feedback will be deemed INVALID. This approach is adopted to prevent bias in categorization. Considering these limitations, the sentiment analysis is not deemed to be 100% accurate; however, it still maintains a high level of accuracy.'
                  )
              ),
              
              div(style="text-align: center; margin-bottom: 20px; margin-top: 70px;",
                  p(class="p-title",
                    "References and Links"
                  )
              ),
              
              div(style="text-align: center; padding-right: 30px; padding-left: 30px;",
                  p(class="p-content",
                    "Book reference for Text Analysis: ",
                    tags$a("Text Mining with R - A Tidy Approach by Julia Silge & David Robinson", href = "https://www.tidytextmining.com/", target = "_blank")
                  ),
                  
                  p(class="p-content",
                    "Google Form Link for USeP Student Feedback: ",
                    tags$a("Feedback Form", href = "https://forms.gle/WhguD4VaHBfa7ThH6", target = "_blank")
                  ),
              ),
            )
          )
        )
        
      )
    )
  )
   
)


server <- function(input, output) {

  # dispaly nnumeric values dynamically
  output$totalResponses <- renderText({
    total_responses
  })
  
  output$percentPositive <- renderUI({
    HTML(paste0(percent_positive, "<span class='percent-sign'>%</span>"))
    
  })
  
  output$percentNegative <- renderUI({
    HTML(paste0(percent_negative, "<span class='percent-sign'>%</span>"))
  })
  
  output$percentNeutral <- renderUI({
    HTML(paste0(percent_neutral, "<span class='percent-sign'>%</span>"))
  })
  

  # overall table
  output$sentimentTable <- DT::renderDataTable(
    datatable(
    final_data %>% 
      mutate(Classification = overall_score) %>%
      select(Timestamp, Feedback, Classification) %>%
      mutate(Classification = ifelse(Classification == 'positive', "Positive", 
                                     ifelse(Classification == 'negative', "Negative",
                                            ifelse(Classification == 'neutral', "Neutral","Invalid")))), 
    options = list(scrollX = TRUE,
                   pageLength = 5,
                   lengthChange = FALSE,
                   scrollY = "397px",
                   scrollCollapse = TRUE
    ) 
    
    ) %>% formatStyle(
      "Classification",
      color = styleEqual(c("Positive", "Negative", "Neutral"), c("#139D50", "#D2222D", "#FEAC00")),
      fontWeight = styleEqual(c("Positive", "Negative", "Neutral"), c("600", "600", "600"))
    ) 
  )

  
  # top topics graph
  output$topTopicsPlot <- renderPlotly({
    top_topics_final %>%
      ggplot(aes(n, words)) +
      geom_col(fill ="#108CBA") + theme(axis.title.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        axis.text.x = element_text(size = 8),
                                        axis.text.y = element_text(size = 8)
                                       )
  })
  
  
  # word cloud 
  output$wordCloud <- renderWordcloud2({
    df_wc <- cleaned_text %>%
      count(word)
    wordcloud2a(df_wc, color='random-dark', fontFamily = 'Arial',
                size = 0.5, minRotation = -pi/2, maxRotation = -pi/2)
  })
  
  
  # feedback per college
  output$fbPerCollegePlot <- renderPlotly({
    perCollegeFinal %>%
      mutate(Sentiment = overall_score) %>%
      mutate(Sentiment = ifelse(Sentiment == 'positive', "Positive", 
                                ifelse(Sentiment == 'negative', "Negative",
                                       ifelse(Sentiment == 'neutral', "Neutral","Invalid")))) %>%
      filter(Sentiment %in% c("Positive", "Negative", "Neutral")) %>%
      ggplot(aes(CollegeAcronym, fill = Sentiment)) + 
      geom_bar() + labs(x = "College", y ="Count") +
      scale_fill_manual(values=c("Positive" = "#139D50", "Negative" = "#D2222D", "Neutral" = "#FEAC00")) +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_text(size = 10, color = "#333333"),
            legend.text = element_text(size = 9, color = "#333333"),
            )
  })
  
  
  # feedback overtime
  output$feedbackOvertime <- renderPlotly({
    month.name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    overall_line %>%
      ggplot(aes(x = month)) +
      geom_line(aes(y = positive, color = "Positive")) +
      geom_point(aes(y = positive, color = "Positive")) +
      geom_line(aes(y = negative, color = "Negative")) +
      geom_point(aes(y = negative, color = "Negative")) +
      geom_line(aes(y = neutral, color = "Neutral")) +
      geom_point(aes(y = neutral, color = "Neutral")) +
      scale_color_manual(values = c("Positive" = "#139D50", "Negative" = "#D2222D", "Neutral" = "#FEAC00")) +
      scale_x_continuous(breaks = 1:12, labels = month.name) +
      labs(x = "Month", y = "Count", color = "Sentiment") +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.title = element_text(size = 10, color = "#333333"),
            legend.text = element_text(size = 9, color = "#333333"),
      )
    
  })
  
  
  # change data set based on selected input 
  collegeSelected <- reactive({
    if(input$collegeChoice == "College of Applied Economics") {
      dataSelected <- caecData
    }
    else if(input$collegeChoice == "College of Arts and Sciences") {
      dataSelected <- casData
    }
    else if(input$collegeChoice == "College of Business and Administration") {
      dataSelected <- cbaData
    }
    else if(input$collegeChoice == "College of Education") {
      dataSelected <- cedData
    }
    else if(input$collegeChoice == "College of Engineering") {
      dataSelected <- coeData
    }
    else if(input$collegeChoice == "College of Information and Computing") {
      dataSelected <- cicData
    }
    else if(input$collegeChoice == "College of Technology") {
      dataSelected <- ctData
    }
    else {
      dataSelected <- final_data[FALSE,]
    }
    
    return(dataSelected)
    
  })
  
  # total responses per college
  output$totalResponsesPC <- renderText({
    data <- collegeSelected()
    nrow(data)
  })
  
  # percent positive per college
  output$percentPositivePC <- renderUI({
    data <- collegeSelected()
    data_positive <- data %>%
      filter(overall_score == 'positive')
    
    percentage_positive <- round((nrow(data_positive) * 100) / nrow(data), 0)
    HTML(paste0(ifelse(is.nan(percentage_positive), 0, percentage_positive), "<span class='percent-sign'>%</span>"))
    
  })
  
  # percent negative per college
  output$percentNegativePC <- renderUI({
    data <- collegeSelected()
    data_negative <- data %>%
      filter(overall_score == 'negative')
    
    percentage_negative <- round((nrow(data_negative) * 100) / nrow(data), 0)
    HTML(paste0(ifelse(is.nan(percentage_negative), 0, percentage_negative), "<span class='percent-sign'>%</span>"))
    
  })
  
  # percent neutral per college
  output$percentNeutralPC <- renderUI({
    data <- collegeSelected()
    data_neutral <- data %>%
      filter(overall_score == 'neutral')
    
    percentage_neutral <- round((nrow(data_neutral) * 100) / nrow(data), 0)
    HTML(paste0(ifelse(is.nan(percentage_neutral), 0, percentage_neutral), "<span class='percent-sign'>%</span>"))
    
  })
  
  
  # table per college
  output$sentimentTablePC <- DT::renderDataTable(
    datatable(
      collegeSelected() %>% 
        mutate(Classification = overall_score) %>%
        select(Timestamp, Feedback, Classification) %>%
        mutate(Classification = ifelse(Classification == 'positive', "Positive", 
                                       ifelse(Classification == 'negative', "Negative",
                                              ifelse(Classification == 'neutral', "Neutral","Invalid")))), 
      options = list(scrollX = TRUE,
                     pageLength = 5,
                     lengthChange = FALSE,
                     scrollY = "397px",
                     scrollCollapse = TRUE
      ) 
      
    ) %>% formatStyle(
      "Classification",
      color = styleEqual(c("Positive", "Negative", "Neutral"), c("#139D50", "#D2222D", "#FEAC00")),
      fontWeight = styleEqual(c("Positive", "Negative", "Neutral"), c("600", "600", "600"))
    ) 
  )
  
  # donut chart
  output$feedbackSummaryDonut <- renderPlot({
    data_pie <- collegeSelected()
    data_neg <- data_pie %>%
      filter(overall_score == 'negative')
    data_pos <- data_pie %>%
      filter(overall_score == 'positive')
    data_neut <- data_pie %>%
      filter(overall_score == 'neutral')
    
    total <- nrow(data_pie)
    
    pos <- nrow(data_pos)
    neg <- nrow(data_neg)
    neut <- nrow(data_neut)
    
    df <- data.frame(
      sentiment = c("positive", "negative", "neutral"),
      value = c(pos, neg, neut)
    )
    
    df <- df %>%
      filter(value > 0)  # Filter out categories with zero values
    
    p <- ggplot(df, aes(x=2, y=value, fill=sentiment)) +  # Use x=2 for the donut shape
      geom_col(width = 1) +  # Use geom_col for the bars
      coord_polar(theta="y", start=0) +
      scale_fill_manual(
        values = c("positive" = "#139D50", "negative" = "#D2222D", "neutral" = "#FEAC00"),
        labels = c("Positive", "Negative", "Neutral")  # Capitalize the first letter
      ) +
      theme_void() + 
      xlim(0.5, 2.5)  # Set x limits to create space for the hole in the middle
    
    # Add separators only if there's more than one category
    if (nrow(df) > 1) {
      p <- p + geom_col(width = 1, color = "white", size = 1.5)
    }
    
    # Add text labels for each segment
    p <- p + geom_text(
      aes(label = paste(round((value * 100) / total, 0),"%")),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 5,
      fontface = "bold"
    )
    
    # Customize legend and place it at the bottom
    p <- p + theme(
      legend.position = "bottom",
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 12, color ="#333333"),  # Customize legend text size
      
    )
    
    p
  })
  
  # word cloud per college
  output$wordCloudPC <- renderWordcloud2({
    data_wc_pc <- collegeSelected()
    if(nrow(data_wc_pc) != 0) {
      cleaned_text_wc_pc <- data_wc_pc %>%
        unnest_tokens(word, Feedback) %>%
        select(id_num, word) %>%
        anti_join(stopwords)
      
      data_wc_pc <- cleaned_text_wc_pc %>%
        count(word)
      
      wordcloud2a(data_wc_pc, color='random-dark', fontFamily = 'Arial',
                  size = 0.5, minRotation = -pi/2, maxRotation = -pi/2)
    }
        
  })
  
  # feedback overtime per college
  output$feedbackOvertimePC <- renderPlotly({
    data_overtime_pc <- collegeSelected()
    if(nrow(data_overtime_pc) != 0) {
      month.name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      
      
      data_overtime_pc$Timestamp <- mdy_hms(data_overtime_pc$Timestamp)
      
      df_pc <- data_overtime_pc %>%
        mutate(year = year(Timestamp),
               month = month(Timestamp),
               day = day(Timestamp),
               hour = hour(Timestamp),
               minute = minute(Timestamp),
               second = second(Timestamp)
        )
      
      
      positive_data_pc <- df_pc %>%
        filter(overall_score == 'positive')
      
      negative_data_pc <- df_pc %>%
        filter(overall_score == 'negative')
      
      neutral_data_pc <- df_pc %>%
        filter(overall_score == 'neutral')
      
      positive_line_pc <- positive_data_pc %>%
        group_by(month) %>%
        summarise(positive = n())
      
      negative_line_pc <- negative_data_pc %>%
        group_by(month) %>%
        summarise(negative = n())
      
      neutral_line_pc <- neutral_data_pc %>%
        group_by(month) %>%
        summarise(neutral = n())
      
      overall_line_pc <- positive_line_pc %>%
        full_join(negative_line_pc, by = 'month')
      
      overall_line_pc <- overall_line_pc %>%
        full_join(neutral_line_pc, by = 'month')
      
      overall_line_pc <- overall_line_pc %>%
        mutate(positive = replace_na(positive, 0))
      
      overall_line_pc <- overall_line_pc %>%
        mutate(negative = replace_na(negative, 0))
      
      overall_line_pc <- overall_line_pc %>%
        mutate(neutral = replace_na(neutral, 0))
      
      overall_line_pc %>%
        ggplot(aes(x = month)) +
        geom_line(aes(y = positive, color = "Positive")) +
        geom_point(aes(y = positive, color = "Positive")) +
        geom_line(aes(y = negative, color = "Negative")) +
        geom_point(aes(y = negative, color = "Negative")) +
        geom_line(aes(y = neutral, color = "Neutral")) +
        geom_point(aes(y = neutral, color = "Neutral")) +
        scale_color_manual(values = c("Positive" = "#139D50", "Negative" = "#D2222D", "Neutral" = "#FEAC00")) +
        scale_x_continuous(breaks = 1:12, labels = month.name) +
        labs(x = "Month", y = "Count", color = "Sentiment") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              legend.title = element_text(size = 10, color = "#333333"),
              legend.text = element_text(size = 9, color = "#333333"),
        ) 
    }
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
