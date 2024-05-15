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


# custom style using css
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
  head(10)


# per college data
perCollegeFinal <- final_data %>%
  mutate(CollegeAcronym = ifelse(College == 'College of Information and Computing', 'CIC',
                          ifelse(
                            College == 'College of Engineering', 'COE',
                            ifelse(
                              College == 'College of Education', 'CEd',
                              ifelse(
                                College == 'College of Business and Administration', 'CBA',
                                ifelse(
                                  College == 'College of Arts and Sciences', 'CAS',
                                  ifelse(
                                    College == 'College of Applied Economics', 'CAEc', 'CT'
                                  )
                                )
                              )
                            )
                          )
                          )
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
  left_join(negative_line, by = 'month')

overall_line <- overall_line %>%
  left_join(neutral_line, by = 'month')

overall_line <- overall_line %>%
  mutate(positive = replace_na(positive, 0))

overall_line <- overall_line %>%
  mutate(negative = replace_na(negative, 0))

overall_line <- overall_line %>%
  mutate(neutral = replace_na(neutral, 0))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    dashboardHeader(
      title = "USeP Student Feedback"
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overall Feedback", tabName = "overall"),
        menuItem("College Feedback", tabName = "perCollege"),
        menuItem("About", tabName = "about")
      )
    
    ),
    
    dashboardBody(
      
      # for user interface design
      tags$head(tags$style(css)),
      
      tags$script(HTML("$('body').addClass('fixed');")),
      
      # tab items
      # tab for overall feedback
      tabItems(
        tabItem(
          tabName = 'overall',
            
          fluidRow(
            column(
              width = 6,
              valueBoxOutput('totalResponses', width = 3),
              valueBoxOutput('percentPositive', width = 3),
              valueBoxOutput('percentNegative', width = 3),
              valueBoxOutput('percentNeutral', width = 3),
              # table
              box(
                width = 12,
                DT::dataTableOutput("sentimentTable")
              )
              
            ),
            
            box(
              title = 'Top Feedback Topics',
              width = 3,
              collapsible = TRUE,
              status = 'primary',
              plotlyOutput("topTopicsPlot", height = 250)
      
            ),
            
            box(
              title = 'Word Cloud',
              width = 3,
              status = 'primary',
              plotOutput("wordCloud", height = 250)
            ),
            
            box(
              title = "Feedback per College",
              width = 6,
              status = 'primary',
              plotlyOutput("fbPerCollegePlot", height = 250)
            ),
            
            box(
              title = 'Feedback Overtime',
              width = 6,
              status = 'primary',
              plotlyOutput("feedbackOvertime", height = 250)
            )
            
          ),
          
            
   
          ),
          

        
        tabItem(
          tabName = 'perCollege',
          h2('test per college')
        ),
        
        tabItem(
          tabName = 'about',
          h2('test about')
        )
      )
      
      
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # display dynamic content
  output$totalResponses <- renderValueBox({
    valueBox(
      total_responses,
      'Feedbacks',
      color = 'blue'
    )
  })
  
  output$percentPositive <- renderValueBox({
    valueBox(
      paste0(percent_positive, "%"),
      'Positive',
      color = 'blue'
    )
  })
  
  output$percentNegative<- renderValueBox({
    valueBox(
      paste0(percent_negative, "%"),
      'Negative',
      color = 'blue'
    )
  })
  
  output$percentNeutral<- renderValueBox({
    valueBox(
      paste0(percent_neutral, "%"),
      'Neutral',
      color = 'blue'
    )
  })
  
  # top topics graph
  output$topTopicsPlot <- renderPlotly({
    top_topics_final %>%
      ggplot(aes(n, words)) +
      geom_col(fill = 'blue')
  })
  
  # word cloud 
  output$wordCloud <- renderPlot({
    cleaned_text %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 20))
  })
  
  # table
  output$sentimentTable <- DT::renderDataTable(
    final_data %>% 
      mutate(Classification = overall_score) %>%
      select(Timestamp, Feedback, Classification), 
    options = list(scrollX = TRUE,
                   pageLength = 5,
                   lengthChange = FALSE
                   )
    
  )
  
  # feedback per college
  output$fbPerCollegePlot <- renderPlotly({
    perCollegeFinal %>%
      ggplot(aes(CollegeAcronym, fill = overall_score)) + 
      geom_bar()
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
      scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "yellow")) +
      scale_x_continuous(breaks = 1:12, labels = month.name) +
      labs(x = "Month", y = "Count", color = "Sentiment")
    
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
