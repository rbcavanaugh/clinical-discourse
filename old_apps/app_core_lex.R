
#### Packages ####
library(tibble)
library(tibble)
library(scales)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(plotly)
library(textstem)
library(here)
library(DT)
library(truncnorm)
library(shinyjs)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

#library(bs4Dash)

#### Setup files ####
# need to give a way for clinicians to take away or add points manually to adjust the score...

broken_window=tibble(c("a","and","ball","be","boy","break","go","he","in",
                       "it","kick","lamp","look","of","out","over","play","sit",
                       "soccer","the","through","to","up","window"))

refused_umbrella=tibble(c("a", 'and', 'back', 'be', 'boy', 'do', 'get', 'go', 'have','he',
                          'home', 'i', 'in', 'it', 'little', 'mother', 'mom', 'need', 'not', 'out',
                          'rain', 'say', 'school', 'she', 'so', 'start', 'take', 'that', 'the',
                          'then', 'to', 'umbrella', 'walk', 'wet', 'with', 'you'))

cat_rescue=tibble(c("a","and","bark","be","call","cat","climb","come","department", 'dad',
                    "dog","down","father","fire","get","girl","go","have","he",
                    "in","ladder","little","tree","not","up", 'out', 'with', 'she', 'fireman',
                    'so', 'stick', 'the', 'their', 'there', 'to'))

cinderella = tibble(c('a', 'after', 'all', 'and', 'as', 'at', 'away', 'back','ball',
                      'be', 'beautiful', 'because', 'but', 'by', 'cinderella', 'clock', 'come',
                      'could', 'dance','daughter', 'do', 'dress', 'ever', 'fairy', 'father',
                      'dad', 'find', 'fit', 'foot', 'for', 'get', 'girl', 'glass', 'go',
                      'godmother', 'happy', 'have', 'he', 'home', 'horse', 'house', 'i', 'in',
                      'into', 'it', 'know', 'leave', 'like', 'little', 'live', 'look', 'lose',
                      'make', 'marry', 'midnight', 'mother', 'mom', 'mouse', 'not', 'of',
                      'off', 'on', 'one', 'out', 'prince', 'pumpkin', 'run', 'say', 'she',
                      'shoe', 'sister', 'slipper', 'so', 'strike', 'take', 'tell', 'that',
                      'the', 'then', 'there', 'they', 'this', 'time', 'to', 'try',
                      'turn', 'two', 'up', 'very', 'want', 'well', 'when', 'who', 'will', 'with'))

sandwich = tibble(c("a","and","bread","butter","get","it","jelly","knife","of", 'on',
                    "one","other","out","peanut","piece","put","slice","spread","take",
                    "the","then","to","together","two","you"))

# gdc_picnic <- read.csv(here('data', 'gdc_picnic_cleaned.csv'))[,2:4] %>%
#   as_tibble()
# 
# list2env(split(gdc_picnic, gdc_picnic$story),envir=.GlobalEnv)

corpus <- list(broken_window, refused_umbrella, cat_rescue, cinderella, sandwich)
#gdc, picnic)


core_lex <- function(text, stimulus, age_input){
   stim = corpus[[stimulus]]
   if(stimulus > 5){
      stim <- stim %>% filter(age == floor(age_input/10)*10) %>%
         select(lexeme)
   }
   colnames(stim) = 'target_lemma'
   text = tibble(text)
   colnames(text) = 'text'
   text <- tibble(unnest_tokens(text, word, text)) %>% distinct()
   text$lemma <- lemmatize_words(text$word)
   colnames(text) = c('token', 'produced_lemma')
   
   return_list <- list()
   
   return_list$match <- stim %>%
      left_join(text, by = c('target_lemma' = 'produced_lemma')) %>%
      mutate(produced = ifelse(!is.na(token), 1,0)) %>%
      select(target_lemma, produced, token)
   
   return_list$extra <- text %>%
      anti_join(stim, by = c('produced_lemma' = 'target_lemma'))
   
   
   return(return_list)
}


#### body ####
body <- dashboardBody(
   tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   ),
   tabItems(
      tabItem(tabName = "cl",
              
   fluidRow(
      column(width = 3,
             boxPlus(solidHeader = T, status = 'primary', title = "Step 1: Enter Transcript",
                 closable = F, 
                 width = NULL,
                 radioButtons("stim", "Elicitation Task:",
                              c("Broken Window" = 'broken_window',
                                "Cat Rescue" = 'cat_rescue',
                                "Refused Umbrella" = 'refused_umbrella',
                                "Cinderella" = 'cinderella',
                                "Sandwich" = 'sandwich')
                 ),
                 textAreaInput("transcr",
                               "Transcript:",
                               value = "Young boy is practicing playing soccer. Kicking the ball up and keeping it in the air. He miskicks. and and it fall goes and breaks the window of his house. of the living room actually. and bounces into the living room knocking a lamp over where his father is sitting. the father picks up the soccer ball. Looks out the window. And calls for the little boy to come and explain.",
                               height = '250px'),
                 sliderInput("time", "Time on task (seconds)", value= 120,
                             min = 1, max = 1200, step = 10),
                 sliderInput("adj", "Score Adjustment:", value= 0,
                             min = -10, max = 10, step = 1)
                 )
      ),

      column(width = 5,
             boxPlus(
                title = "Step 2: Check Scoring",
                closable = F, 
                tags$ol(
                   tags$li("Check target lexemes = tokens, following published rules."), 
                   tags$li("Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)"), 
                   tags$li("+1 point to Cinderella score if the possessive [ 's ] is used"),
                   tags$li("Count any variation of mom/mother or dad/father.")
                ),
                solidHeader = T,
                status = "primary",
                width= NULL,
                br(),
               DTOutput("table")
                # solidHeader = T, status = "primary",
                # title = "Check Matching",
                # width = NULL,
             )
      ),
      column(width = 4, 
             fluidRow(
                boxPlus(title = "Step 3: Review Core Lexicon Analysis Results",
                    width = NULL,
                    closable = F, 
                    solidHeader = T, status = "primary",
                tabBox(
                  side = "right",
                   width = NULL, 
                  selected = "Results Summary",
                  tabPanel("Percentile Scoring", 
                           plotOutput("plot1")),
                   tabPanel("Results Summary", 
                            valueBoxOutput("progressBox", width = NULL),
                            valueBoxOutput("progressBox2", width = NULL))

                )
                ),
                boxPlus(
                   title = "References",
                   closable = F, 
                   solidHeader = T,
                   status = "primary",
                   collapsible = T,
                   collapsed = F,
                   htmlOutput("ref"),
                   width = NULL
                )
             )
      )
   )
),
tabItem(tabName = 'mca'),
tabItem(tabName = 'ld')
)
)

#### header ####
header <- dashboardHeaderPlus(title = "Aphasia Discourse Analysis",
                          tags$li(tags$a(target = "_blank", href = 'https://robcavanaugh.com',"Rob Cavanaugh"),  class= 'dropdown'),
                          tags$li(tags$a(target = "_blank", href = 'https://www.marquette.edu/speech-pathology-audiology/building-rehabilitation-advances-in-neurscience-lab.php', "Sarah Grace Dalton"),  class= 'dropdown'),
                          tags$li(tags$a(target = "_blank", href = 'https://github.com/rbcavanaugh',tags$img(icon("github"))), class= 'dropdown')
)



#### sidebar ####
sidebar <- dashboardSidebar(collapsed = F,
                 sidebarMenu(
                    menuItem("Core Lexicon analysis", tabName = "cl", icon = icon("apple-alt")),
                    menuItem("Main Concept Analysis", tabName = "mca", icon = icon("lightbulb")),
                    menuItem("Lexical Diversity", tabName = "ld", icon = icon("book"))
                    
                 )
)
# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
   # Application title
   header,
   # Sidebar with a slider input for number of bins 
   sidebar,
   body
   )



#### server ####
server <- function(input, output) {
   
   selectedData <- reactive({
      
      task = case_when(
         input$stim == 'broken_window' ~ 1,
         input$stim == 'refused_umbrella' ~ 2,
         input$stim == 'cat_rescue' ~ 3,
         input$stim == 'cinderella' ~ 4,
         input$stim == 'sandwich' ~ 5,
         input$stim == 'gdc' ~ 6,
         input$stim == 'picnic' ~ 7
      )
      

      norm_mean = case_when(
         input$stim == 'broken_window' ~ list(c(19, 2.9, 12, 23)),
         input$stim == 'refused_umbrella' ~ list(c(26.5, 3.1, 17, 33)),
         input$stim == 'cat_rescue' ~ list(c(26.3, 3.3, 16, 33)),
         input$stim == 'cinderella' ~ list(c(69.8, 15.5, 6, 90)),
         input$stim == 'sandwich' ~ list(c(19, 2.7, 14, 23))
      )
      
      aphasia_mean = case_when(
         input$stim == 'broken_window' ~ list(c(12, 4.9, 0, 22)),
         input$stim == 'refused_umbrella' ~ list(c(17.3, 7.7, 0, 34)),
         input$stim == 'cat_rescue' ~ list(c(16.8, 7.0, 0, 31)),
         input$stim == 'cinderella' ~ list(c(37.5, 19.4, 1, 82)),
         input$stim == 'sandwich' ~ list(c(11.3, 5.4, 0, 23))
      )
      
      # EFFICIENCY ####
      
      norm_mean_eff = case_when(
         input$stim == 'broken_window' ~ list(c(34.8, 13.5, 6.7, 72.9)),
         input$stim == 'refused_umbrella' ~ list(c(41.3, 14.5, 16.6, 98.2)),
         input$stim == 'cat_rescue' ~ list(c(39.8, 14.3, 13.3, 100)),
         input$stim == 'cinderella' ~ list(c(24.7, 10.0,3.4, 72.0)),
         input$stim == 'sandwich' ~ list(c(40.5, 16.6, 8.6, 114.0))
      )
      
      aphasia_mean_eff = case_when(
         input$stim == 'broken_window' ~ list(c(18.5, 13.3, 0, 84.0)),
         input$stim == 'refused_umbrella' ~ list(c(19.7, 13.4, 0, 67.5)),
         input$stim == 'cat_rescue' ~ list(c(16.9, 12.3, 0, 93.8)),
         input$stim == 'cinderella' ~ list(c(14.1, 9.1, 0.4, 62.5)),
         input$stim == 'sandwich' ~ list(c(24.1,20.1, 0, 156.0))
      )
      
      # Max 
      max_val = case_when(
         input$stim == 'broken_window' ~ 24,
         input$stim == 'refused_umbrella' ~ 35,
         input$stim == 'cat_rescue' ~ 34,
         input$stim == 'cinderella' ~ 94,
         input$stim == 'sandwich' ~ 25
      )
      
      df <- core_lex(input$transcr, task, input$age)
      matches <- df$match
      score_num = sum(matches$produced) + input$adj
      score_eff = score_num/(input$time/60)
      
      dist1 = truncnorm::rtruncnorm(10000,
                                    mean = norm_mean[[1]][[1]],
                                    sd = norm_mean[[1]][[2]],
                                    a = norm_mean[[1]][[3]],
                                    b = norm_mean[[1]][[4]])
      percentile1 = label_percent()(ecdf(dist1)(22))
      
      dist2 = truncnorm::rtruncnorm(10000,
                                    mean = aphasia_mean[[1]][[1]],
                                    sd = aphasia_mean[[1]][[2]],
                                    a = aphasia_mean[[1]][[3]],
                                    b = aphasia_mean[[1]][[4]])
      percentile2 = label_percent()(ecdf(dist2)(score_num))
      
      dist3 = truncnorm::rtruncnorm(10000,
                                    mean = norm_mean_eff[[1]][[1]],
                                    sd = norm_mean_eff[[1]][[2]],
                                    a = norm_mean_eff[[1]][[3]],
                                    b = norm_mean_eff[[1]][[4]])
      percentile3 = label_percent()(ecdf(dist3)(score_eff))
      
      dist4 = truncnorm::rtruncnorm(10000,
                                    mean = aphasia_mean_eff[[1]][[1]],
                                    sd = aphasia_mean_eff[[1]][[2]],
                                    a = aphasia_mean_eff[[1]][[3]],
                                    b = aphasia_mean_eff[[1]][[4]])
      percentile4 = label_percent()(ecdf(dist4)(score_eff))
      
      score = tibble(
         Metric = c('Production', 'Efficiency'),
         Score = c(paste0(round(score_num,0),' core words'),
                   paste0(round(score_eff, 1), ' core words/min')),
         ControlPercentile =  c(percentile1, percentile3),
         AphasiaPercentile = c(percentile2, percentile4)
      )
      
      score <- score %>%
         mutate(
            ControlPercentile = ifelse(Metric != 'Production', ControlPercentile,
                                       ifelse(score_num > max_val,
                                              'exceeded max score',
                                              ControlPercentile)
            ),
            AphasiaPercentile = ifelse(Metric != 'Production', AphasiaPercentile,
                                       ifelse(score_num > max_val,
                                              'exceeded max score',
                                              AphasiaPercentile)
            )
         )
      
      colnames(score) <- c('Metric', 'Score', 'Control Percentile', 'Aphasia Percentile')
      
      dists <- tibble(
         dist1 = dist1,
         dist2 = dist2, 
         dist3 = dist3,
         dist4 = dist4
      ) %>%
         pivot_longer(cols = 1:4, names_to = 'dist', values_to = 'val')
      
      core_lex_data <- list()
      core_lex_data[["score"]] = score
      core_lex_data[["dist"]] = dists
      core_lex_data[["scores"]] = c(score_num, score_eff)
      
      return(core_lex_data) # make this a list with the data for the histograms too. 
      
   })
   
   selectedData2 <- reactive({
      task = case_when(
         input$stim == 'broken_window' ~ 1,
         input$stim == 'refused_umbrella' ~ 2,
         input$stim == 'cat_rescue' ~ 3,
         input$stim == 'cinderella' ~ 4,
         input$stim == 'sandwich' ~ 5,
         input$stim == 'gdc' ~ 6,
         input$stim == 'picnic' ~ 7
      )
      df <- core_lex(input$transcr, task, input$age)
      options = list(show = 10)
      table = df$match %>%
         mutate(produced = ifelse(produced == 1, 'yes', 'no')) %>%
         rename('Target Lexeme' = target_lemma,
                'Lexeme Produced?' = produced,
                'Token' = token)
      return(table)
   })
   
   output$table <- renderDT({
                     table = selectedData2()
                  })
   
   output$results <- renderTable({
      score = selectedData()[[1]]
      score
   })
   
   output$plot1 <- renderPlot({
      selectedData()[[2]] %>%
         mutate(Cohort = ifelse(dist == 'dist1' | dist == 'dist3', 'control', 'aphasia'),
                met = ifelse(dist == 'dist1' | dist == 'dist2', 'Production', 'Efficiency'),
                met = factor(met, levels = c('Production', 'Efficiency'))
         ) %>%
         ggplot(aes(x = val, color = Cohort, fill = Cohort)) +
         geom_density(alpha = .3) +
         geom_vline(data = data.frame(xint=selectedData()[[3]][[1]],met="Production"), 
                    aes(xintercept = xint)) +
         geom_vline(data = data.frame(xint=selectedData()[[3]][[2]],met="Efficiency"), 
                    aes(xintercept = xint)) +
         facet_wrap(.~met, scales = 'free', ) +
         theme_grey(base_size = 16) +
         theme(legend.position = 'bottom',
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               legend.margin=margin(c(.5, .5, .5, .5))) +
         labs(x = 'Score')
   })
   
   output$progressBox <- renderValueBox({
      valueBox(
         paste0(selectedData()[["scores"]][1], " core words"),
         subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][1,4], " | Control Percentile: ", selectedData()[["score"]][1,3]),
         icon = icon("chart-bar"),
         color = "navy"
         )
   })
   
   output$progressBox2 <- renderValueBox({
      valueBox(
         paste0(selectedData()[["scores"]][2], " core words/min"),
         subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][2,4], " | Control Percentile: ", selectedData()[["score"]][2,3]),
         icon = icon("stopwatch-20"),
         color = "navy"
         )
   })
   
   output$ref <- renderUI({
    ref1 <- 'Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers.'
    ref2 <- 'Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938.'
    ref3 <- 'Kim, H., & Wright, H. H. (2020). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers.'
    HTML(paste(ref1, ref2, ref3, sep = "<br/><br/>"))
      
   })
   
   
}

##### Run the application ####
shinyApp(ui = ui, server = server )

# runGist("3f1de92df0a0bb59688f609e6637d2f1")


#         