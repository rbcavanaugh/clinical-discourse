

library(shiny)
library(here)
library(dplyr)
library(tibble)
library(scales)
library(ggplot2)
library(patchwork)
library(tidyr)
library(textstem)
library(tidytext)
library(DT)
library(truncnorm)
library(koRpus)
set.kRp.env(lang="en")
koRpus.lang.en::lang.support.en()
library(waiter)
library(shinythemes)


waiter_preloader(html = spin_dots(), color = "#f0f0f0")

# load info 
source(here('R', 'core_lex.R'))


# Define UI for application that draws a histogram
ui <- 
    tagList(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        navbarPage(
    theme = shinytheme("flatly"),
    fluid = T, 
    title = div("Aphasia Discourse Analysis",
                  a(id = "img-id",
                  href = "https://github.com/rbcavanaugh/clinical-discourse",
                  icon("github")
                ),
                a(id = "rc",
                  href = "https://github.com/rbcavanaugh/clinical-discourse",
                  "Rob Cavanaugh"
                ),
                a(id = "sg",
                  href = "https://github.com/rbcavanaugh/clinical-discourse",
                  "Sarah Grace Dalton"
                )
    ),

                 
                 tabPanel("Core Lexicon",
                              sidebarLayout(
                                  sidebarPanel(width = 3,
                                      selectInput("stim", h5("Select Stimulus"),
                                                  c("Broken Window" = 'broken_window',
                                                    "Cat Rescue" = 'cat_rescue',
                                                    "Refused Umbrella" = 'refused_umbrella',
                                                    "Cinderella" = 'cinderella',
                                                    "Sandwich" = 'sandwich'),
                                                  selected = "broken_window",
                                                  # "Good Dog Carl" = 'gdc',
                                                  #"Picnic" = 'picnic'),
                                      ),
                                      
                                      textAreaInput("transcr",
                                                    h5("Enter Transcript"),
                                                    value = transcriptDefault,
                                                    height = '300px'),
                                      
                                      
                                      numericInput("time", "Time on task (seconds)", value= 120,
                                                   min = 1, max = 1200, step = 1),
                                      
                                      sliderInput("adj", "Score Adjustment:", value= 0,
                                                  min = -10, max = 10, step = 1),
                                      
                                      actionButton("button", "Send Feedback", style = 'float:right;')
                                  ),
                              mainPanel(width = 9,
                                  fluidRow(
                                  column(width = 6,
                                         tabsetPanel(
                                             tabPanel("Check Scoring", 
                                                      br(),
                                                      p("Instructions"),
                                                      DTOutput("table_cl")), 
                                             tabPanel("Detailed Instructions", textOutput("scoring_cl"))
                                         )
                                  ),
                                  column(width = 6,
                                         tableOutput("results_cl"),
                                         plotOutput("plot_cl", height = '300px'),
                                         p(refsCl)
                                         )
                                         )
                              )
                              
                     )
                 ),
                 
                 tabPanel("Main Concept & Sequencing",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("stimMC", h5("Select Stimulus"),
                                              c("Broken Window" = 'broken_window',
                                                "Cat Rescue" = 'cat_rescue',
                                                "Refused Umbrella" = 'refused_umbrella',
                                                "Cinderella" = 'cinderella',
                                                "Sandwich" = 'sandwich'),
                                              selected = "broken_window"
                                  ),
                                  
                                  textAreaInput("transcrMC",
                                                h5("Enter Transcript"),
                                                value = transcriptDefault,
                                                height = '300px'),
                                  
                                  actionButton("buttonMC", "Send Feedback", style = 'float:right;')
                              ),
                              mainPanel(
                                  fluidRow(
                                    column(width = 6,
                                         tabsetPanel(
                                             tabPanel("Check Scoring", 
                                                    p("Instructions")),
                                                    #DTOutput("table_cl")), 
                                                    tabPanel("Detailed Instructions",
                                                             textOutput("scoring_mc"))
                                         )
                                      ),
                                    column(width = 6,
                                           #tableOutput("results_cl"),
                                           #plotOutput("plot_cl"),
                                           p("References")
                                    )
                                  )
                              )
                          )
                 ),

                 tabPanel("Lexical Diversity",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("stimLD", h5("Select Stimulus"),
                                              c("Broken Window" = 'broken_window',
                                                "Cat Rescue" = 'cat_rescue',
                                                "Refused Umbrella" = 'refused_umbrella',
                                                "Cinderella" = 'cinderella',
                                                "Sandwich" = 'sandwich'),
                                              selected = "broken_window"

                                  ),
                                  
                                  textAreaInput("transcrLD",
                                                h5("Enter Transcript"),
                                                value = transcriptDefault,
                                                height = '300px'),
                                  
                                  sliderInput("mattr_w", "MATTR WINDOW:", value = 5, min = 5, max = 50),
                                  
                                  actionButton("buttonLD", "Send Feedback", style = 'float:right;')
                              ),
                              mainPanel(
                                  fluidRow(
                                      column(width = 6,
                                             tabsetPanel(
                                                 tabPanel("Check Scoring", 
                                                          p("Instructions")),
                                                         # DTOutput("table_cl")), 
                                                 tabPanel("Detailed Instructions",
                                                          textOutput("scoring_ld"))
                                             )
                                      
                                      ),
                                      column(width = 6,
                                             tableOutput("resultsLD"),
                                             #plotOutput("plot_cl"),
                                             p("References")
                                      )
                                  )
                              )
                          )
                 )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ###### Core Lex ####################################
    
    observeEvent(input$button, {
        showModal(
            modalDialog(
                title = "Feedback",
                tags$iframe(src = 'https://docs.google.com/forms/d/e/1FAIpQLSfrae3ucppQC_Hy2udxj5_xZwRqbkHwTzUX6PQEnpUdahAb4g/viewform?usp=sf_link',
                            width = '100%',
                            height = 500,
                            frameborder = 0,
                            marginheight = 0),
                easyClose = TRUE,
                size = "l"
            ))
    })
    
    output$table_cl <- renderDT({
        table = selectedData2()
        
    },options = list(dom = "ftp"))
    
    ### Check Scoring
    output$scoring_cl <- renderText({
   txt<-  "1.  Check that target lexemes match tokens, following core lexicon rules.
    2.  Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)
    3.  Add 1 point to the Cinderella passage if the possessive [ 's ] is used
4.  Count any variation of mom/mother or dad/father."
   txt
    })
    
    ### Check Scoring
    output$scoring_ld <- renderText({
        txt<-  "1.  Check that target lexemes match tokens, following core lexicon rules.
    2.  Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)
    3.  Add 1 point to the Cinderella passage if the possessive [ 's ] is used
4.  Count any variation of mom/mother or dad/father."
        txt
    })
    
    ### Check Scoring
    output$scoring_mc <- renderText({
        txt<-  "1.  Check that target lexemes match tokens, following core lexicon rules.
    2.  Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)
    3.  Add 1 point to the Cinderella passage if the possessive [ 's ] is used
4.  Count any variation of mom/mother or dad/father."
        txt
    })
    
   output$results_cl <- renderTable({
        score = selectedData()[[1]]
        score
    })
     
    output$plot_cl <- renderPlot({
        prod <- selectedData()[[2]] %>%
            mutate(Cohort = ifelse(dist == 'dist1' | dist == 'dist3', 'control', 'aphasia'),
                   met = factor(ifelse(dist == 'dist1' | dist == 'dist2', 'Production', 'Efficiency'),
                                levels = c('Production', 'Efficiency'))
            ) %>%
            filter(met == "Production") %>%
            ggplot(aes(x = val, color = Cohort, fill = Cohort)) +
            geom_density(alpha = .3) +
            geom_vline(data = data.frame(xint=selectedData()[[3]][[1]],met="Production"), 
                       aes(xintercept = xint), linetype = "dashed", size = 1) +
            theme_grey(base_size = 14) +
            theme(#panel.background = element_rect(fill = "transparent"),
                legend.position = 'bottom',
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5)) +
            labs(title = "Core words")
        
        eff <- selectedData()[[2]] %>%
            mutate(Cohort = ifelse(dist == 'dist1' | dist == 'dist3', 'control', 'aphasia'),
                   met = factor(ifelse(dist == 'dist1' | dist == 'dist2', 'Production', 'Efficiency'),
                                levels = c('Production', 'Efficiency'))
            ) %>%
            filter(met == "Efficiency") %>%
            ggplot(aes(x = val, color = Cohort, fill = Cohort)) +
            geom_density(alpha = .3) +
            geom_vline(data = data.frame(xint=selectedData()[[3]][[2]],met="Efficiency"), 
                       aes(xintercept = xint), linetype = "dashed", size = 1) +
            theme_grey(base_size = 14) +
            theme(#panel.background = element_rect(fill = "transparent"),
                legend.position = 'bottom',
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5)) +
            labs(title = "Core words / min")
        
        prod + eff + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
        
    })
    
    
    output$resultsLD <- renderTable({
        
        window = as.numeric(input$mattr_w)
        # save input as a variable
        transcript <- as.character(input$transcrLD)
        wim <- round(qdap::diversity(as.character(transcript))$shannon,2)
        m <- round(MATTR(tokenize(as.character(transcript), lang = "en", format = 'obj'), window = window)@MATTR$MATTR,2)
        ttr <- round(TTR(tokenize(as.character(transcript), lang = "en", format = 'obj'))@TTR,2)
        df <- tibble(
            `Moving Average TTR` = m,
            TTR = ttr,
            `Word Information Measure` = wim
        )
        
        return(df)
    })
    
    
    
    
    # Reactive that returns the whole dataset if there is no brush
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
        
        # ACCURACY ####
        
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
