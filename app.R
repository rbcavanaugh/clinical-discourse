
############### load stuff ################
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
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(rhandsontable)
# load info 
source(here('R', 'core_lex.R'))

###################### UI ###################
ui <- 
    tagList(
        #setup
        use_waiter(),
        waiter_preloader(html = spin_dots(), color = "#2c3e50"), #html = spin_dots(), color = "#f0f0f0"
        useShinydashboard(),
        tags$head(
            tags$link(rel = "shortcut icon", href = "favicon.png", type="image/png"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(src="javascript.js"),
            tags$title("Aphasia Discourse")
        ),
        #app
        navbarPage(
            theme = bslib::bs_theme(secondary = "#55a998", success = "#55a998", 
                                    base_font = font_google("Open Sans"), `enable-gradients` = TRUE, 
                                    `enable-shadows` = TRUE, bootswatch = "flatly"),
            fluid = T,collapsible = T, 
            #windowTitle = "Aphasia Discourse",
            title = div("Aphasia Discourse Analysis",
                          tags$a(id = "img-id",
                          href = "https://github.com/rbcavanaugh/clinical-discourse",
                          icon("github")
                        ),
                        tags$a(id = "rc",
                          href = "https://www.marquette.edu/speech-pathology-audiology/building-rehabilitation-advances-in-neurscience-lab.php",
                          "Sarah Grace Dalton"
                        ),
                        tags$a(id = "sg",
                          href = "https://robcavanaugh.com",
                          "Rob Cavanaugh"
                        )
            ),

             ########### core lex ##########   
                 tabPanel("Core Lexicon",
                          
                          sidebarLayout(
                                  sidebarPanel(width = 3,
                                      selectInput("stim", "Select Stimulus",
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
                                                    "Enter Transcript",
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
                                                      tags$ol(
                                                          tags$li("Check that target lexemes match tokens, following core lexicon rules."),
                                                          tags$li("Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)"),
                                                          tags$li("Add 1 point to the Cinderella passage if the possessive [ 's ] is used"),
                                                          tags$li("Count any variation of mom/mother or dad/father.")
                                                      ),
                                                      DTOutput("table_cl")), 
                                             tabPanel("Detailed Instructions", textOutput("scoring_cl"))
                                         )
                                  ),
                                  column(width = 6,
                                         tabsetPanel(
                                            tabPanel("Results",br(),
                                                valueBoxOutput("results_cl1", width = NULL),
                                                valueBoxOutput("results_cl2", width = NULL),
                                                box(width = NULL,
                                                plotOutput("plot_cl", height = '300px')
                                                )
                                            ),
                                            tabPanel("References",
                                                     br(),
                                                 tags$ol(
                                                     tags$li("Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers."), br(),
                                                     tags$li("Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938."),br(),
                                                     tags$li("Kim, H., & Wright, H. H. (2020, January). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers."),br(),
                                                     tags$li("Silge J, Robinson D (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R. JOSS, 1(3). doi: 10.21105/joss.00037")
                                                 )
                                         )
                                         )
                                         )
                              )
                              )
                              
                     )
                 ),
###### main concept ######
                 tabPanel("Main Concept & Sequencing",
                          sidebarLayout(
                              sidebarPanel(width = 3,
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
                                                value = transcriptDefault,# transcriptDefault,
                                                height = '300px'),

                                  actionButton("buttonMC", "Send Feedback")
                              ),
                              mainPanel(width = 9,
                                  fluidRow(
                                    column(width = 7,
                                         tabsetPanel(
                                             tabPanel("Check Scoring", br(),
                                                    box(width = NULL,
                                                        div(id='hottable', 
                                                        rHandsontableOutput("hotable1")
                                                        )
                                                        #DT::DTOutput('foo')
                                                    )
                                                    ), 
                                                    tabPanel("Detailed Instructions",
                                                             textOutput("scoring_mc"))
                                         )
                                      ),
                                    column(width = 5,
                                           tabsetPanel(
                                               tabPanel("Instructions", br(),
                                                        tags$ol(
                                                            tags$li("We can use this area to provide a list of the main concepts and scoring info"),
                                                            tags$li("This content can change based on which task is selected in the left menu. "),
                                                            tags$li("If you click on results, right now I've shown the kind of data we can track from this table. As the transcript changes, the table to the left and the results will also change."),
                                                            tags$li("Below I've done a crude calculation of the number correct for each concept (not sure if that matters) and the order based on when the first concept was presented (Regardless of accuracy right now, though that can change)."),
                                                            tags$li("Based on certain rules or potentially some analyses of existing data, we could potentially use a model to pre-set guesses for each sentence about the concept or the accuracy. ")
                                                        )
                                                   
                                               ),
                                               tabPanel("Results",br(),
                                                        DTOutput('table_out'),br(),
                                                        tableOutput('results_ht_out')
                                                        #valueBoxOutput("results_ld1", width = NULL),
                                                        #valueBoxOutput("results_ld2", width = NULL),
                                                        #valueBoxOutput("results_ld3", width = NULL),
                                                        # box(width = NULL,
                                                        #     plotOutput("plot_cl", height = '300px')
                                                        # )
                                               ),
                                               tabPanel("References",
                                                        br(),
                                                        tags$ol(
                                                            tags$li("Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers."), br(),
                                                            tags$li("Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938."),br(),
                                                            tags$li("Kim, H., & Wright, H. H. (2020, January). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers."),br(),
                                                            tags$li("Silge J, Robinson D (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R. JOSS, 1(3). doi: 10.21105/joss.00037")
                                                        )
                                               )
                                           )
                                    )
                                  )
                              )
                          )
                 ),
######### lexical diversity ########
                 tabPanel("Lexical Diversity",
                          sidebarLayout(
                              sidebarPanel(width = 3,
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
                              mainPanel(width = 9,
                                  fluidRow(
                                      column(width = 6,
                                             tabsetPanel(
                                                 tabPanel("Check Scoring", 
                                                          h4("Instructions"),
                                                          tags$ol(
                                                              tags$li("Check that target lexemes match tokens, following core lexicon rules."),
                                                              tags$li("Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)"),
                                                              tags$li("Add 1 point to the Cinderella passage if the possessive [ 's ] is used"),
                                                              tags$li("Count any variation of mom/mother or dad/father.")
                                                          )),
                                                         # DTOutput("table_cl")), 
                                                 tabPanel("Detailed Instructions",
                                                          textOutput("scoring_ld"))
                                             )
                                      
                                      ),
                                      column(width = 6,
                                             tabsetPanel(
                                                 tabPanel("Results",br(),
                                                          valueBoxOutput("results_ld1", width = NULL),
                                                          valueBoxOutput("results_ld2", width = NULL),
                                                          valueBoxOutput("results_ld3", width = NULL),
                                                          # box(width = NULL,
                                                          #     plotOutput("plot_cl", height = '300px')
                                                          # )
                                                 ),
                                                 tabPanel("References",
                                                          br(),
                                                          tags$ol(
                                                              tags$li("Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers."), br(),
                                                              tags$li("Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938."),br(),
                                                              tags$li("Kim, H., & Wright, H. H. (2020, January). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers."),br(),
                                                              tags$li("Silge J, Robinson D (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R. JOSS, 1(3). doi: 10.21105/joss.00037")
                                                          )
                                                 )
                                             )

                                      )
                                  )
                              )
                          )
                 )
)
)

######## server ########
# Define server logic required to draw a histogram
server <- function(input, output) {

    ###### Core Lex ####################################
   #bs_themer()
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
    
#### value boxes ####   
    
   #core lex production
   output$results_cl1 <- renderValueBox({
        valueBox(
            value = paste0(selectedData()[["scores"]][1], " core words"),
            subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][1,4], " | Control Percentile: ", selectedData()[["score"]][1,3]),
            icon = icon("list-ol"),
            color = "aqua"
        )
    })
   
    # core lex efficiency
    output$results_cl2 <- renderValueBox({
        valueBox(
            paste0(round(selectedData()[["scores"]][2],1), " core words/min"),
            subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][2,4], " | Control Percentile: ", selectedData()[["score"]][2,3]),
            icon = icon("tachometer"),
            color = "aqua"
        )
    })
    
    #wim
    output$results_ld1 <- renderValueBox({
        valueBox(
            value = round(qdap::diversity(as.character(input$transcr))$shannon,2),
            subtitle = paste0("Word Information Measure"),
            icon = icon("book"),
            color = "aqua"
        )
    })
    #mattr
    output$results_ld2 <- renderValueBox({
        valueBox(
            value = round(MATTR(tokenize(as.character(input$transcr), lang = "en", format = 'obj'), window = 5)@MATTR$MATTR,2),
            subtitle = paste0("Moving Average Type Token Ratio"),
            icon = icon("book-open"),
            color = "aqua"
        )
    })
    #ttr
    output$results_ld3 <- renderValueBox({
        valueBox(
            value = round(TTR(tokenize(as.character(input$transcr), lang = "en", format = 'obj'))@TTR,2),
            subtitle = paste0("Type Token Ratio"),
            icon = icon("newspaper"),
            color = "aqua"
        )
    })
    
    
    

   ################## main concept stuff ##################

   

    data <- reactive({

        df <- tibble(Sentence = as.character(qdap::sent_detect_nlp(input$transcrMC))) %>%
            mutate(Concept = as.factor("no concept"),
                   Correct = FALSE)

    })
        
    
    output$hotable1 <- renderRHandsontable({
        concept_options = c('no concept', 'concept1', 'concept2', 'concept3', 'concept4')
        df = data()
        if (!is.null(df))
            rhandsontable(df, stretchH = "all", height = "500", rowHeaders = NULL, contextMenu = FALSE) %>%
            hot_col(col = "Concept", type = "dropdown", source = concept_options) %>%
            hot_cols(colWidths = c(150, 40, 40),
                     manualColumnMove = FALSE,
                     manualColumnResize = TRUE
            )
        })
    
    hotable1_out <- reactive({
        hot_to_r(input$hotable1) # this will convert your input into a data.frame
    })
    
    output$table_out <- renderDT(hotable1_out(),
                               options = list(dom = 'tp',pageLength = 5
                                            )
    )

    output$results_ht_out <- renderTable({
        mca = hotable1_out()
        mca %>%
            filter(Concept != 'no concept') %>%
            mutate(rownum = row_number(),
                   concept_num = ifelse(Correct == TRUE, 1, 0)) %>%
            group_by(Concept) %>%
            summarize(total_correct = sum(concept_num),
                      order = min(rownum))

    })
    
    # output$results_ht_out2 <- renderTable({
    #     mca = hotable1_out()
    #     len = nrow(mca)
    #     mca %>%
    #         #filter(Concept != 'no concept') %>%
    #         mutate(rownum = row_number(),
    #                concept_num = ifelse(Correct == TRUE, 1, 0)) %>%
    #         group_by(Concept) %>%
    #         summarize(order = min(row_num))
    #     
    # })
    # 
    
    
    
    
    
    
    
    
    
    
    
        
    #########################################################
    
    
    
    
    
    
    
    
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

# Run the application  ###################3
shinyApp(ui = ui, server = server)













# data <- reactive({
# 
#     df <- tibble(Sentence = qdap::sent_detect_nlp(input$transcrMC))
#     rows = nrow(df)
#     
#     for (i in 1:rows) {
#         df$Concept[i] <- as.character(selectInput(inputId = paste0("sel", i),
#                                                   label="",
#                                                   choices = c("select", seq(1,8,1)),
#                                                   width = "75px"))
#         df$Correct[i] <- as.character(selectInput(paste0("cor", i),
#                                                   "",
#                                                   choices = c("select", "yes", "no"),
#                                                   width = "75px"))
#     }
#     
#    return(df)
# 
# })
#     
# 
# output$foo = DT::renderDataTable(
#     data(),
#     escape = FALSE,
#     selection = 'none',
#     server = FALSE,
#     editable = list(target = "column", disable = list(columns = c(2,3))),
#     options = list(dom = 't',
#                    ordering = FALSE,
#                    scrollY = "70vh",
#                    scroller = TRUE,
#                    fixedColumns = list(heightMatch = 'none'),
#                    scrollCollapse = TRUE,
#                    paging = FALSE),
#     callback = JS("table.rows().every(function(i, tab, row) {
#     var $this = $(this.node());
#     $this.attr('id', this.data()[0]);
#     $this.addClass('shiny-input-container');
#   });
#   Shiny.unbindAll(table.table().node());
#   Shiny.bindAll(table.table().node());")
# )
# 
# 
# proxyTeams <- dataTableProxy("foo")
# 
# newdata <- reactive({
#     observeEvent(input$foo_cell_edit, {
#         info <- input$foo_cell_edit
#         i <- info$row
#         j <- info$col + 1L  # column index offset by 1
#         v <- info$value
#         data()
#         data()[i, j] <- coerceValue(v, data()[i, j])
#         replaceData(proxyTeams, data(), resetPaging = FALSE, rownames = FALSE)  # important
#     })
# })
#    
#         output$newdat = renderDT(data(),
#                           options = list(dom = '',
#                                          ordering = FALSE,
#                                          scrollY = "70vh",
#                                          scroller = TRUE,
#                                          fixedColumns = list(heightMatch = 'none'),
#                                          scrollCollapse = TRUE,
#                                          paging = FALSE
#                           )
#         )
# 
# 

# observe({ data()
#     
#     # when it updates, 
#     isolate({
#         new_table = NULL
#         new_table <- reactive({
#             row_num = nrow(data())
#             out <- tibble(
#                 text_order = as.character(seq(1,row_num,1)),
#                 concepts = unlist(sapply(1:row_num, function(i) input[[paste0("sel", i)]])),
#                 accuracy = unlist(sapply(1:row_num, function(i) input[[paste0("cor", i)]]))
#             )
#             out
#             
#         })
#         
#         output$newdat = renderDT(new_table(),
#                           options = list(dom = '',
#                                          ordering = FALSE,
#                                          scrollY = "70vh",
#                                          scroller = TRUE,
#                                          fixedColumns = list(heightMatch = 'none'),
#                                          scrollCollapse = TRUE,
#                                          paging = FALSE
#                           )
#             )
#             
#     })
# })
