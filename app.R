
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
# library(koRpus)
# set.kRp.env(lang="en")
# koRpus.lang.en::lang.support.en()
library(waiter)
#library(bslib)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(shinyjs)
library(tokenizers)
library(sortable)

# load info 
source(here('R', 'core_lex.R'))



###################### UI ###################
ui <- 
  tagList(
    #setup
    useShinyjs(),
    use_waiter(),
    waiter_preloader(html = spin_dots(), color = "#2c3e50"), #html = spin_dots(), color = "#f0f0f0"
    useShinydashboard(),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.png", type="image/png"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "js/javascript.js"),
      tags$script(src="javascript.js"),
      tags$title("Aphasia Discourse"),
      includeHTML("www/analytics.html")
    ),
    #app
    navbarPage(
      id = "bigpage",
      theme = shinytheme("flatly"),
        # bslib::bs_theme(#secondary = "#55a998",# success = "#55a998", 
        #                        base_font = font_google("Roboto"), `enable-gradients` = TRUE, 
        #                        `enable-shadows` = TRUE, bootswatch = "flatly", spacer = "1rem"),
      fluid = T,collapsible = T, 
      #windowTitle = "Aphasia Discourse",
      title = div("",
        tags$a(id = "img-id",
                     href = "https://github.com/rbcavanaugh/clinical-discourse",
                     icon("github"))),
      
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
                                          min = -10, max = 10, step = 1)
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
                              
                              textAreaInput("type",
                                            h5("Enter Transcript"),
                                            value = transcriptDefault,# transcriptDefault,
                                            height = '400px'),
                              
                              actionButton("button2", "See All Concepts", style = "float: right;")
                 ),
                 mainPanel(
                   conditionalPanel(condition = "output.countzero == true",
                                      fluidRow(
                                        column(width = 12, style = "align:center;",
                                               p("This is the getting started page."),
                                               p("The code only works for the broken window task right now. It will be a little tricky programming-wise to adjust the code based on the stimulus, so I've focused on the scoring setup first."),
                                               p("This page should have instructions and some background information"),
                                               p("After going through each step, we can show the results"),
                                               p("'See all concepts' on the left will be accessible at any point in the scoring process"),
                                               p("right now, if you go back, you will have to re-score the ones you've gone back over. I might be able to save the scores for going backwards and forwards without having to re-score...but I'll leave that for one of the final steps."),
                                               p("right now, I've put in two tables that will spit out information needed to calculate order and accuracy at the end page."),
                                               actionButton("start", "Get Started")
                                               )
                                      )
                                    ),
                   # scoring panels
                   conditionalPanel(condition = "output.count == true",
                     fluidRow(
                       column(width = 1,style = "margin-top:40vh",
                                conditionalPanel(condition = "output.countleft == true",
                                actionButton("prev", label = "", icon = icon('arrow-left'))
                                )
                              ),
                       column(width = 10,
                         fluidRow(
                             box(width = NULL, height = "180px",
                             htmlOutput("img")
                             
                           )
                         ),
                         fluidRow(
                           #column(
                             #width = 6,
                           "Select the sentences that match the concept.",
                            box(width=NULL,
                                uiOutput("sortable")
                            )
                         ),
                         fluidRow(
                                   #)
                             #)
                           #),
                           #column(
                            # width = 6,
                            # box(width= NULL,
                             "Check boxes for scoring accuracy go here.",
                             #box(
                               #width = NULL,
                             box(width = NULL,
                             column(width = 4,
                               uiOutput("score1")
                               ),
                             column(width = 4,
                               uiOutput("score2")
                               ),
                             column(width = 4,
                               uiOutput("score3")
                             )
                             )
                             #)
                            # )
                           #)
                         )
                       ),
                       column(width = 1, style = "margin-top:40vh",
                                conditionalPanel(condition = "output.countright == true",
                                actionButton("nxt", label = "", style = "float:right;", icon = icon('arrow-right'))
                                )
                              )
                     )
                 ),
                 conditionalPanel(condition = "output.countback == true",
                                  fluidRow(
                                    column(width = 12, style = "align:center;",
                                           "This is the results page",
                                           actionButton("goback", "Start Over"),
                                           tableOutput("sequencing"),
                                           #tableOutput("accuracy"),
                                           tableOutput("results_mca_table")
                                           #tableOutput("results_mca_table2")
                                    )
                                  )
                 )
               )
               )
      ),
      tabPanel("About",
               box(width = NULL,
                   tags$a(id = "rc",
                          icon("external-link"),
                          href = "https://robcavanaugh.com",
                          "Rob Cavanaugh")),
               box(width = NULL,
                   tags$a(id = "sg",
                          href = "https://www.marquette.edu/speech-pathology-audiology/building-rehabilitation-advances-in-neurscience-lab.php",
                          icon("external-link"),
                          "Sarah Grace Dalton")),
               box(width = NULL,
                   tags$a(id = "jr",
                          href = "https://www.marquette.edu/speech-pathology-audiology/building-rehabilitation-advances-in-neurscience-lab.php",
                          icon("external-link"),
                          "Jessica Richardson")),
               box(width = NULL,
                   actionButton("button", "Send Feedback", style = 'float:right;')
               )
               )

    )
  )

######## server ########
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ###### Core Lex Stuff ####################################
###### stuff that doesn't matter right now.... #####
  # feedback core lex
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
  # feedback main concept
  observeEvent(input$button2, {
    showModal(
      modalDialog(
        title = "All concepts...need better image",
        tags$img(src = file.path('all.png'), height = "600px"),
        easyClose = TRUE,
        size = "l"
      ))
  })
  

######## core lexicon stuff ######

# core lexicon table output
  output$table_cl <- renderDT({
    table = selectedData2()
    
  },options = list(dom = "ftp"))
  
 # core lexicon results plot 
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
  
  #core lex value box production
  output$results_cl1 <- renderValueBox({
    valueBox(
      value = paste0(selectedData()[["scores"]][1], " core words"),
      subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][1,4], " | Control Percentile: ", selectedData()[["score"]][1,3]),
      icon = icon("list-ol"),
      color = "aqua"
    )
  })
  
  # core lex value box efficiency
  output$results_cl2 <- renderValueBox({
    valueBox(
      paste0(round(selectedData()[["scores"]][2],1), " core words/min"),
      subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][2,4], " | Control Percentile: ", selectedData()[["score"]][2,3]),
      icon = icon("tachometer"),
      color = "aqua"
    )
  })
  

  
  ################## main concept stuff ##################
  
  # startign places for pages and storing data. ####
  values = reactiveValues(i=0,
                          concept = list(),
                          selected_sentences = list(),
                          concept_accuracy = list()
  )
  
  
  # counter functions for conditional panels ####
  output$countzero <- reactive({
    values$i==0
  })
  outputOptions(output, "countzero", suspendWhenHidden = FALSE)
  
  output$count <- reactive({
    values$i>0&&values$i<9
  })
  outputOptions(output, "count", suspendWhenHidden = FALSE)
  
  output$countleft <- reactive({
    values$i>0
  })
  outputOptions(output, "countleft", suspendWhenHidden = FALSE)
  
  output$countright <- reactive({
    values$i>0&&values$i<9
  })
  outputOptions(output, "countright", suspendWhenHidden = FALSE)
  
  output$countback <- reactive({
    values$i>8
  })
  outputOptions(output, "countback", suspendWhenHidden = FALSE)
  
  
  #counter fnctions to change page contents ######
  # note this also saves the input data to the reactive list 'values' #####
  observeEvent(input$nxt,{

    values$concept[[values$i]] = values$i
    values$selected_sentences[[values$i]] = input$score_mca
    values$concept_accuracy[[values$i]] = c(input$accuracy1, input$accuracy2, input$accuracy3)

    values$i <- values$i + 1
  })
  
  # counter down
  observeEvent(input$prev,{
    #input$prev
    #isolate({
      values$i <- values$i - 1
     # })
  })
  #counter up getting started
  observeEvent(input$start,{
    #input$start
    #isolate({
      values$i <- values$i + 1
    #})
  })
  observeEvent(input$goback,{
    #input$prev
    #isolate({
    values$i <- 0
    # })
  })
  
  
  
  order <- eventReactive(input$nxt,{
    if(values$i==9){
    if(length(values$selected_sentences) < 2){
        tibble(warning = "not enough data for sequencing")
      } else {
      as_data_frame(t(map_dfr(values$selected_sentences, ~as_data_frame(t(.))))) %>%
        pivot_longer(cols = tidyselect::everything(), names_to = "concept", values_to = "sentence") %>%
        mutate(concept = str_remove(concept, "V")) %>%
      arrange(concept) %>%
      drop_na()
      }
    }
  })
  
  acc <- eventReactive(input$nxt,{
    if(values$i==9){
      as_data_frame(t(map_dfr(values$concept_accuracy, ~as_data_frame(t(.))))) %>%
        pivot_longer(cols = tidyselect::everything(), names_to = "concept", values_to = "rating") %>%
        mutate(concept = str_remove(concept, "V")) %>%
        arrange(concept) %>%
        mutate(component = c(rep(seq(1,3,1),3), 1,2,NA,1,2,NA,rep(seq(1,3,1),3))) %>%
      drop_na()
    }
  })
  
  results_mca <- eventReactive(input$nxt,{
    if(values$i==9){
      mca = as_data_frame(t(map_dfr(values$concept_accuracy, ~as_data_frame(t(.))))) %>%
        pivot_longer(cols = tidyselect::everything(), names_to = "concept", values_to = "rating") %>%
        mutate(concept = str_remove(concept, "V")) %>%
        arrange(concept) %>%
        mutate(component = c(rep(seq(1,3,1),3), 1,2,NA,1,2,NA,rep(seq(1,3,1),3))) %>%
        drop_na() %>%
        group_by(concept) %>%
        summarize(absent = sum(as.numeric(rating == "Absent")),
                  accurate = sum(as.numeric(rating == "Accurate")),
                  inaccurate = sum(as.numeric(rating == "Inaccurate"))) %>%
        mutate(
          accuracy = case_when(
            inaccurate > 0 ~ "I",
            accurate > 0 ~ "A",
            TRUE ~ "I"
          ),
          completeness = case_when(
            absent > 0 ~ "I",
            absent == 0 ~ "C",
            TRUE ~ "missed"
          )
        ) %>%
        unite(col = "together", accuracy, completeness, sep = "", remove = F)
    }
    
  })
  
  # results_mca2 <- eventReactive(input$nxt,{
  #   if(values$i==9){
  #     mca = as_data_frame(t(map_dfr(values$concept_accuracy, ~as_data_frame(t(.))))) %>%
  #       pivot_longer(cols = tidyselect::everything(), names_to = "concept", values_to = "rating") %>%
  #       mutate(concept = str_remove(concept, "V")) %>%
  #       arrange(concept) %>%
  #       mutate(component = c(rep(seq(1,3,1),3), 1,2,NA,1,2,NA,rep(seq(1,3,1),3))) %>%
  #       drop_na() %>%
  #       group_by(concept) %>%
  #       summarize(absent = sum(as.numeric(rating == "Absent")),
  #                 accurate = sum(as.numeric(rating == "Accurate")),
  #                 inaccurate = sum(as.numeric(rating == "Inaccurate"))) %>%
  #       summarize(
  #         accuracy = case_when(
  #                               inaccurate > 0 ~ "I",
  #                               accurate > 0, "A",
  #                               TRUE ~ "missed"
  #                             ),
  #       completeness = case_when(
  #                               absent > 0 ~ "I",
  #                               absent == 0 ~ "C",
  #                               TRUE ~ "missed"
  #                             )
  #       ) %>%
  #      unite(col = "together", accuracy, completeness, sep = "", remove = F)
  #   }
  # 
  # })
  
  output$results_mca_table <- renderTable({
    results_mca()
  })
  
  output$results_mca_table2 <- renderTable({
    results_mca2()
  })
  
  output$sequencing <- renderTable({
    order()
  })
  
  output$accuracy <- renderTable({
    acc()
  })
  
  # get sentences #######
  sentences <- reactive({
    df = tibble(txt = unlist(tokenize_sentences(input$type)))
  })
  
  # resets the check box group sentences input when values$i changes
  # later, change this to return to what was selected for that page
  sel = eventReactive(values$i,{
      sel = NULL
      sel
  })
  
  # displays the unique sentences to be selected
  output$sortable <- renderUI({
    df = sentences()
    div(style = 'overflow-x: scroll',
    checkboxGroupButtons(
      inputId = "score_mca",
      justified = F,
      #width = "100%",
      individual = T,
      #label = "Select Concept",
      choices = unique(df$txt),
      direction = "vertical",
      selected = sel(),
      status = "custom-status"
    )
    )
    
  })
  
  # accuracy scoring x3 #######
 output$score1 <- renderUI({
   if(values$i<9){
   prettyRadioButtons(
     inputId = "accuracy1",
     label = bw_mca[1, values$i], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     fill = TRUE,
     selected = "Absent"
    
   )
     }else{}
   
 })
 
 output$score2 <- renderUI({
   if(values$i<9){
   prettyRadioButtons(
     inputId = "accuracy2",
     label = bw_mca[2, values$i], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     fill = TRUE,
     selected = "Absent"
   )
     }else{}
   
 })
 
 output$score3 <- renderUI({
   if(values$i<9){
   prettyRadioButtons(
     inputId = "accuracy3",
     label = bw_mca[3, values$i], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     fill = TRUE,
     selected = "Absent"
   )
   }else{}
   
 })
 
 ##### right now, screenshot with instructions:
 
 output$img <- renderUI({
   img_val = values$i
   tags$img(src = file.path(paste('c', img_val, '.png', sep='')), width = "100%")
 })
 #####
  
  # observeEvent(selected(),{
  #   values$scored[[values$i]] = tibble(Sentence = round(values$i,0),
  #                                      CIUs = length(input$click_sentence),
  #                                      Words = nrow(sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)))
  #   
  #   values$out_cius[values$i] = input$click_sentence
  #   values$out_words[values$i] = sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)
  #   values$out_sentences[values$i] = sentences()
  # })
  
  # data <- reactive({
  #   df = sentences()
  #   for (i in 1:nrow(df)) {
  #     df$Concept[i] <- as.character(pickerInput(inputId = paste0(get_sel_id(), i),
  #                                               label="",
  #                                               choices = c("no concept", seq(1,8,1)), #
  #                                               width = "115px"))
  #     df$Correct[i] <- as.character(shinyWidgets::awesomeCheckbox(paste0(get_cor_id(), i),
  #                                                 label="",
  #                                                 value = FALSE #width = "40px",
  #                                                 ))
  #   }
  #   df
  # })

  # table_out <- reactive({
  #   concepts = unlist(sapply(1:nrow(data()), function(i) input[[paste0(get_sel_id(), i)]]))
  #   accuracy = unlist(sapply(1:nrow(data()), function(i) input[[paste0(get_cor_id(), i)]]))
  #   
  #   results = sentences()
  #   results$Concept = concepts
  #   results$Accuracy = accuracy
  #   results
  # })
  

  
  
  
  
  
  
  
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
  

###### mobile friendly and small screen stuff ######
  
  # output$title_isitmobile <- renderUI({
  #   if (input$isMobile) 
  #     return("Aphasia Discourse Analysis")
  #   else if (input$width < 950) 
  #     return(div("Aphasia Discourse Analysis",
  #                 tags$a(id = "img-id",
  #                        href = "https://github.com/rbcavanaugh/clinical-discourse",
  #                        icon("github"))))
  #   else 
  #     return(div("Aphasia Discourse Analysis",
  #                      tags$a(id = "img-id",
  #                             href = "https://github.com/rbcavanaugh/clinical-discourse",
  #                             icon("github")),
  #                      tags$a(id = "rc",
  #                             href = "https://www.marquette.edu/speech-pathology-audiology/building-rehabilitation-advances-in-neurscience-lab.php",
  #                             "Sarah Grace Dalton"),
  #                      tags$a(id = "sg",
  #                             href = "https://robcavanaugh.com",
  #                             "Rob Cavanaugh")))
  # })
  

  
}

# Run the application  ###################3
shinyApp(ui = ui, server = server)

# counter <- reactiveVal(0)
# get_sel_id <- reactive({
#   nrow(sentences())
#   isolate(counter(counter() + 1))
#   paste0("sel", counter())
# })
# 
# counter2 <- reactiveVal(0)
# get_cor_id <- reactive({
#   nrow(sentences())
#   isolate(counter2(counter2() + 1))
#   paste0("cor", counter2())
# })

# output$foo = DT::renderDataTable(
#   data(),
#   escape = FALSE,
#   selection = 'none',
#   server = FALSE,
#   editable = list(target = "column", disable = list(columns = c(2,3))),
#   options = list(dom = 't',
#                  ordering = FALSE,
#                  scrollY = "70vh",
#                  scroller = TRUE,
#                  fixedColumns = list(heightMatch = 'none'),
#                  scrollCollapse = TRUE,
#                  paging = FALSE,
#                  columnDefs = list(list(className = 'dt-center dt-bottom', targets = 3),
#                                    list(className = 'dt-top', targets = 2))
#   ),
#   callback = JS("table.rows().every(function(i, tab, row) {
#       var $this = $(this.node());
#       $this.attr('id', this.data()[0]);
#       $this.addClass('shiny-input-container');
#     });
#     Shiny.unbindAll(table.table().node());
#     Shiny.bindAll(table.table().node());")
# )

# output$sel = renderTable({
#   table_out()
# })

# output$results_ht_out <- renderTable({
#   mca = table_out()
#   mca %>%
#     filter(Concept != 'no concept') %>%
#     mutate(rownum = row_number(),
#            Accuracy = ifelse(Accuracy == TRUE, 1, 0)) %>%
#     group_by(Concept) %>%
#     summarize(total_correct = sum(Accuracy),
#               order = min(rownum))
#   
# })


# output$slick_output <- renderSlickR({
#   
#   x <- slickR(slick,
#               slideId = 'myslick',
#               #slideType = 'p',
#               #height = 600,
#               width = '80%'
#               ) + 
#     settings(dots = T)
#   
# })








# ui

# width = 9,
# fluidRow(
#   column(width = 7,
#          tabsetPanel(
#            tabPanel("Check Scoring", br(),
#                     box(width = NULL,
#                         DTOutput("foo")
#                     )
#            ), 
#            tabPanel("Detailed Instructions",
#                     textOutput("scoring_mc"))
#          )
#   ),
#   column(width = 5,
#          tabsetPanel(
#            tabPanel("Instructions",
#                     br(), 
#                     br(),
#                     box(width = NULL,
#                         #h5("Main Concept Checklist"),
#                         slickROutput("slick_output",width='90%'),
#                     )
#                     
#            ),
#            tabPanel("Results",br(),
#                     DTOutput('newdat'),br(),
#                     tableOutput('sel'),
#                     tableOutput('results_ht_out')
#                     # box(width = NULL,
#                     #     plotOutput("plot_cl", height = '300px')
#                     # )
#            ),
#            tabPanel("References",
#                     br(),
#                     tags$ol(
#                       tags$li("Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers."), br(),
#                       tags$li("Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938."),br(),
#                       tags$li("Kim, H., & Wright, H. H. (2020, January). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers."),br(),
#                       tags$li("Silge J, Robinson D (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R. JOSS, 1(3). doi: 10.21105/joss.00037")
#                     )
#            )
#          )
#   )
# )


######## lexical diversity stuff #######
# #wim value  box ####
# output$results_ld1 <- renderValueBox({
#   valueBox(
#     value = round(qdap::diversity(as.character(input$transcr))$shannon,2),
#     subtitle = paste0("Word Information Measure"),
#     icon = icon("book"),
#     color = "aqua"
#   )
# })
# #mattr value box ######
# output$results_ld2 <- renderValueBox({
#   valueBox(
#     value = round(MATTR(tokenize(as.character(input$transcr), lang = "en", format = 'obj'), window = 5)@MATTR$MATTR,2),
#     subtitle = paste0("Moving Average Type Token Ratio"),
#     icon = icon("book-open"),
#     color = "aqua"
#   )
# })
# #ttr value box #####
# output$results_ld3 <-  renderValueBox({
#   valueBox(
#     value = round(TTR(tokenize(as.character(input$transcr), lang = "en", format = 'obj'))@TTR,2),
#     subtitle = paste0("Type Token Ratio"),
#     icon = icon("newspaper"),
#     color = "aqua"
#   )
# })

# feedback lexical diversity ####
# observeEvent(input$button3, {
#   showModal(
#     modalDialog(
#       title = "Feedback",
#       tags$iframe(src = 'https://docs.google.com/forms/d/e/1FAIpQLSfrae3ucppQC_Hy2udxj5_xZwRqbkHwTzUX6PQEnpUdahAb4g/viewform?usp=sf_link',
#                   width = '100%',
#                   height = 500,
#                   frameborder = 0,
#                   marginheight = 0),
#       easyClose = TRUE,
#       size = "l"
#     ))
# })



######### lexical diversity ########
# tabPanel("Lexical Diversity",
#          sidebarLayout(
#            sidebarPanel(width = 3,
#                         selectInput("stimLD", h5("Select Stimulus"),
#                                     c("Broken Window" = 'broken_window',
#                                       "Cat Rescue" = 'cat_rescue',
#                                       "Refused Umbrella" = 'refused_umbrella',
#                                       "Cinderella" = 'cinderella',
#                                       "Sandwich" = 'sandwich'),
#                                     selected = "broken_window"
#                                     
#                         ),
#                         
#                         textAreaInput("transcrLD",
#                                       h5("Enter Transcript"),
#                                       value = transcriptDefault,
#                                       height = '300px'),
#                         
#                         sliderInput("mattr_w", "MATTR WINDOW:", value = 5, min = 5, max = 50),
#                         
#                         actionButton("button3", "Send Feedback", style = 'float:right;')
#            ),
#            mainPanel(width = 9,
#                      fluidRow(
#                        column(width = 6,
#                               tabsetPanel(
#                                 tabPanel("Scoring", 
#                                          h4("Instructions"),
#                                          tags$ol(
#                                            tags$li("Check that target lexemes match tokens, following core lexicon rules."),
#                                            tags$li("Check that target lexemes without a matched token were not missed by the algorithm. (Lexeme Produced = no)"),
#                                            tags$li("Add 1 point to the Cinderella passage if the possessive [ 's ] is used"),
#                                            tags$li("Count any variation of mom/mother or dad/father.")
#                                          ))#,
#                                 # DTOutput("table_cl")), 
#                               #   tabPanel("Detailed Instructions",
#                               #            textOutput("scoring_ld"))
#                               # )
#                               )
#                        ),
#                        column(width = 6,
#                               tabsetPanel(
#                                 tabPanel("Results",br(),
#                                          valueBoxOutput("results_ld1", width = NULL),
#                                          valueBoxOutput("results_ld2", width = NULL),
#                                          valueBoxOutput("results_ld3", width = NULL),
#                                          # box(width = NULL,
#                                          #     plotOutput("plot_cl", height = '300px')
#                                          # )
#                                 ),
#                                 tabPanel("References",
#                                          br(),
#                                          tags$ol(
#                                            tags$li("Dalton, S. G., Hubbard, H. I., & Richardson, J. D. (2019). Moving toward non-transcription based discourse analysis in stable and progressive aphasia. In Seminars in speech and language. Thieme Medical Publishers."), br(),
#                                            tags$li("Dalton, S. G., & Richardson, J. D. (2015). Core-lexicon and main-concept production during picture-sequence description in adults without brain damage and adults with aphasia. American Journal of Speech-Language Pathology, 24(4), S923-S938."),br(),
#                                            tags$li("Kim, H., & Wright, H. H. (2020, January). A tutorial on core lexicon: development, use, and application. In Seminars in speech and language (Vol. 41, No. 01, pp. 020-031). Thieme Medical Publishers."),br(),
#                                            tags$li("Silge J, Robinson D (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R. JOSS, 1(3). doi: 10.21105/joss.00037")
#                                          )
#                                 )
#                               )
#                               
#                        )
#                      )
#            )
#          )
# )