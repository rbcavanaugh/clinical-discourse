#Easy:

# Medium:
# view examples for story or for that specific concept?
# front page - welcome modal 
# true corlex norms

# Do towards the end:
# mca norms and results page
# download of data - core lex and mca

# questions: 
# swap sentences and scoring??



############### load stuff ################
library(shiny)
library(here)
library(scales)
library(fresh)
library(tidyverse)
library(patchwork)
library(textstem)
library(tidytext)
library(DT)
library(truncnorm)
library(waiter)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(shinyjs)
library(tokenizers)
library(shinyglide)
# load info 
source(here('R', 'core_lex.R'))
source(here('R', 'main_concept.R'))
source(here('R', 'extra_text.R'))

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
      tags$link(rel = "stylesheet", type = "text/css", href = "mca_css.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$title("Aphasia Discourse"),
      includeHTML("www/analytics.html")
    ),
    #app
    navbarPage(
      id = "bigpage",
      #theme = shinytheme("flatly"),
      fluid = T,collapsible = T, 
      title = div(tags$a(id = "img-id", target = "_blank",
                     href = "https://github.com/rbcavanaugh/clinical-discourse",
                     icon("github"))),

      ###### main concept ######
      tabPanel("Main Concept & Sequencing",
               sidebarLayout(fluid = T, 
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
                              awesomeCheckbox("show_sequencing", "Ignore Sequencing", value = F, status = "primary"),
                              
                              div(align = "center",
                              actionButton("start", "Start Scoring"),
                              actionButton("button3", "Start Over")
                              )
                              
                 ),
                 mainPanel(width = 9, style = "padding-right: 30px;",
                   conditionalPanel(condition = "output.countzero == true",
                                    tabsetPanel(id = "page1",
                                    tabPanel("Getting Started",
                                      
                                    fluidRow(
                                        column(width = 12, 
                                             glide_div
                                      )
                                    )
                                    ),
                                    tabPanel("Reference Manual", 
                                            tags$iframe(style="height:600px; width:100%; scrolling=yes",
                                                        src="manual.pdf")
                                            )

                                    )
                   ),
                   # scoring panels
                   conditionalPanel(condition = "output.count == true",
                                    actionButton(
                                      inputId = "help",
                                      label = " Help",
                                      #color = "default",
                                      #style = "unite", 
                                      icon = icon("question")
                                    ),
                     fluidRow(
                       column(width = 12,
                         fluidRow(
                           wellPanel(style = 'overflow-x: scroll; height:190px; padding: 10px 20px 0px 5px;border-radius: 5px;',
                             htmlOutput("img")
                               )
                         ),
                         fluidRow(
                           wellPanel(id = "sequencing_input", style = "padding-bottom:0px; padding: 0px 10px 10px 10px; overflow-x: scroll; height:200px; border-color: #ecf0f1; background-color: #fff;",
                               h4("1. Select the sentences that match the concept."),
                                uiOutput("sortable"),
                           )
                         ),
                         fluidRow(
                           wellPanel(style = "padding-bottom:0px; padding: 0px 10px 10px 10px; overflow-x: scroll; height:150px; border-color: #ecf0f1; background-color: #fff;",
                             h4("2. Score each concept noted above. The current score is [current score]."),
                             #column(width = 12, align = "center", 
                               div( id = "mca_results", 
                                    uiOutput("scores1234")
                             )
                           )

                         ),
                         fluidRow(align = "center",
                           #column(width = 12, style = "align:center;",
                           actionButton("prev", label = "", icon = icon('arrow-left')),
                           actionButton("nxt", label = "",  icon = icon('arrow-right')) #style = "float:right;",
                           
                         )
                       ),
                     )
                 ),
                 conditionalPanel(condition = "output.countback == true",
                                  fluidRow(
                                    wellPanel(align = "center",
                                    br(),
                                    h3("This will be information about interpreting results"),
                                    br(), br(),
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 4, align = "center",
                                           #tableOutput("sequencing"),
                                           DTOutput("results_mca_table"),
                                           ),
                                  column(width = 8, align = "center", br(), br(), br(), 
                                         h3("Plot goes here")
                                         )
                                    ),br(),
                                  fluidRow(
                                    wellPanel(align = "center",
                                    h4("this will give an option to download all data."),
                                    h5("This includes each sentence, which concept it was scored with, how the essential elements of each concept was scored, and how each concept was scored in terms of accuracy and completeness. ")
                                  )
                                  )
                                  )
                 )
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
                              
                              awesomeCheckbox(inputId = "adj",
                                              label = "Possessive ['s] (Cinderella)", 
                                              value = FALSE, #bigger = T, 
                                              #icon = icon("check"),
                                              status = "primary"
                              )
                              
                              # sliderInput("adj", "Score Adjustment:", value= 0,
                              #             min = -10, max = 10, step = 1)
                 ),
                 mainPanel(width = 9,
                           fluidRow(
                             column(width = 6,
                                    tabsetPanel(
                                      tabPanel("Check Scoring", 
                                               br(),
                                               tags$ol(
                                                 tags$li("Following Core-lex rules, check that target lexemes match tokens and that target lexemes without a matched token were not missed by the algorithm."),
                                                 tags$li("Add 1 point for a possessive [ 's ] (Cinderella only)"),
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
                          href = "https://shs.unm.edu/people/faculty/jessica-richardson.html",
                          icon("external-link"),
                          "Jessica Richardson")),
               box(width = NULL,
                   actionButton("feedback", "Send Feedback", style = 'float:right;')
               )
               )

    )
  )

######## server ########
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
#####introduction modal
  # 
  # showModal(
  #   modalDialog(
  #     title = "Core Lexicon and Main Concept Scoring!",
  #     p("This is a welcome screen"),
  #     p("Should we use this screen?"),
  #     p("Here's why you should use this app (or similar methods)"),
  #     p("here's what this app does"),
  #     p("Here's what it doesn't do"),
  #     p("Here's some other stuff that you should know"),
  #     p("This app is free and open source; the code is public."),
  #     size = "m",
  #     easyClose = T,
  #     footer = modalButton("Lets Go!")
  #   )
  # )
  # 
  
  
  
  
  
  
  ###### Core Lex Stuff ####################################
###### stuff that doesn't matter right now.... #####
  # feedback core lex
  observeEvent(input$feedback, {
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
  observeEvent(input$help, {
    showModal(
      modalDialog(
        tabsetPanel(
          tabPanel("Review Scoring Rules",
                   glide_div
                   ),
          tabPanel("All Concepts",
                  div(style = "height: 600px; overflow-y:auto;",
                    get_concepts(concept = input$stimMC)
                    )
           )
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
  })
  
  

######## core lexicon stuff ######

# core lexicon table output
  
  # core lex counter
  counter2 <- reactiveVal(0)
  get_cor_id <- reactive({
    nrow(selectedData2())
    isolate(counter2(counter2() + 1))
    paste0("cor_check", counter2())
  })
  
  data <- reactive({
    df = selectedData2()
    for (i in 1:nrow(df)) {
      df[["Correct"]][i] <- as.character(shinyWidgets::awesomeCheckbox(paste0(get_cor_id(), i),
                                                                  label="",
                                                                  #width = "20px",
                                                                  value = df$produced[i],
                                                                  status = "primary"#,
                                                                  #bigger = T,
                                                                 #icon = icon("check")
      ))
    }
    df %>% dplyr::select(-produced)
  })
  
  ####core lex table #####
  output$table_cl = DT::renderDataTable(
    data(),
    escape = FALSE,
    selection = 'none',
    server = FALSE,
    options = list(dom = 't',
                   ordering = TRUE,
                   scrollY = "400px",
                   #scroller = TRUE,
                   fixedColumns = list(heightMatch = 'none'),
                   #scrollCollapse = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:3)),
                   paging = FALSE
    ),
    callback = JS(
      "table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  
  
  score_num_data <- reactive({
    accuracy = unlist(sapply(1:nrow(data()), function(i) input[[paste0(get_cor_id(), i)]]))
    
    return(sum(as.numeric(accuracy)))

  })

  output$sel = renderTable({
    score_num()
  })
  
  
  
 # core lexicon results plot 
  output$plot_cl <- renderPlot({
    prod <- selectedData()[[2]] %>%
      mutate(Cohort = ifelse(dist == 'dist1' | dist == 'dist3', 'control', 'aphasia'),
             met = factor(ifelse(dist == 'dist1' | dist == 'dist2', 'Production', 'Efficiency'),
                          levels = c('Production', 'Efficiency'))
      ) %>%
      dplyr::filter(met == "Production") %>%
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
      dplyr::filter(met == "Efficiency") %>%
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
      color = "blue"
    )
  })
  
  # core lex value box efficiency
  output$results_cl2 <- renderValueBox({
    valueBox(
      paste0(round(selectedData()[["scores"]][2],1), " core words/min"),
      subtitle = paste0("Aphasia Percentile: ", selectedData()[["score"]][2,4], " | Control Percentile: ", selectedData()[["score"]][2,3]),
      icon = icon("tachometer"),
      color = "blue"
    )
  })
  
  observeEvent(input$stim,{
    if(input$stim != "cinderella"){
      shinyjs::disable("adj")
    } else {
      shinyjs::enable("adj")
    }
  })
  
  ################## main concept stuff ##################
  
  # startign places for pages and storing data. ####
  values = reactiveValues(i=0,
                          concept = list(),
                          selected_sentences = list(),
                          concept_accuracy = list()
  )
  
  observeEvent(input$goback | input$button3,{
    values$i=0
    values$concept = list()
    values$selected_sentences = list()
    values$concept_accuracy = list()
  })
  
  observeEvent(input$start,{ 
    values$i=0
    values$concept = list()
    values$selected_sentences = list()
    values$concept_accuracy = list()
  })
  
  stim_task <- reactive({
    tibble(
    stim = input$stimMC,
    stim_num = if(input$stimMC == 'broken_window'){1
    } else if(input$stimMC=='cat_rescue'){2
    } else if(input$stimMC == 'refused_umbrella'){3
    } else if(input$stimMC == 'cinderella'){4
    } else if(input$stimMC == 'sandwich'){5
    } else {0},
    num_slides = if(input$stimMC == 'broken_window'){8
    } else if(input$stimMC=='cat_rescue'){10
    } else if(input$stimMC == 'refused_umbrella'){10
    } else if(input$stimMC == 'cinderella'){34
    } else if(input$stimMC == 'sandwich'){10
    } else {0}
    )
  })
  
 
  
  
  # counter functions for conditional panels ####
  
  # page 1 getting started
  output$countzero <- reactive({
    values$i==0
  })
  outputOptions(output, "countzero", suspendWhenHidden = FALSE)
  
  # show panel with scoring
  output$count <- reactive({
    if(stim_task()$stim == 'broken_window'){
      values$i>0&&values$i<9
    } else if(stim_task()$stim=='cat_rescue'){
      values$i>0&&values$i<11
    } else if(stim_task()$stim == 'refused_umbrella'){
      values$i>0&&values$i<11
    } else if(stim_task()$stim == 'cinderella'){
      values$i>0&&values$i<35
    } else if(stim_task()$stim == 'sandwich'){
      values$i>0&&values$i<11
    } else {}
  })
  outputOptions(output, "count", suspendWhenHidden = FALSE)
  
  # does a left arrow appear?
  output$countleft <- reactive({
    values$i>0
  })
  outputOptions(output, "countleft", suspendWhenHidden = FALSE)
  
  # does a right arrow appear?
  output$countright <- reactive({
    if(stim_task()$stim == 'broken_window'){
      values$i>0&&values$i<9
    } else if(stim_task()$stim=='cat_rescue'){
      values$i>0&&values$i<11
    } else if(stim_task()$stim == 'refused_umbrella'){
      values$i>0&&values$i<11
    } else if(stim_task()$stim == 'cinderella'){
      values$i>0&&values$i<35
    } else if(stim_task()$stim == 'sandwich'){
      values$i>0&&values$i<11
    } else {}
  })
  outputOptions(output, "countright", suspendWhenHidden = FALSE)
  
  # is there a start-over button?
  output$countback <- reactive({
    if(stim_task()$stim == 'broken_window'){
      values$i>8
    } else if(stim_task()$stim=='cat_rescue'){
      values$i>10
    } else if(stim_task()$stim == 'refused_umbrella'){
      values$i>10
    } else if(stim_task()$stim == 'cinderella'){
      values$i>34
    } else if(stim_task()$stim == 'sandwich'){
      values$i>10
    } else {}
  })
  outputOptions(output, "countback", suspendWhenHidden = FALSE)
  
  
  #counter fnctions to change page contents ######
  # note this also saves the input data to the reactive list 'values' #####
  # this is probably ok for the time being
  observeEvent(input$nxt,{
    
    score = c(input$accuracy1, input$accuracy2, input$accuracy3, input$accuracy4)
    len = length(score)
    component = seq(1,len,1)
    concept = rep(values$i, len)
    
    values$concept[[values$i]] = values$i
    values$selected_sentences[[values$i]] = input$score_mca
    values$concept_accuracy[[values$i]] = tibble(rating = score,
                                                 component = component,
                                                 concept = concept)
    values$i <- values$i + 1
  })
  
  # counter down
  observeEvent(input$prev,{
     values$i <- values$i - 1
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
  
  observeEvent(input$prev|input$nxt,{
    if(values$i ==1){
      shinyjs::disable("prev")
    } else {shinyjs::enable("prev")}
  })
  
  
  order <- eventReactive(input$nxt,{
    if(values$i==stim_task()$num_slides+1){
      if(length(values$selected_sentences) < 2){
          tibble(warning = "not enough data for sequencing. try selecting more sentences next time.")
        } else {
        sen_df = sentences() %>% 
          dplyr::mutate(sentence_num = row_number()) %>% 
          rename(sentence = txt)
        
        as_data_frame(t(map_dfr(values$selected_sentences, ~as_data_frame(t(.))))) %>%
          pivot_longer(cols = tidyselect::everything(), names_to = "concept", values_to = "sentence") %>%
          mutate(concept = str_remove(concept, "V")) %>%
        arrange(concept) %>%
        drop_na() %>%
          left_join(sen_df, by = "sentence")
        }
    }
  })
  
  
  
  #### ok trickier bits
  
  component_df <- reactive({
    tmp = main_concepts %>%
      dplyr::filter(task == input$stimMC) %>%
      mutate(new = seq2(from = 1, to = concept_length, by = 1))
    
    tibble(
      a = unlist(tmp$new)
    )
  })

  filter_na_concepts <- reactive({
    main_concepts %>%
      ungroup() %>%
      dplyr::filter(task == input$stimMC) %>% #'broken_window') %>%#
      dplyr::select(id, 2:5) %>%
      pivot_longer(cols= 2:5, names_to = "component", values_to = "element") %>%
      mutate(component = as.numeric(str_remove(component, "e"))) %>%
      rename(concept = id)
  })
  
  results_mca <- eventReactive(input$nxt,{
    if(values$i==stim_task()$num_slides+1){
      mca = bind_rows(values$concept_accuracy) %>%
        left_join(filter_na_concepts(), by = c('concept', 'component')) %>%
        drop_na(element) %>%
        group_by(concept) %>%
        summarize(absent = sum(as.numeric(rating == "Absent")),
                  accurate = sum(as.numeric(rating == "Accurate")),
                  inaccurate = sum(as.numeric(rating == "Inaccurate"))) %>%
        mutate(
          accuracy = case_when(
            inaccurate > 0 ~ "I",
            accurate > 0 ~ "A",
            TRUE ~ "Absent"
          ),
          completeness = case_when(
            absent > 0 ~ "I",
            absent == 0 ~ "C",
            TRUE ~ "missed"
          )
        ) %>%
        mutate(completeness = ifelse(accuracy == "Absent", "", completeness)) %>%
        unite(col = "Result", accuracy, completeness, sep = "", remove = F) %>%
        left_join(scoring_mca, by = "Result")
    } else {}
    
  })
  
  # output_results <- eventReactive(input$nxt,{ # change this to download....
  #     if(values$i==stim_task()$num_slides+1){
  #       mca = bind_rows(values$concept_accuracy) %>%
  #         left_join(filter_na_concepts(), by = c('concept', 'component')) %>%
  #         drop_na(element)
  #     } else {}
  # })
  
  output$results_mca_table <- renderDT(
    results_mca() %>%
      select(Concept = concept, Code = Result, Score = score) %>%
      mutate(Concept = as.character(Concept),
             Score = as.character(Score)),
    rownames = F,
    options = list(dom = 't',
                   #ordering = TRUE,
                   #scrollY = "400px",
                   #scroller = TRUE,
                   #fixedColumns = list(heightMatch = 'none'),
                   #scrollCollapse = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                   paging = FALSE
    )
  )

  
  # output$sequencing <- renderTable({
  #   order()
  # })
  
  
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
      selected = if (length(values$selected_sentences)>=values$i && values$i>0){
        values$selected_sentences[[values$i]]
      } else {sel()},
      status = "custom-status"
    )
    )
    
  })
  
  observeEvent(input$show_sequencing,{
    if(isTruthy(input$show_sequencing)){
    shinyjs::hide("sequencing_input")
    } else {
      shinyjs::show("sequencing_input")
    }
  })
  
  scoring_df <- reactive({
    main_concepts %>%
      dplyr::filter(task == input$stimMC)
  })
  # accuracy scoring x3 #######
  

 output$score1 <- renderUI({
   if(values$i<stim_task()$num_slides+1){
     awesomeRadio(
     inputId = "accuracy1",
     label = scoring_df()[values$i, 2], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     checkbox = TRUE,
     #fill = TRUE,
     selected = if (length(values$concept_accuracy)>=values$i && values$i>0){
       values$concept_accuracy[[values$i]][1,1]
       } else {"Absent"}
   )
     } else {}
   
 })
 
 output$score2 <- renderUI({
   if(values$i<stim_task()$num_slides+1){
     awesomeRadio(
     inputId = "accuracy2",
     label = scoring_df()[values$i, 3], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     checkbox = TRUE,
     selected = if (length(values$concept_accuracy)>=values$i && values$i>0){
       values$concept_accuracy[[values$i]][2,1]
     } else {"Absent"}
   )
     }else{}
   
 })
 
 
 dontshow <- reactive({
   tmp = main_concepts %>%
     dplyr::filter(task == input$stimMC) %>%
     ungroup() %>%
     dplyr::select(e3) %>%
     slice(values$i)
 })
 
 
 output$score3 <- renderUI({
   if(values$i<stim_task()$num_slides+1){
     if(anyNA(dontshow())){} else {
       awesomeRadio(
     inputId = "accuracy3",
     label = scoring_df()[values$i, 4], 
     choices = c("Accurate", "Inaccurate", "Absent"),
     #inline = TRUE, 
     status = "primary",
     checkbox = TRUE,
     selected = if (length(values$concept_accuracy)>=values$i && values$i>0){
       values$concept_accuracy[[values$i]][3,1]
     } else {"Absent"}
   )
     } 
   }else{}
   
 })
 
 output$score4 <- renderUI({
   awesomeRadio(
       inputId = "accuracy4",
       label = scoring_df()[values$i, 5], 
       choices = c("Accurate", "Inaccurate", "Absent"),
       #inline = TRUE, 
       status = "primary",
       checkbox = TRUE,
       selected = if (length(values$concept_accuracy)>=values$i && values$i>0){
         values$concept_accuracy[[values$i]][4,1]
       } else {"Absent"}
     )
 })
 
 
 output$scores1234 <- renderUI({
   if (input$stimMC == "cinderella" && values$i == 16){
     tagList(
   column(width = 3, align = "left",
          uiOutput("score1")
   ),
   column(width = 3, align = "left",
          uiOutput("score2")
   ),
   column(width = 3, align = "left",
          uiOutput("score3")
   ),
   column(width = 3, align = "left",
          uiOutput("score4")
   )
     )
   } else {
     tagList(
     column(width = 4, align = "left",
            uiOutput("score1")
     ),
     column(width = 4, align = "left",
            uiOutput("score2")
     ),
     column(width = 4, align = "left",
            uiOutput("score3")
     )
     )
   }
   
 })
 
 
 
 
 
 
 
 
 observeEvent(input$nxt | input$prev | input$button3 | input$goback,{
   if(values$i>0){
     shinyjs::disable("stimMC")
   } else {
     shinyjs::enable("stimMC")
   }
 })


 observeEvent(input$nxt | input$prev, {
   if(values$i != 16){
     shinyjs::hide("score4")
   } else {
     shinyjs::show("score4")
   }
 })
 
 ##### right now, screenshot with instructions:
 
 output$img <- renderUI({
   img_val = values$i
   paste_val = if(input$stimMC == 'broken_window'){'bw'
     } else if(input$stimMC == 'cat_rescue'){'cr'
     } else if(input$stimMC == 'refused_umbrella'){'u'
     } else if(input$stimMC == 'cinderella'){'c'
     } else if(input$stimMC == 'sandwich'){'s'
     } else {}
   #tags$img(src = file.path(paste('c', img_val, '.png', sep='')), width = "100%")
   if(img_val>0 && img_val < stim_task()$num_slides+1){
   return(get(paste0(paste_val, img_val)))
   } else {}
 })




  #########################################################
  
 # this makes the table for scoring....
 # reactive data
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
     rename('Target Lexeme' = target_lemma,
            'Token Produced' = token
     )
   return(table)
 })
 
 # this calculates the scores....
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
    
    #df <- core_lex(input$transcr, task, input$age)
    #matches <- df$match
    score_num = score_num_data() + input$adj #sum(matches$produced) +
    score_eff = score_num/(input$time/60)
    
    # Norn data ####
    
    # ACCURACY #
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
    
    # EFFICIENCY 
    
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
    
    
    #####
    
    
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
  
  
  
  
  
  # observeEvent(input$nxt | input$prev, {
  #   if(stim_task()$stim_num == 1 && values$i == 4 || values$i == 5) {
  #   shinyjs::hide("score3")
  #   } else if(stim_task()$stim_num == 2 && values$i == 6){
  #     shinyjs::hide("score3")
  #   } else if(stim_task()$stim_num == 3 && values$i == 1 || values$i == 5){
  #     shinyjs::hide("score3")
  #   } else if(stim_task()$stim_num == 4 && values$i == 6 || values$i == 7 || values$i == 9 || values$i == 10 || values$i == 12 || values$i == 14 || values$i == 19 || values$i == 32 || values$i == 33 || values$i == 34){
  #     shinyjs::hide("score3")
  #   } else if(stim_task()$stim_num == 5 && values$i == 3 || values$i == 4 || values$i == 5){
  #     shinyjs::hide("score3")
  #   } else {
  #     shinyjs::show("score3")
  #   }
  # })

  
}

# Run the application  ###################3
shinyApp(ui = ui, server = server)

