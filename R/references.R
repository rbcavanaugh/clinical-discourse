library(shiny)
library(DT)
library(dplyr)

ui <- fluidPage(
  title = 'Selectinput column in a table',
  h3("Source:", tags$a("Yihui Xie", href = "https://yihui.shinyapps.io/DT-radio/")),
  numericInput('num', "enter a number", value = 5, min = 1, max = 10, step = 1),
  DT::dataTableOutput('foo'),
  verbatimTextOutput('sel')
)

server <- function(input, output, session) {
  
  counter <- reactiveVal(0)
  get_sel_id <- reactive({
    input$num
    isolate(counter(counter() + 1))
    paste0("sel", counter())
  })
  
  data <- reactive({
    df <- slice_sample(iris, n= input$num)
    
    for (i in 1:nrow(df)) {
      df$species_selector[i] <- as.character(selectInput(paste0(get_sel_id(), i),
                                                         "",
                                                         choices = unique(iris$Species),
                                                         width = "100px"))
    }
    df
  })
  
  output$foo = DT::renderDataTable(
    data(), escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  output$sel = renderPrint({
    str(sapply(1:nrow(data()), function(i) input[[paste0(get_sel_id(), i)]]))
  })
}

shinyApp(ui, server)