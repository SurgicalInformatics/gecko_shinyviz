#' render_barplot_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_render_barplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, # begin_A3.2 palettes ----------
             conditionalPanel("$('html').hasClass('shiny-busy')", h3("Loading...", style="color:#FF0099")),
             uiOutput(ns("barplot.ui")),
             bsCollapsePanel(style = "default", h4('Show table:'),
                             wellPanel(style = "background-color: #ffffff;",
                                       DT::DTOutput(ns("table"))
                             )
             ),
             
             bsCollapsePanel(style = "default", h4('Optional: plot width/height'),
                             wellPanel(style = "background-color: #ffffff;",
                                       sliderInput(ns("width"),  "Plot Width (%)", min = 20, max = 100, value = 80, step=10),
                                       sliderInput(ns("height"), "Plot Height (px)", min = 200, max = 1000, value = 700, step=50)
                             )
             )
      )
    )
  )
}

#' render_barplot_ui Server Function
#'
#' @noRd 
mod_render_barplot_server <- function(input, output, session, r, summary_table, barplot){
  ns <- session$ns
  
  observe({
    number_explanatory   = summary_table()$expl1   %>% n_distinct()
    number_panels        = summary_table()$expl2   %>% n_distinct()
    number_outcomes      = summary_table()$outcome %>% n_distinct()
    
    
    adjust_height = 80 + number_explanatory*50 + number_panels*100 + number_outcomes*10
    updateSliderInput(session, "height", value = adjust_height)
  })
  
  output$barplot <- renderPlot({
    barplot()
  }, res = 96)
  
  
  output$barplot.ui <- renderUI({
    plotOutput(ns("barplot"), width = paste0(input$width, "%"), height = input$height)
  })
  
  output$table <- DT::renderDataTable({
    summary_table() %>% 
      mutate(relative = round(relative, 3))
  })
  
}

## To be copied in the UI
# mod_render_barplot_ui("render_barplot_ui_1")

## To be copied in the server
# callModule(mod_render_barplot_server, "render_barplot_ui_1")

