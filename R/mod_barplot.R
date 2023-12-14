#' barplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barplot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, # begin_A3.2 palettes ----------
             checkboxInput(ns("axis_relative"),
                           label = "Relative to total (x-axis to %)",
                           value = FALSE)
             # plotOutput(ns("barplot"))
             
      )),
    fluidRow(column(12,
                    bsCollapsePanel(style = "default", h4('Plot appearance:'), #plot panel -----------
                                    wellPanel(style = "background-color: #ffffff;",
                                              selectInput(ns("my_palette"),
                                                          label = "Colour palette:",
                                                          selected = "Set1",
                                                          choices  = palettes,
                                                          multiple = FALSE),
                                              radioButtons(ns("legend_columns"), "Legend columns",
                                                           choices  = c(1:3),
                                                           selected = 2,
                                                           inline   = TRUE),
                                              checkboxInput(ns("perc_label"), "% label:", TRUE),
                                              radioButtons(ns("black_white"), label=NULL,
                                                           choices = list(
                                                             'Black' = 'black',
                                                             'White' = 'white'
                                                           ),
                                                           selected = 'white',
                                                           inline=TRUE)
                                    )
                    )
                    
    ))
    
    
  )
}

#' barplot Server Function
#'
#' @noRd 
mod_barplot_server <- function(input, output, session, r, summary_table, plot_labels = list(expl1 = "", expl2 = "", outcome = "")){
  ns <- session$ns
  
  barplot <- reactive({
    
    # plot_labels = allvars 
    # 
    # 
    # bmjdata_allvars %>% 
    #   tibble::enframe() %>% 
    #   tidyr::unnest(cols = c("name", "value"))
    
    
    p <- create_barplot(summary_table(),
                        axis_relative   = input$axis_relative,
                        mypalette       = input$my_palette,
                        my_ncol         = as.numeric(input$legend_columns),
                        perc_label      = input$perc_label,
                        black_white     = input$black_white,
                        plot_labels     = plot_labels)
    return(p)
  })
  

  return(barplot)

  

  
}

## To be copied in the UI
# mod_barplot_ui("barplot_ui_1")

## To be copied in the server
# callModule(mod_barplot_server, "barplot_ui_1")