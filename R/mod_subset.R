#' subset UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_subset_ui <- function(id, allvars = allvars, alldata){
  ns <- NS(id)
  tagList(
    fluidRow(
      
      column(12, # A - input panel
             #tableOutput(ns("mytable")),
             wellPanel(style = "background-color: #ffffff;", # begin_A1
                       h4('Main input parameters:'),
                       # A1.1 - explanatory, split, outcome --------------
                       selectInput(ns("explanatory1"),
                                   label    = "Explanatory variable:",
                                   selected = c("gender"),
                                   choices  = allvars,
                                   multiple = FALSE),
                       selectInput(ns("explanatory2"),
                                   label    = "Split by:",
                                   selected = c("ALL"),
                                   choices  = allvars,
                                   multiple = FALSE),
                       selectInput(ns("outcome"),
                                   label    = "Outcome variable:",
                                   selected = c("pre_urgency"),
                                   choices  = allvars,
                                   multiple = FALSE),
                       fluidRow(column(12,
                                       h5("Remove Missing values from:"))),
                       fluidRow(
                         column(3,
                                checkboxInput(ns("rem_missing_expl1"),   "Expl.",   TRUE)),
                         column(3,
                                checkboxInput(ns("rem_missing_expl2"),   "Split",   TRUE)),
                         column(6,
                                checkboxInput(ns("rem_missing_outcome"), "Outcome", TRUE))),
                       checkboxInput(ns("subset"),
                                     label = tags$b("Subset dataset"),
                                     value = FALSE),
                       conditionalPanel(condition = 'input.subset == true',
                                        selectInput(ns("subset_variable"),
                                                    label    = "Variable to subset by:",
                                                    selected = "ALL",
                                                    choices  = allvars,
                                                    selectize = TRUE),
                                        selectInput(ns("subset_levels"),
                                                    label    = "Keep:",
                                                    selected = "ALL",
                                                    choices  = "ALL",
                                                    multiple = TRUE),
                                        #actionButton("updateButton", "Apply filter!"),
                                        ns = ns
                       )
             ),
             #mod_barplot_ui("barplot_ui_1"),
             bsCollapsePanel(style = "default", h4('Advanced parameters:'), #advanced panel -----------
                             wellPanel(style = "background-color: #ffffff;",
                                       fluidRow(column(12,
                                                       h5("Remove Unknown values from:"))),
                                       fluidRow(
                                         column(3,
                                                checkboxInput(ns("rem_unknown_expl1"),   "Expl.",   FALSE)),
                                         column(3,
                                                checkboxInput(ns("rem_unknown_expl2"),   "Split",   TRUE)),
                                         column(6,
                                                checkboxInput(ns("rem_unknown_outcome"), "Outcome", TRUE))),

                                       fluidRow(
                                         column(3,
                                                h5('Reverse order:')),
                                         column(3,
                                                checkboxInput(ns("rev_expl1"),   "Expl.",   TRUE)),
                                         column(2,
                                                checkboxInput(ns("rev_expl2"),   "Split",   TRUE)),
                                         column(4,
                                                checkboxInput(ns("rev_outcome"), "Outcome", TRUE))
                                       ),
                                       
                                       fluidRow( # the percentage label is only plotted for the first factor level (otherwise would start overlapping)
                                         # this "shifter" is useful if you want another level to be the first one on the barplot
                                         # this complements the Reverse order options.
                                         column(12,
                                                sliderInput(ns("fct_shift"), "Shift outcome levels:",
                                                            min = 0, max = 6, value = 0, step=1,
                                                            ticks=TRUE)
                                         )
                                       ) # end remove unknown/reverse levels/shift levels ---------
                             ))
             
             
      )
    ) #end_A   inputs end ----------------
    
  )
}

#' subset Server Function
#'
#' @noRd 
mod_subset_server <- function(input, output, session, r, alldata){
  ns <- session$ns
  
  
  
  # output$mytext <- renderText({
  #   paste(input$explanatory1, input$explanatory2, input$outcome)
  # })
  # 
  output$mytable <- renderTable({
    data_subset() %>% head()
  })
  
  
  ## update UI subset selections: update the list of levels within the variable to be subsetted from
  
  observe({
    subset_levels_update = alldata %>% dplyr::pull(input$subset_variable) %>% levels()
    
    updateSelectInput(session, "subset_levels",
                      choices = subset_levels_update,
                      selected = subset_levels_update
    )
  })	
  
  
  # subset data --------------------------
  summary_table         <- reactive({     
    # list requested variables
    expl1 = input$explanatory1
    expl2 = input$explanatory2
    outcome = input$outcome
    
    #if testing set input values here
    #expl1 = 'aaa.onepanel'
    #expl2 = 'aaa.onepanel'
    #outcome = 'price.quartiles'
    
    
    
    subdata = alldata %>%
      dplyr::filter(UQ(sym(input$subset_variable)) %in% input$subset_levels) %>% 
      dplyr::mutate_if(is.factor, forcats::fct_drop)
    
    
    subdata = subdata %>% 
      dplyr::select(expl1 = !!input$explanatory1,
                    expl2   = !!input$explanatory2,
                    outcome = !!input$outcome)
    
    if (input$rev_expl1){
      subdata$expl1 %<>%   fct_rev()
    }
    if (input$rev_expl2){
      subdata$expl2 %<>%   fct_rev()
    }
    if (input$rev_outcome){
      subdata$outcome %<>%  fct_rev()
    }
    
    # fct_shift can't handle a non-factor
    # as it uses nlevels before converting to factor I think
    subdata = subdata %>% 
      mutate(outcome  = outcome %>% 
               as_factor() %>% 
               fct_shift(input$fct_shift))
    
    
    # remove Missing or Unknown ------------

    if (input$rem_missing_expl1){
      subdata %<>% filter(expl1   != "Missing")
    }
    if (input$rem_missing_expl2){
      subdata %<>% filter(expl2   != "Missing")
    }
    if (input$rem_missing_outcome){
      subdata %<>% filter(outcome != "Missing")
    }
    if (input$rem_unknown_expl1){
      subdata %<>% filter(expl1   != "Unknown")
    }
    if (input$rem_unknown_expl2){
      subdata %<>% filter(expl2   != "Unknown")
    }
    if (input$rem_unknown_outcome){
      subdata %<>% filter(outcome != "Unknown")
    }
    
    summary_table = subdata %>% 
      dplyr::count(expl1, expl2, outcome) %>% 
      group_by(expl1, expl2) %>% 
      mutate(total = sum(n),
             relative = n/total) %>% 
      mutate(relative_label = if_else(relative < 0.1,
                                      scales::percent(relative, 0.1),
                                      scales::percent(relative, 1)))
    
    summary_table
    
  })
  
  
  
  
  return(summary_table)
  
  
}

## To be copied in the UI
# mod_subset_ui("subset_ui_1")

## To be copied in the server
# callModule(mod_subset_server, "subset_ui_1")