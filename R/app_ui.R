#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library(shinyBS)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      h1("GECKO Data Explorer"),
      column(4,
             mod_subset_ui("subset_ui_1", allvars = allvars, alldata = appdata),
             mod_barplot_ui("barplot_ui_1")
             
      ),
      column(8,
             p(paste("Preliminary data updated on", updated_date, ":", nrow(appdata), "patient records")),
             mod_render_barplot_ui("render_barplot_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  # add_resource_path(
  #   'www', app_sys('app/www')
  # )
  # 
  # tags$head(
  #   favicon(),
  #   bundle_resources(
  #     path = app_sys('app/www'),
  #     app_title = 'covidviz'
  #   )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert() 
  #)
}
