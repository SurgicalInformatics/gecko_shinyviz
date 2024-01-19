#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 
library(dplyr)
library(pins)
library(magrittr)
board = board_connect()
appdata      = pin_read(board, "rots/gecko_appdata")
updated_date = pin_meta(board, "rots/gecko_appdata")$created

allvars = appdata %>%
  finalfit::extract_labels() %>%
  select(vname, vfill) %$%
  setNames(as.list(vname), vfill)

# set missing BDIs to No as we away 1-year results
appdata = appdata %>% 
  mutate(bdi_yn = if_else(bdi_yn == "Missing", "No", bdi_yn))

app_server <- function( input, output, session ) {
  # List the first level callModules here
  library(dplyr)
  library(forcats)
  library(tidyr)
  library(ggplot2)
  library(magrittr)
  
  r <- reactiveValues()

  
  #data_subset <- callModule(mod_subset_server, "subset_ui_1", r, alldata = appdata)
  #data_subset <- callModule(mod_subset_server, "subset_ui_1", r, alldata = cs_data_app)
  #summary_table <- callModule(mod_subset_server, "subset_ui_1", r, alldata = bmjdata)
  summary_table <- callModule(mod_subset_server, "subset_ui_1", r, alldata = appdata)
  #callModule(mod_display_server, "display_ui_1", dataset = cs_data_app)
  barplot = callModule(mod_barplot_server, "barplot_ui_1", r, summary_table = summary_table)
  callModule(mod_render_barplot_server, "render_barplot_ui_1", r, summary_table = summary_table, barplot = barplot)
  
}