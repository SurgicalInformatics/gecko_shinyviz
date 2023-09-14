#' Produce a summary table for the 3 variables
#'
#' @param subset data frame with variables necessary for scatterplot
#'
#' @return tibble object for the scatterplot
#' @export
#'
#' @examples
#' summary_table <- scatter_sales(data = ames, xvar = "Lot_Frontage", yvar = "Sale_Price")
#' plot_obj
create_summary = function(subdata){

  summary_table = subdata %>% 
    dplyr::count(expl1, expl2, outcome) %>% 
    group_by(expl1, expl2) %>% 
    mutate(total = sum(n),
           relative = n/total) %>% 
    mutate(relative_label = if_else(relative<99,
                                    signif(relative, 2) %>% formatC(digits=2, format='fg'),
                                    signif(relative, 4) %>% formatC(digits=4, format='fg')) %>%
             paste0('%')
    )

  summary_table
  
}
