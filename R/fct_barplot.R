#' Produce a barplot (geom_col) from the summary_table
#'
#' @param subset data frame with variables necessary for scatterplot
#'
#' @return tibble object for the scatterplot
#' @export
#'
#' @examples
#' plot_obj <- create_barplot(subdata)
#' plot_obj
#' 

create_head = function(subdata){
  subdata %>% head()
}

create_barplot = function(summary_table,
                          axis_relative = FALSE,
                          mypalette = "Paired",
                          my_ncol = 2,
                          perc_label = TRUE,
                          black_white = "white",
                          plot_labels){
  
 
  
  # testing:
  # subdata = select(appdata, expl1 = ALL, expl2 = ALL, outcome = mort)
  # axis_relative = FALSE
  # barplot_type = "stack"
  # mypalette = "Paired"
  # my_colour_order = -1
  # my_ncol = 2
  # perc_label = TRUE
  # black_white = "white"
  #plot_labels = list(expl1 = "", expl2 = "", outcome = "")
  if (axis_relative){
    barplot_type = "fill"
  }else{
    barplot_type = "stack"
  }

  p = ggplot(summary_table, aes(x=expl1, fill = outcome, y=n))+
    geom_col(position = barplot_type) +
    # If expl2 label missing, no need to print the semicolon in the panel strip:
    facet_wrap(~paste(plot_labels$expl2, expl2, sep = if_else(plot_labels$expl2 == "", "", ": ")), ncol=1)+
    coord_flip() +
    theme(
      strip.background = element_rect(fill = "white", colour = "grey50", size = 0.2),
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill=NA, linetype = 'solid', colour = "grey50"),
      #panel.margin = unit(2, "lines"),
      plot.margin = unit(c(2, 2, 2, 2), 'lines'),
      panel.grid.major.x = element_line(colour = "grey90", size = 0.2),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size=12),
      axis.text.x = element_text(size=12, vjust=0.7, colour='black'),
      axis.text.y = element_text(size=12, colour='black'),
      axis.title = element_text(size=14),
      #axis.title.y = element_blank(),
      legend.justification=c(1,0),
      legend.position='top',
      legend.title=element_text(size=12),
      legend.text=element_text(size=12)
    ) +
    ylab("Patients") +
    xlab(plot_labels$expl1) +
    scale_fill_brewer(plot_labels$outcome, palette = mypalette, direction = -1)+
    guides(fill = guide_legend(ncol = my_ncol, reverse = TRUE))

  summary_table$outcome = fct_drop(summary_table$outcome)
  
  # get first or last label for plot
  first_outcome = levels(summary_table$outcome)[1]
  last_outcome = levels(summary_table$outcome) %>% tail(1)
  first_only = summary_table %>%
    filter(outcome == first_outcome)
  last_only = summary_table %>%
    filter(outcome == last_outcome)
  
  if (barplot_type == "fill"){
    p = p+scale_y_continuous(expand = c(0, 0), label = scales::percent, breaks = 0:5/5)
    
  }else{
    p = p+ scale_y_continuous(expand = c(0, 0))
  }
  
  if (perc_label){
    p = p+geom_text(data = last_only, aes(label=relative_label), y=0.01, size=6, hjust=0,
                    colour=black_white)
  }
  

  return(p)
  
  
}


