library(tidyverse)
library(glue)
library(ggtext)
library(extrafont)
# library(ggridges)

num_by_cat <- function(dataset, num_var, cat_var,
                       font_family = "Lato",
                       smooth_span = 0.3,
                       smooth_se   = FALSE) {
  
  my_theme <- theme_minimal() +
    theme(
      text               = element_text(family = font_family),
      plot.title         = element_text(size = 18, face = "bold"),
      plot.subtitle      = element_text(size = 14, color = "#36454f"),
      plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
      axis.text          = element_text(size = 12, color = "#36454f"),
      axis.title         = element_text(size = 12, face = "italic"),
      legend.position    = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  out <- list()
  
  out[["summary"]] <- dataset %>%
    group_by({{ cat_var }}) %>%
    summarize(
      min    = min({{ num_var }}, na.rm = T),
      mean   = mean({{ num_var }}, na.rm = T),
      median = median({{ num_var }}, na.rm = T),
      max    = max({{ num_var }}, na.rm = T),
      obs    = n(),
      miss   = sum(is.na({{ num_var }})),
      .groups = "drop_last"
    )
  
  out[["boxplot"]] <- dataset %>%
    drop_na({{ cat_var}}, {{ num_var }}) %>%
    ggplot(aes(x  = {{ cat_var }}, y = {{ num_var }})) +
    geom_boxplot() +
    labs(
      title = glue("Boxplot: {substitute(num_var)} by {substitute(cat_var)}")
    ) +
    my_theme
  
  # out[["ridgeplot"]] <- dataset %>%
  #   drop_na({{ cat_var}}, {{ num_var }}) %>%
  #   ggplot(aes(x = {{ cat_var }}, y = {{ num_var }})) +
  #   geom_density_ridges()
  
  return(out)
  
}


num_by_cat(penguins, bill_length_mm, species)


