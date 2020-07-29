library(tidyverse)
library(glue)
library(ggtext)
library(extrafont)

num_by_num <- function(dataset, var1, var2,
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
  
  tmp_corr <- cor.test(dataset %>% pull({{ var1 }}), dataset %>% pull({{ var2 }}),
                       use    = "complete.obs",
                       method = "pearson")

  out[["summary"]] <- tibble(
    correlation = tmp_corr$estimate,
    p_value     = tmp_corr$p.value,
    method      = tmp_corr$method
    )
  
  n_complete <- dim(dataset)[1] - (dim(dataset)[1] - dim(dataset %>% drop_na())[1])
  n_missing  <- dim(dataset)[1] - dim(dataset %>% drop_na())[1]

  out[["plot"]] <- dataset %>%
    drop_na() %>%
    ggplot(aes(x  = {{var1}}, y = {{ var2 }})) +
    geom_point() +
    geom_smooth(method = "loess", formula = "y ~ x", span = smooth_span, se = smooth_se) +
    labs(
      title = glue("Scatterplot: {substitute(var1)} by {substitute(var2)}"),
      captions = glue("Observations: {n_complete}; missing: {n_missing}; span: {smooth_span}")
    ) +
    my_theme

  return(out)

}