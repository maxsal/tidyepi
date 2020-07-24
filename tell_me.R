library(tidyverse)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)

tell_me <- function(dataset, var, plots = TRUE,
                    font_family = "Lato",
                    percentiles = c(0.01, 0.05, 0.1, 0.25, 0.5,
                                    0.75, 0.9, 0.95, 0.99)) {
  
  if (!is.numeric(dataset %>% pull({{ var }}))) {
    
    print(glue("{var} is not numeric. Please provide a numeric variable."))
    
  } else {
    
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
    
    out[["summary"]] <- tibble(
      stat  = c("min", "mean", "median", "max", "sd", "var",
                "n_obs", "n_missing"),
      value = c(
        dataset %>% pull({{ var }}) %>% min(),
        dataset %>% pull({{ var }}) %>% mean(., na.rm = T),
        dataset %>% pull({{ var }}) %>% median(),
        dataset %>% pull({{ var }}) %>% max(),
        dataset %>% pull({{ var }}) %>% sd(),
        dataset %>% pull({{ var }}) %>% stats::var(),
        dataset %>% pull({{ var }}) %>% na.omit() %>% length(),
        dataset %>% pull({{ var }}) %>% is.na() %>% sum()
      )
    )
    
    out[["quantiles"]] <- quantile(dataset %>% pull({{ var }}),
                                   probs = percentiles) %>%
      t() %>% t() %>% as.data.frame() %>% rownames_to_column(var = "quantile") %>%
      rename(value = V1) %>% as_tibble() %>% mutate(quantile = gsub("%", "_pct", quantile))
    
    out[["ends"]] <- tibble(
      smallest_five = head(sort(dataset %>% pull({{ var }})), 5),
      largest_five  = tail(sort(dataset %>% pull({{ var }})), 5)
    )
    
    if (plots == TRUE) {
      
      out[["hist"]] <- dataset %>%
        ggplot(aes(x = {{ var }})) +
        geom_vline(xintercept = out[["summary"]] %>%
                     filter(stat == "mean") %>%
                     pull(value), linetype = 2, size = 1, color = "#eb4034") +
        geom_vline(xintercept = out[["summary"]] %>%
                     filter(stat == "median") %>%
                     pull(value), linetype = 2, size = 1, color = "#3434eb") +
        geom_histogram(bins = 30) +
        labs(
          title   = glue("Histogram of {substitute(var)}"),
          x       = glue("{substitute(var)}"),
          y       = "Count",
          caption = glue("**{out[['summary']] %>%
                               filter(stat == 'n_obs') %>%
                               pull(value)} observations** <br>",
                         "Red line is the mean. Blue line is the median.")
        ) + 
        my_theme
      
      out[["qq_plt"]] <- dataset %>%
        ggplot(aes(sample = {{ var }})) +
        stat_qq_line(alpha = 0.3) +
        stat_qq() +
        labs(
          title = glue("qq-plot for {substitute(var)}"),
          x     = "Theoretical",
          y     = "Sample"
        ) +
        my_theme
      
    }
    
    return(out)
    
  }
  
  
}