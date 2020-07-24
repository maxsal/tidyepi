library(tidyverse)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)

my_theme <- theme_minimal() +
  theme(
    text               = element_text(family = "Lato"),
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

tell_me <- function(dataset, var) {
  
  if (!is.numeric(dataset[[var]])) {
    
    print(glue("{var} is not numeric. Please provide a numeric variable."))
    
  } else {
    
    out <- list()
    
    out[["summary"]] <- summary(dataset[[var]])
    
    out[["quantiles"]] <- quantile(dataset[[var]],
                                   probs = c(0.01, 0.05, 0.1, 0.25,
                                             0.5,
                                             0.75, 0.9, 0.95, 0.99))
    
    out[["smallest_five"]] <- head(sort(dataset[[var]]), 5)
    out[["largest_five"]]  <- tail(sort(dataset[[var]]), 5)
    out[["n_obs"]]         <- length(na.omit(dataset[[var]]))
    out[["n_missing"]]     <- sum(is.na(dataset[[var]]))
    out[["mean"]]          <- mean(dataset[[var]], na.rm = T)
    out[["sd"]]            <- sd(dataset[[var]], na.rm = T)
    out[["var"]]           <- var(dataset[[var]], na.rm = T)
    
    out[["hist"]] <- dataset %>%
      ggplot(aes(x = .data[[var]])) +
      geom_vline(xintercept = out[["mean"]], linetype = 2, size = 1, color = "#eb4034") +
      geom_vline(xintercept = out[["quantiles"]]["50%"], linetype = 2, size = 1, color = "#3434eb") +
      geom_histogram(bins = 30) +
      labs(
        title   = glue("Histogram of {var}"),
        x       = glue("{var}"),
        y       = "Count",
        caption = glue("**{out[['n_obs']]} observations** <br>",
                       "Red line is the mean. Blue line is the median.")
      ) + 
      my_theme
    
    out[["qq_plt"]] <- dataset %>%
      ggplot(aes(sample = .data[[var]])) +
      stat_qq_line(alpha = 0.3) +
      stat_qq() +
      labs(
        title = glue("qq-plot for {var}"),
        x     = "Theoretical",
        y     = "Sample"
      ) +
      my_theme
    
    return(out)
    
  }
  
  
}