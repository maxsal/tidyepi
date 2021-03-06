library(tidyverse)
library(glue)

ct_2x2 <- function(x, dataset = NULL,
                   conf = 0.95) {
  
  out <- list()
  
  if (is.numeric(x) & length(x) == 4) { 
  
  a <- x[1]
  b <- x[2]
  c <- x[3]
  d <- x[4]
  
  y <- x
  
  } else if (is.character(x) & length(x) == 2) {
    
    if ( all(unique(dataset[[x[1]]]) == c(0, 1)) &
         all(unique(dataset[[x[2]]]) == c(0, 1))
         ) {
      
      tab <- table(dataset[[x[1]]], dataset[[x[2]]])
        
      a <- tab["1", "1"]
      b <- tab["1", "0"]
      c <- tab["0", "1"]
      d <- tab["0", "0"]
      
      y <- c(a, b, c, d)
      
    }
    
  }

  e <- a + b
  f <- c + d
  g <- a + c
  h <- b + d
  n <- a + b + c + d
  
  exposure <- c("yes", "yes", "no", "no")
  outcome  <- c("yes", "no", "yes", "no")
  
  out[["cross_tab"]] <- tibble::tibble(
    exposure,
    outcome,
    y
  ) %>%
    stats::xtabs(
      data = .,
      y ~ exposure + outcome
    )
  
  or <- (a * d) / (b * c)
  or_hi <- exp(log(or) + qnorm(conf + ((1 - conf)/2)) * sqrt((1/a) + (1/b) + (1/c) + (1/d)))
  or_lo <- exp(log(or) - qnorm(conf + ((1 - conf)/2)) * sqrt((1/a) + (1/b) + (1/c) + (1/d)))
  
  p0 <- c / f
  p1 <- a / e
  
  rd <- p1 - p0
  
  rr <- p1 / p0
  
  sensitivity <- a / g
  specificity <- d / h
  
  exp_a <- (e * g) / n
  exp_b <- (e * h) / n
  exp_c <- (f * g) / n
  exp_d <- (f * h) / n
  
  out[["stats"]] <- tibble(
    "stat" = c("odds_ratio", glue("or_{conf*100}_lo"), glue("or_{conf*100}_hi"), "risk_diff", "risk_ratio",
               "sensitivity", "specificity"),
    "value" = c(or, or_lo, or_hi, rd, rr, sensitivity, specificity)
  )
  
  out[["chisq_test"]] <- chisq.test(out[["cross_tab"]])
  
  return(out)
  
}
