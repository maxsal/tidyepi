# Welcome to `tidyepi`!!!

I'm creating a repository of scripts that I've created to help :mag: explore data in `R` using `tidyverse`.

Currently, I am working on functions related to quick 2x2 contingency table analysis and single-variable summaries.

Let me know what you think!
* :bird: Tweet [@MaxSalTweets](https://twitter.com/MaxSalTweets)
* :inbox_tray: [mmsalva@umich.edu](mailto:mmsalva@umich.edu)

:warning: **These are works in progress and will possibly contain errors!** :warning:

## `ct_2x2` for 2x2 contigency tables

Currently, this function takes a four number vector `c(a, b, c, d)` that corresponds to the following 2x2 table set-up:

```
        outcome
exposure no yes
     no   d   c
     yes  b   a
```

It can also take two dummy variables that are coded 0/1. (e.g., `ct_2x2(x = c("var1", "var2"), dataset = dat)`)

It will do the following in a list:
 * `cross_tab`: reproduce the above 2x2 table
 * `stats`: a tibble with OR (95% CI), RR, RD, sensitivity, and specificity estimates
 * `chisq_test`: results from `chisq.test`


## `univar` for quick single-variable summaries

Currently, this function takes a numeric variable and outputs the following in a list:

* `summary`: tibble of min, mean, median, max, standard deviation, variance, number of non-missing observations, and number of missing observations
* `quantiles`: tibble of the following percentiles: 1, 5, 10, 25, 50, 75, 90, 95, 99
* `ends`: tibble of the 5 smallest and 5 largest observations
* `hist`: histogram of variable with mean and median lines and number of non-missing observations in caption
* `qq_plt`: qqplot of variable

It takes the following arguments `univar(dataset, var, plots = TRUE)`. In `tidyverse`-style, this can be piped such that:
```
dataset %>%
  univar(var)
```
