get_est <- function(emmeans, digits = 3) {
  tmp <- as_tibble(emmeans)
  paste0(
    formatC(tmp[["prob"]], digits = digits, format = "f"), 
    " [",
    formatC(tmp[[ncol(tmp)-1]], digits = digits, format = "f"), 
    ",  ",
    formatC(tmp[[ncol(tmp)]], digits = digits, format = "f"), 
    "]"
  )
}

fit_combined_formula <- function(covariate, formula_fixed, formula_random, 
                                 data, by = "+", cl = NULL, all_fit = FALSE) {
  newf <- formula(paste(deparse(formula_fixed), by, covariate, formula_random))
  mixed(newf, data, method = "LRT", family = binomial, 
        progress = FALSE, cl = cl, all_fit = all_fit)
}

get_z <- function(x) {
  (x - mean(x))/sd(x)
}