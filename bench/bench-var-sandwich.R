set.seed(168465)

# Number of decimal places to round the variance estimators
NUM_DEC_PL <- 7

# Function to create the sample linear regression simulated data
create_lm_fit <- function(n, p) {
    Sigma <- diag(p)
    betas <- seq.int(from = 1, by = 1, length.out = p)
    X <- MASS::mvrnorm(n = n, rep(0, p), Sigma)
    y <- 2 + X %*% betas + stats::rnorm(n, 0, 10)
    return(stats::lm(y ~ X))
}

results <- bench::press(
    n = seq.int(from = 1000, by = 1000, length.out = 4),
    p = seq.int(from = 1, by = 1, length.out = 5),
    {
        lm_fit <- create_lm_fit(n, p)
        bench::mark(
            min_iterations = 50,
            qr_var = unname(round(maar::comp_sandwich_qr_var(lm_fit),
                                  NUM_DEC_PL)),
            sandwich_sandpkg_var = unname(round(sandwich::sandwich(lm_fit),
                                                NUM_DEC_PL))
        )
    }
)

# Replicate autoplot code from bench [R] package
# Source: https://github.com/r-lib/bench/blob/master/R/autoplot.R
summary_cols <- c("min", "median", "itr/sec", "mem_alloc", "gc/sec")
data_cols <- c("n_itr", "n_gc", "total_time", "result", "memory", "time", "gc")
object <- results
res <- tidyr::unnest(object, c(time, gc))
res <- res %>% dplyr::filter(p == 5)
p <- ggplot2::ggplot(res)
p <- p +
    ggplot2::aes_string("expression", "time", color = "gc") +
    ggbeeswarm::geom_quasirandom() +
    ggplot2::coord_flip()

parameters <- setdiff(
    colnames(object),
    c("expression",
      summary_cols, data_cols,
      c("level0", "level1", "level2")))

p +
    ggplot2::facet_grid(
        paste0(parameters[[1]], "~", parameters[[2]]),
        labeller = ggplot2::label_both) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.x = element_blank(),
                   strip.text.y = element_text(size = 12)) +
    ggplot2::ylim(0, 0.05) +
    labs(title = "Sandwich Est. Benchmark: p = 5, n = (1000,...,5000)",
         y = "Time (ms)",
         x = "Sandwich Estimator Type")

ggplot2::ggsave(filename = here::here("bench", "figures",
                                      "rbench_p_5_n_1k_to_5k.png"))
