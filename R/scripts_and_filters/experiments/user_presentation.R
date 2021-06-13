# Set seed ----
set.seed(14352)

# Load maars code ----
#install.packages("pak")
#pak::pkg_install("shamindras/maars")
#library("maars")

# Load standard libraries ----
library(tidyverse)

# generate data and fit model ----
n <- 1e3
x <- runif(n, -10, 10)
y <- 2 * x + sapply(sqrt(abs(x)), rnorm, n = 1, mean = 0)
df <- tibble(x, y)

# plot the data sample ----
# add CI for residuals
grid_x <- unique(x)
res_up <- 2 * x + 1.96 * sqrt(abs(grid_x))
res_down <- 2 * x - 1.96 * sqrt(abs(grid_x))
res_df <- tibble(x = grid_x, res_up, res_down)

# plot #1
df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(col = 'grey') +
    theme_bw()
# plot #2
df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(col = 'grey') +
    theme_bw() +
    geom_abline(slope = 2, col = 'blue', size = 1.15) #+
    geom_line(data = res_df, aes(x = x, y = res_up), col = 'red') +
    geom_line(data = res_df, aes(x = x, y = res_down), col = 'red')


# inference based on lm ----
lm_fit <- lm(y ~ 0 + x)
confint(lm_fit)
summary(lm_fit)

# maars on sample data ----

# compute estimates of the variance based on sandwich and bootstraps
maars_fit <- lm_fit %>%
    comp_var(boot_emp = list(B = 1e3),
             boot_mul = list(B = 1e3),
             boot_res = list(B = 1e3))

# methods for showing maars results
print(maars_fit)
summary(maars_fit)

# tidy analogues of summary
get_summary(maars_fit)

# plot one of the maars diagnostics
maars_fit %>%
    get_plot() %>%
    pluck('p7') +
    geom_hline(yintercept = 2,
               linetype = 'dashed',
               col = 'black') +
    ylim(1.95, 2.05)


# get coverage for well specified model over 10000 replications ------
conf <- c()
for(i in 1:1e4){
    x <- runif(n,-10,10)
    y <- 2 * x + unlist(lapply(sqrt(abs(x)), rnorm, n = 1, mean = 0))
    lm_fit <- lm(y ~ 0 + x) %>%
        confint()
    conf <- rbind(conf, lm_fit)
}

low <- if_else(conf[,1] -  2 < 0, 1 , 0)
high <- if_else(conf[,2] -  2 > 0, 1 , 0)

covers <- pmin(low, high)
mean(covers)
