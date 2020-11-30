# multiplier_boot_dat <- function(lm_fit) {
#     J_inv <- summary.lm(lm_fit)$cov.unscaled
#     X <- qr.X(lm_fit$qr)
#     res <- residuals(lm_fit)
#     n <- length(res)
#     return(list("J_inv" = J_inv, "X" = X, "res" = res, "n" = n))
# }
#
# multiplier_single_bootstrap3 <- function(n, J_inv, res, e) {
#     out <- as.list(1:n) %>%
#         purrr::map(~ t(e[.x] * J_inv %*% X[.x, ] * res[.x])) %>%
#         do.call(rbind, .) %>%
#         apply(., 2, mean)
#     return(out)
# }
#
# multiplier_bootstrap3 <- function(lm_fit, B = 100) {
#     J_inv <- summary.lm(lm_fit)$cov.unscaled
#     X <- qr.X(lm_fit$qr)
#     res <- residuals(lm_fit)
#     n <- nrow(X)
#     print(class(X))
#     print(X[1, ])
#     print(dim(J_inv))
#
#     e <- 1:B %>% purrr::map(~ rnorm(n, 0, 1))
#
#     dist <- 1:B %>%
#         purrr::map(~ multiplier_single_bootstrap3(n, J_inv, res, e[[.x]])) %>%
#         do.call(rbind, .)
#
#     out <- tibble(
#         iteration = rep(1:B, ncol(X)),
#         term = rep(colnames(X), each = B),
#         estimate = as.numeric(dist)
#     ) %>%
#         arrange(iteration, term)
#
#     return(out)
# }
#
# #### EXAMPLE
# n <- 1e2
# X <- stats::rnorm(n, 0, 1)
# y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
# lm_fit <- stats::lm(y ~ X)
# multiplier_bootstrap(lm_fit, B = 15)
# multiplier_bootstrap2(lm_fit, B = 15)
# multiplier_bootstrap3(lm_fit, B = 15) # this is not working for some reason
#
# ############# -- all code for examples
# n <- 1e2
# X <- stats::rnorm(n, 0, 1)
# y <- 2 + X * 1 + stats::rnorm(n, 0, 1)
# lm_fit <- stats::lm(y ~ X)
# J_inv <- summary.lm(lm_fit)$cov.unscaled
# res <- residuals(lm_fit)
# X <- qr.X(lm_fit$qr)
# J_inv_X_res <- 1:nrow(X) %>% purrr::map(~ t(J_inv %*% X[.x, ] * res[.x]))
# value <- 0
#
# e <- matrix(rnorm(n, 0, 1), ncol = 1)
# e <- rnorm(n, 0, 1)
# # value_store <- c()
# # for (i in 1:n) {
# #   #value <- value + e[i] * J_inv %*% X[i, ] * residuals(lm_fit)[i]
# #   value_store <- rbind(value_store,
# #                       t(e[i] * J_inv %*% X[i, ] * residuals(lm_fit)[i]))
# # }
# #
# # start <- Sys.time(); d <- apply(value_store, 2, mean); print(Sys.time()-start)
#
# start <- Sys.time()
# a <- multiplier_single_bootstrap3(length(res), J_inv, res, e)
# print(Sys.time() - start)
# start <- Sys.time()
# b <- multiplier_single_bootstrap2(nrow(X), J_inv_X_res, e)
# print(Sys.time() - start)
# start <- Sys.time()
# c <- multiplier_single_bootstrap(n, J_inv_X_res %>%
#                                      do.call(rbind, .), e)
# print(Sys.time() - start)
# a
# b
# c
#
# start <- Sys.time()
# a <- multiplier_bootstrap3(lm_fit, B = 15)
# print(Sys.time() - start)
# start <- Sys.time()
# b <- multiplier_bootstrap2(lm_fit, B = 15)
# print(Sys.time() - start)
# start <- Sys.time()
# a <- multiplier_bootstrap(lm_fit, B = 15)
# print(Sys.time() - start)
# a
#
# end_time <- c()
# seq_B <- seq(5, 100, by = 10)
# for (i in seq_B) {
#     print(i)
#     start <- Sys.time()
#     a <- multiplier_bootstrap(lm_fit, B = i)
#     end_time <- c(end_time, Sys.time() - start)
#     print(end_time[i])
# }
# plot(seq_B, end_time)
