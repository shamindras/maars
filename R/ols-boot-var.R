

multiplier_single_bootstrap <- function(n, J_inv_X_res){

    #e <- matrix(rnorm(n, 0, 1), ncol = 1)
    e <- matrix(rep(0.1, n), ncol = 1)

    out <- t(J_inv_X_res)%*%e
    # e <- rnorm(n,0,1)
    #
    #
    # purrr::map2(
    #     .x = e,
    #     .y = J_inv_X_res,
    #     ~ t(.x*.y)
    # ) %>%
    #     do.call(rbind, .) %>%
    #          apply(., 2, mean)
    # out <- as.list(1:n) %>% purrr::map(~ t(e[.x]*J_inv%*%X[.x,]*res[.x])) %>%
    #     do.call(rbind, .) %>%
    #     apply(., 2, mean) %>%
    #     matrix()

    return(out)
}

multiplier_bootstrap <- function(lm_fit, B = 1000) {
    # this is wrong for some reason
    J_inv <- summary.lm(lm_fit)$cov.unscaled
    X <- qr.X(lm_fit$qr)
    res <- residuals(lm_fit)

    iter_list <- as.list(1:B)
    names(iter_list) <- paste0("Iter_", 1:B)
print('asda')
    J_inv_X_res <- 1:nrow(X) %>% purrr::map(~ t(J_inv%*%X[.x,]*res[.x])) %>%
        do.call(rbind, .)

    print('dddd')

    out <- 1:B %>% purrr::map(~ multiplier_single_bootstrap(nrow(X), J_inv_X_res)) %>%
        do.call(rbind, .) %>%
        matrix(., byrow = T, ncol = ncol(X))
    colnames(out) <- colnames(qr.X(lm_fit$qr))

    return(out)
}

n <- 100
X <- stats::rnorm(n, 0, 1)
y <- 2 + X * 1 + stats::rnorm(n, 0, 10)
lm_fit <- stats::lm(y ~ X)

set.seed(10)
multiplier_bootstrap(lm_fit, B = 1)

J_inv <- summary.lm(lm_fit)$cov.unscaled
X <- qr.X(lm_fit$qr)
value <- 0
e <- matrix(rep(0.1, n), ncol = 1)
for(i in 1:n){
    value <- value+e[i]*J_inv%*%X[i,]*residuals(lm_fit)[i]
}
value/n



J_inv <- summary.lm(lm_fit)$cov.unscaled
X <- qr.X(lm_fit$qr)
set.seed(1)
e <- rnorm(nrow(X),0,1)
res <- residuals(lm_fit)

start.time <- Sys.time()
as.list(1:nrow(X)) %>% purrr::map(~ t(e[.x]*J_inv%*%X[.x,]*res[.x])) %>%
    do.call(rbind, .) %>%
    apply(., 2, mean)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

value <- 0
for(i in 1:n){
    value <- value+e[i]*J_inv%*%X[i,]*residuals(lm_fit)[i]
}
value/n

gamma <- matrix(e*residuals(lm_fit), ncol = 1)
# #gamma2 <- Matrix::Diagonal(x = e)%*%residuals(lm_fit)
# J_inv_block <- Matrix::bdiag(replicate(nrow(X), J_inv, simplify=FALSE))
# vec_X <- matrix(t(X), ncol = 1)

start.time <- Sys.time()
X_list <- as.list(1:nrow(X)) %>% purrr::map(~ X[.x,])
X_list2 <- X_list %>% purrr::map(~ J_inv%*%.x)
purrr::map2(.x=X_list2, .y=gamma, ~ t(.x*.y)) %>%
    do.call(rbind, .) %>%
    apply(., 2, mean)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# vec_X <- matrix(t(X), ncol = 1)
# diag(gamma)%*%
# t(gamma)%*%(J_inv_block%*%vec_X)
# G <- as.vector(gamma)
# G2 <- G %>% purrr::map(~ .x*diag(ncol(X))) %>% Matrix::bdiag(.)
# G2%*%(J_inv_block%*%vec_X)
# apply(matrix(G2%*%(J_inv_block%*%vec_X), byrow = F, ncol = 2), 2, mean)
# # as.matrix(rnorm(nrow(X), 0, 1) * residuals(lm_fit), ncol = 1)
# e <- as.matrix(coef(lm_fit),ncol=1)
# e + 1 / nrow(X) * J_inv %*% t(X) %*% e * residuals(lm_fit)

