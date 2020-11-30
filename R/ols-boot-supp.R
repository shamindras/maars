# # other functions -- idea for how to process the output of the boostrap multipliers above
# # we should add another method for this
# boot_conf_int <- function(coef_info, alpha = 0.05) {
#     # "stolen from int_pctl and pctl_single from rsample
#     coef_info %>%
#         group_by(term) %>%
#         summarise(
#             lower = quantile(estimate, prob = alpha / 2, na.rm = TRUE),
#             estimate = mean(estimate, na.rm = TRUE),
#             upper = quantile(estimate, prob = 1 - alpha / 2, na.rm = TRUE),
#             alpha = alpha,
#             method = "percentile"
#         )
# }
#
# boot_conf_int(multiplier_bootstrap(lm_fit, B = 15))
#
#
