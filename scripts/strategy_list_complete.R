library(dplyr)

# all possible combinations
strategies <- tibble(
  ft_with_dose10 = rep(c(T, T, T, T, F, F), times = 2),
  ft_with_ice = rep(c(T, F, T, T, T, F), times = 2),
  mt_with_dose10 = rep(c(T, T, F, F, F, F), times = 2),
  mt_with_ice = rep(c(T, F, T, F, T, F), times = 2),
  dispatch = rep(c("parallel", "asap"), each = length(ft_with_dose10)/2)
) %>%
  #dynamically create the strategy names from their composition
  mutate(strategy = as.factor(sprintf(
    "%s_%s_%s",
    c("monodose", "mixed", "dose10")[1 + ft_with_dose10 + mt_with_dose10],
    c("occ", "pcc", "fcc")[1 + ft_with_ice + mt_with_ice],
    dispatch
  )))


