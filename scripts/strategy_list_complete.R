library(dplyr)

# all possible combinations
strategies <- expand.grid(
  ft_with_dose10 = c(T, F),
  ft_with_ice = c(T, F),
  mt_with_dose10 = c(T, F),
  mt_with_ice = c(T, F),
  dispatch = c("parallel", "asap")
) %>%
  #dynamically create the strategy names from their composition
  mutate(strategy = as.factor(sprintf(
    "%s_%s_%s",
    c("monodose", "mixed", "dose10")[1 + ft_with_dose10 + mt_with_dose10],
    c("occ", "pcc", "fcc")[1 + ft_with_ice + mt_with_ice],
    dispatch
  )))


