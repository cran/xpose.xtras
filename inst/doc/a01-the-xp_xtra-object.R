## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align='center'
)

## ----setup, echo=FALSE, include = FALSE---------------------------------------
library(dplyr)
library(xpose)
library(xpose.xtras)

## ----conversion---------------------------------------------------------------
xpdb_converted <- xpdb_ex_pk %>%
  as_xpdb_x()

# To verify:
xpdb_ex_pk
xpdb_converted

## ----set_var_types------------------------------------------------------------
# Unset all example covariates
xpdb_ex_covs <- xp_var(xpdb_ex_pk, type = c("catcov","contcov"), .problem=1) %>% 
  pull(col)
xpdb_ex_covs
no_covs <- set_var_types(xpdb_ex_pk, .problem=1, na = xpdb_ex_covs)

# set_var_types on xpose_data objects uses xpose::set_var_types
set_var_types(no_covs, .problem=1, catcov = c("MED1","MED2")) %>%
  xp_var(type = c("catcov"), .problem=1) %>% 
  pull(col)
no_covs %>%
  as_xpdb_x() %>%
  set_var_types(catcov = starts_with("MED"), .problem=1) %>%
  xp_var(type = c("catcov"), .problem=1) %>% 
  pull(col)

## ----set_labels_units---------------------------------------------------------
w_unit_labs <- xpdb_x %>%
  set_var_labels(AGE="Age", MED1 = "Digoxin", .problem = 1) %>%
  set_var_units(AGE="yrs")
list_vars(w_unit_labs, .problem = 1)

## ----set_levels---------------------------------------------------------------
w_levels <- w_unit_labs  %>%
  set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin())
list_vars(w_levels, .problem = 1)

## ----plot_cont, fig.width=unit(6,"in"), fig.height=unit(3,"in")---------------
eta_vs_contcov(w_unit_labs,etavar=ETA1, quiet=TRUE)

## ----plot_cat, fig.width=unit(6,"in"), fig.height=unit(3,"in")----------------
eta_vs_catcov(w_levels,etavar=ETA1, quiet=TRUE)

## ----get_prm------------------------------------------------------------------
get_prm(pheno_final) %>%
  select(-c(fixed,m,n))

## ----get_prm2-----------------------------------------------------------------
pheno_final %>%
   add_prm_association(CLpkg~logit(IIVCL),Vpkg~logit(IIVV)) %>%
   get_prm() %>%
  select(-c(fixed,m,n))

## ----xp_themes, fig.width=unit(6,"in"), fig.height=unit(3,"in")---------------
favorite_theme <- xpose::theme_xp_xpose4() # stand-in for "custom" theme

eta_vs_catcov(w_levels,etavar=ETA1, quiet=TRUE)
eta_vs_catcov(w_levels,etavar=ETA1, quiet=TRUE, xp_theme = favorite_theme)


## ----get_props----------------------------------------------------------------
pheno_final %>% get_shk()
pheno_final %>% get_shk("eps")
pheno_final %>% get_prop("ofv")
pheno_final %>% get_prop("descr")

## ----describer----------------------------------------------------------------
pheno_final %>% desc_from_comments() %>% get_prop("descr")

## ----process_preset-----------------------------------------------------------
add_process_preset(
  ~ .x %>% as_xpdb_x() %>% set_var_types(na = any_of(c("SEX", "MED1", "MED2")), .problem = 1),
  name = "drop_covariates"
)
print_process_preset()

# before: SEX/MED1/MED2 are categorical covariates
list_vars(xpdb_ex_pk, .problem = 1)

# after: they've been reassigned to "na" (not attributed) by the preset
xpdb_ex_pk %>%
  process_preset("drop_covariates") %>%
  list_vars(.problem = 1)

## ----process_preset_amend-----------------------------------------------------
amend_process_preset(
  "drop_covariates",
  ~ .x %>% as_xpdb_x() %>% set_var_types(na = any_of(c("SEX", "MED1")), .problem = 1)
)
remove_process_preset("drop_covariates")

## ----process_preset_persist, eval = FALSE-------------------------------------
# # only meaningful in an actual interactive session
# add_process_preset(
#   ~ .x %>% as_xpdb_x() %>% set_var_types(na = any_of(paste0("ETA", 5:9))),
#   name = "drop_higher_etas",
#   persist = TRUE
# )

