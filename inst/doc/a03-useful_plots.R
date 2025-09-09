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

## ----modavg, fig.width=unit(7,"in"), fig.height=unit(5,"in")------------------
pheno_set %>%
  ipred_vs_idv_modavg(auto_backfill = TRUE, quiet=TRUE)

## ----m3, fig.width=unit(7,"in"), fig.height=unit(5,"in")----------------------
described_pkpd_m3 <- pkpd_m3 %>%
  # Need to ensure var types are set
  set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
  # Set probs ("LIKE is the probability tht BLQ is 1")
  set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
  # Optional, but useful to set levels
  set_var_levels(1, BLQ = lvl_bin())
described_pkpd_m3 %>%
  catdv_vs_dvprobs(quiet=TRUE)

## ----vismo, fig.width=unit(7,"in"), fig.height=unit(5,"in")-------------------
vismo_xpdb <- vismo_pomod  %>%
  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
vismo_xpdb %>%
  catdv_vs_dvprobs(quiet=TRUE)
vismo_xpdb %>%
  catdv_vs_dvprobs(cutpoint = 2, quiet=TRUE)
vismo_xpdb %>%
  catdv_vs_dvprobs(cutpoint = 3, quiet=TRUE)

## ----roc_ex1, fig.width=unit(7,"in"), fig.height=unit(5,"in")-----------------
described_pkpd_m3 %>%
  roc_plot(quiet=TRUE)
vismo_xpdb %>%
  # Epsilon shrinkage is still included in default subtitle for M3-like use cases
  roc_plot(cutpoint = 2, quiet=TRUE, subtitle = "Ofv: @ofv") 

## ----roc_ex2, fig.width=unit(7,"in"), fig.height=unit(5,"in")-----------------
described_pkpd_m3 %>%
  roc_plot(quiet=TRUE, group="ID", type="pt")

## ----waterfall, fig.width=unit(7,"in"), fig.height=unit(5,"in")---------------
pheno_set %>%
  eta_waterfall(run3,run6, quiet=TRUE)

## ----waterfall2, fig.width=unit(7,"in"), fig.height=unit(5,"in")--------------
pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  iofv_waterfall(run3,run6, quiet=TRUE)

## ----ofv_change, fig.width=unit(7,"in"), fig.height=unit(5,"in")--------------
iofv_vs_mod(pheno_set, auto_backfill = TRUE, quiet=TRUE)

