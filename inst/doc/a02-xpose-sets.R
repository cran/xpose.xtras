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

## ----set_setup----------------------------------------------------------------
xpose_set(pheno_base, pheno_final, pheno_saem)
xpose_set(base=pheno_base, reparam=pheno_final, reparam_saem=pheno_saem)

## ----pheno_set_diagram--------------------------------------------------------
diagram_lineage(pheno_set) %>%
  DiagrammeR::render_graph(layout="tree")

## ----pheno_decon--------------------------------------------------------------
phrun3 <- pheno_set$run3$xpdb
phrun5 <- pheno_set$run5$xpdb
phrun6 <- pheno_set$run6$xpdb
phrun7 <- pheno_set$run7$xpdb
phrun8 <- pheno_set$run8$xpdb
phrun9 <- pheno_set$run9$xpdb
pheno_stem <- xpose_set(phrun3,phrun5,phrun6, .as_ordered = TRUE)
pheno_stem
diagram_lineage(pheno_stem) %>%
  DiagrammeR::render_graph(layout="tree")
pheno_branch <- xpose_set(phrun6,phrun7,phrun8,phrun9, .relationships = c(phrun7+phrun8+phrun9~phrun6))
pheno_branch
diagram_lineage(pheno_branch) %>%
  DiagrammeR::render_graph(layout="tree")

## ----pheno_concat-------------------------------------------------------------
pheno_tree <- pheno_stem %>% 
  # drop phrun6 from stem
  select(-phrun6) %>%
  c(
    pheno_branch,
    .relationships = c(phrun6~phrun5)
  )
pheno_tree
diagram_lineage(pheno_tree) %>%
  DiagrammeR::render_graph(layout="tree")

## ----diff---------------------------------------------------------------------
diff(pheno_set)

## ----diff_lineage-------------------------------------------------------------
tbl_diff <- function(set) tibble(
  models = xset_lineage(set),
  diff = c(0,diff(set))
)
tbl_diff(pheno_set)
pheno_set %>%
  remove_relationship(run9~run6) %>%
  tbl_diff()
pheno_set %>%
  set_base_model(run6) %>%
  tbl_diff()
tibble(
  models = xset_lineage(pheno_set,run6),
  diff = c(0,diff(pheno_set,run6))
)

## ----diff_list----------------------------------------------------------------
diff(pheno_set, run10,run9)
xset_lineage(pheno_set, run10,run9)

## ----shark, fig.width=unit(7,"in"), fig.height=unit(5,"in")-------------------
pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  shark_plot(run6, run9, quiet = TRUE)

## ----exposed------------------------------------------------------------------
pheno_set %>%
  expose_property(ofv) %>%
  expose_param(ome1) %>%
  reshape_set() %>%
  head()

## ----verbs--------------------------------------------------------------------
pheno_set %>%
  select(run3,run15) %>%
  names()
pheno_set %>%
  # Note renaming can affect parentage.
  # For simplicity, this method does not change 
  # parent automatically in child
  rename(NewName = run3) %>%
  names()
pheno_set %>%
  expose_property(ofv) %>%
  filter(..ofv < 700) %>%
  names()
pheno_set %>%
  expose_param(ome1) %>%
  pull(..ome1)

## ----focus--------------------------------------------------------------------
focus_test <- pheno_set %>%
  focus_xpdb(run3,run15) %>%
  mutate(test_col = 1) %>%
  unfocus_xpdb()
tail(names(get_data(focus_test$run6$xpdb, quiet=TRUE)))
tail(names(get_data(focus_test$run3$xpdb, quiet=TRUE)))

