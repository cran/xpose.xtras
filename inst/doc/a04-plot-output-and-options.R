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

## ----default_labs_option------------------------------------------------------
options(xpose.xtras.default_labs = list(caption = "DRAFT: do not distribute"))

p <- dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)
p_labelled <- apply_default_labs(p)
p_labelled

## ----default_labs_overwrite---------------------------------------------------
apply_default_labs(p, caption = "Run @run, @nobs observations", overwrite = TRUE)
options(xpose.xtras.default_labs = NULL)

## ----default_labs_xpdb--------------------------------------------------------
xpdb_draft <- xpdb_x %>%
  set_default_labs(caption = "Model @run, interim, subject to change")

dv_vs_ipred(xpdb_draft, quiet = TRUE) %>%
  apply_default_labs(xpdb = xpdb_draft, overwrite = TRUE)

## ----default_labs_direct------------------------------------------------------
dv_vs_ipred(xpdb_draft, quiet = TRUE) %>%
  apply_default_labs(caption = "Final for submission", xpdb = xpdb_draft, overwrite = TRUE)

## ----watermark_basic----------------------------------------------------------
p %>%
  add_watermark()

## ----watermark_custom---------------------------------------------------------
p %>%
  add_watermark(label = "PRELIMINARY", colour = "firebrick", alpha = 0.15, angle = 20)

## ----watermark_facets---------------------------------------------------------
dv_vs_ipred(xpdb_ex_pk, quiet = TRUE, facets = "SEX") %>%
  add_watermark(label = "DRAFT")

## ----watermark_angle_unity_default--------------------------------------------
pred_plot <- dv_vs_pred(xpdb_ex_pk, quiet = TRUE)

pred_plot %>%
  add_watermark(alpha = 0.6)

## ----watermark_angle_unity_adjusted-------------------------------------------
pred_plot %>%
  add_watermark(alpha = 0.6, angle = -45)

## ----watermark_option---------------------------------------------------------
options(xpose.xtras.default_watermark = list(label = "CONFIDENTIAL", colour = "firebrick"))
p %>%
  add_watermark()
options(xpose.xtras.default_watermark = NULL)

## ----plot_default-------------------------------------------------------------
default_plots <- plot(xpdb_x, quiet = TRUE)
names(default_plots)
default_plots$dv_vs_ipred

## ----plot_custom--------------------------------------------------------------
custom_plots <- plot(
  xpdb_x,
  plots = list(
    xpose::dv_vs_ipred,
    ~ xpose::res_vs_idv(.x, res = "CWRES"),
    ~ xpose::res_vs_idv(.x, res = "IWRES")
  ),
  quiet = TRUE
)
names(custom_plots)

## ----plot_force---------------------------------------------------------------
flaky_plots <- plot(
  xpdb_x,
  plots = list(
    xpose::dv_vs_ipred,
    ~ stop("simulated failure"),
    xpose::eta_distrib
  ),
  force = TRUE,
  quiet = FALSE
)
names(flaky_plots)

## ----plot_default_plots-------------------------------------------------------
xpdb_custom <- set_default_plots(xpdb_x, list(~ xpose::dv_vs_ipred(.x), ~ xpose::eta_distrib(.x)))
names(plot(xpdb_custom, quiet = TRUE))

## ----ggsave_xp_basic----------------------------------------------------------
out_dir <- tempdir()

saved_path <- ggsave_xp(p, filename = "dv_vs_ipred.png", path = out_dir, width = 6, height = 4)
basename(saved_path)

## ----ggsave_xp_basic_preview, echo = FALSE, out.width = "80%"-----------------
# knitr/pkgdown can only reliably embed images under knitr's own fig.path,
# not an arbitrary absolute path like the tempdir() used above to
# demonstrate an arbitrary save location
preview_path <- knitr::fig_path("png")
dir.create(dirname(preview_path), recursive = TRUE, showWarnings = FALSE)
file.copy(saved_path, preview_path, overwrite = TRUE)
knitr::include_graphics(preview_path)

## ----ggsave_xp_keywords-------------------------------------------------------
run_path <- ggsave_xp(dv_vs_ipred(xpdb_ex_pk, quiet = TRUE), filename = "@run_@plotfun.png", path = out_dir)
basename(run_path)

## ----ggsave_xp_custom_backend-------------------------------------------------
save_with_note <- function(plot, filename, path = NULL, width, height, note = "", ...) {
  saved <- ggplot2::ggsave(filename = filename, plot = plot, path = path, width = width, height = height, ...)
  writeLines(note, sub("\\.[^.]+$", ".txt", saved))
  saved
}

annotated_path <- ggsave_xp(
  p, filename = "dv_vs_ipred_annotated.png", path = out_dir,
  save_fun = save_with_note, note = "Generated for the Q3 interim analysis."
)

# the .png was written by ggplot2::ggsave() as usual; the .txt is new
readLines(sub("\\.png$", ".txt", annotated_path))

## ----auto_apply_demo----------------------------------------------------------
options(
  xpose.xtras.default_labs = list(caption = "DRAFT: do not distribute"),
  xpose.xtras.default_watermark = list(label = "DRAFT")
)

dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

## ----auto_apply_off-----------------------------------------------------------
options(xpose.xtras.auto_apply = FALSE)

dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

## ----auto_apply_reset, include = FALSE----------------------------------------
options(
  xpose.xtras.auto_apply = NULL,
  xpose.xtras.default_labs = NULL,
  xpose.xtras.default_watermark = NULL
)

## ----set_xtras_options--------------------------------------------------------
set_xtras_options(
  default_labs = list(caption = "DRAFT"),
  default_watermark = list(label = "DRAFT"),
  save_dir = tempdir(),
  save_width = 6,
  save_height = 4
)

# labels and watermark are auto-applied, and save_dir/save_width/save_height
# supply filename's path/width/height: one call, nothing else configured
saved_together <- ggsave_xp(dv_vs_ipred(xpdb_ex_pk, quiet = TRUE), filename = "tied_together.png")
basename(saved_together)

## ----tied_together_preview, echo = FALSE, out.width = "80%"-------------------
preview_path <- knitr::fig_path("png")
dir.create(dirname(preview_path), recursive = TRUE, showWarnings = FALSE)
file.copy(saved_together, preview_path, overwrite = TRUE)
knitr::include_graphics(preview_path)

## ----set_xtras_options_theme--------------------------------------------------
set_xtras_options(gg_theme = theme_bw)

xpdb_ex_pk %>%
  as_xpdb_x() %>%
  dv_vs_ipred(quiet = TRUE)

## ----get_xtras_option---------------------------------------------------------
xpdb_labelled <- xpdb_x %>%
  set_default_labs(caption = "Model-specific caption")

get_xtras_option("default_labs", xpdb_labelled)

## ----set_xtras_options_reset, include = FALSE---------------------------------
set_xtras_options(
  default_labs = NULL, default_watermark = NULL,
  save_dir = NULL, save_width = NULL, save_height = NULL,
  gg_theme = NULL
)

