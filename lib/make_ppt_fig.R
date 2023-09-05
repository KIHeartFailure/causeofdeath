make_ppt_fig <- function(fig_name = NULL, append = TRUE) {
  if (is.null(fig_name)) fig_name <- " "
  topptx(filename = here::here("./output/figs/figs_causeofdeath.pptx"), append = append, title = fig_name, devsize = T)
}
