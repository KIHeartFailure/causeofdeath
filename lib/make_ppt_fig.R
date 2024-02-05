make_ppt_fig <- function(fig_name = NULL, append = TRUE) {
  if (is.null(fig_name)) fig_name <- " "
  topptx(filename = here::here("./output/figs/figs.pptx"), append = append, title = fig_name, devsize = T)
}

create_dml <- function(plot) {
  rvg::dml(ggobj = plot)
}

create_pptx <- function(plot, left = 0.5, top = 0.5, width = 10, height = 6) {
  plot <- create_dml(plot)

  figs %>%
    officer::add_slide() %>%
    officer::ph_with(plot, location = officer::ph_location(
      width = width, height = height, left = left, top = top
    )) %>%
    print(target = here::here("output/figs/figs.pptx"))
}
