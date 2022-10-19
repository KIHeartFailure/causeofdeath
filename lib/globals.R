# default is to use tidyverse functions
select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours 
global_cols <- rev(c(
  #"#F0FAFF",
  #"#D6F0F7",
  "#9BD4E5",
  "#70C1DA",
  "#4FB3D1",
  "#2F99BA",
  "#0F83A3",
  "#006E8A",
  "#034F69",
  "#023647"
))

# used for calculation of ci 
global_z05 <- qnorm(1 - 0.025)