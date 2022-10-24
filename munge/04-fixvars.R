

rsdata <- rsdata %>% mutate(
  sos_deathcause_cat =
    factor(case_when(
      is.na(sos_deathcause) ~ NA_real_,
      str_detect(sos_deathcause, "I21|I22|I252") ~ 1,
      str_detect(sos_deathcause, "I110|I130|I132|I255|I420|I423|I425|I426|I427|I428|I429|I43|I50|J81|K761|R57") ~ 2,
      str_detect(sos_deathcause, "I0[5-8]|I3[4-9]|Q22|Q23[0-3]|Z95[2-4]") ~ 3,
      str_detect(sos_deathcause, "I461") ~ 4,
      str_detect(sos_deathcause, "I6[0-4]|I69[0-4]") ~ 5,
      str_detect(sos_deathcause, "I") ~ 6,
      str_detect(sos_deathcause, "J0[0-6]|J09|J1[0-8]") ~ 7,
      str_detect(sos_deathcause, "N1[7-9]|Z491|Z492") ~ 8,
      str_detect(sos_deathcause, "E1[0-4]") ~ 9,
      str_detect(sos_deathcause, "C") ~ 10,
      TRUE ~ 11
    ),
    levels = 1:11, labels = c(
      "MI", "HF", "VHD", "SCD", "Stroke", "Other CV",
      "Respitory", "Kidney", "Diabetes", "Cancer", "Other Non-CV"
    )
    ),
  # cut 2 yr
  sos_deathcause_cat_2y =
    factor(case_when(
      is.na(sos_deathcause) | sos_outtime_death > 365 * 2 ~ NA_real_,
      str_detect(sos_deathcause, "I21|I22|I252") ~ 1,
      str_detect(sos_deathcause, "I110|I130|I132|I255|I420|I423|I425|I426|I427|I428|I429|I43|I50|J81|K761|R57") ~ 2,
      str_detect(sos_deathcause, "I0[5-8]|I3[4-9]|Q22|Q23[0-3]|Z95[2-4]") ~ 3,
      str_detect(sos_deathcause, "I461") ~ 4,
      str_detect(sos_deathcause, "I6[0-4]|I69[0-4]") ~ 5,
      str_detect(sos_deathcause, "I") ~ 6,
      str_detect(sos_deathcause, "J0[0-6]|J09|J1[0-8]") ~ 7,
      str_detect(sos_deathcause, "N1[7-9]|Z491|Z492") ~ 8,
      str_detect(sos_deathcause, "E1[0-4]") ~ 9,
      str_detect(sos_deathcause, "C") ~ 10,
      TRUE ~ 11
    ),
    levels = 1:11, labels = c(
      "MI", "HF", "VHD", "SCD", "Stroke", "Other CV",
      "Respitory", "Kidney", "Diabetes", "Cancer", "Other Non-CV"
    )
    ),
  sos_deathcause_cat2 =
    factor(case_when(
      is.na(sos_deathcause) ~ 0,
      sos_deathcause_cat %in% c("MI", "HF", "VHD", "SCD", "Stroke", "Other CV") ~ 1,
      TRUE ~ 2
    ), levels = 0:2, labels = c("Alive", "CV", "Non-CV")),
  # cut 2 yr
  sos_deathcause_cat2_2y = if_else(sos_outtime_death <= 365 * 2, sos_deathcause_cat2, factor("Alive")),
  sos_deathcause_catcv_cr = create_crevent(sos_deathcause_cat2,
    sos_deathcause_cat2,
    eventvalues = c("CV", "Non-CV")
  ),
  # cut 2 yr
  sos_deathcause_catcv_cr_2y = if_else(sos_outtime_death > 365 * 2, 0, sos_deathcause_catcv_cr),
  sos_deathcause_catnoncv_cr = create_crevent(sos_deathcause_cat2,
    sos_deathcause_cat2,
    eventvalues = c("Non-CV", "CV")
  ),
  # cut 2 yr
  sos_deathcause_catnoncv_cr_2y = if_else(sos_outtime_death > 365 * 2, 0, sos_deathcause_catnoncv_cr),
  sos_outtime_death_2y = ifelse(sos_outtime_death > 365 * 2, 365 * 2, sos_outtime_death)
)


# ntprobnp

mednt <- median(rsdata$shf_ntprobnp, na.rm = T)

rsdata <- rsdata %>%
  mutate(
    shf_ntprobnp_cat = factor(case_when(
      shf_ntprobnp < mednt ~ 1,
      shf_ntprobnp >= mednt ~ 2
    ),
    levels = 1:2,
    labels = c("Below medium", "Above medium")
    )
  )

# income

inc <- rsdata %>%
  group_by(shf_indexyear) %>%
  summarise(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.5),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(incsum)) %>%
  spread(name, value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < `50%` ~ 1,
      scb_dispincome >= `50%` ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-`50%`)

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))


rsdata <- create_crvar(rsdata, "shf_ef_cat")
rsdata <- create_crvar(rsdata, "shf_sex")
