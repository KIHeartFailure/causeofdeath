
levscd <- c(
  "MI", "IHD excl MI", "HF", "Stroke", "VHD", "AF", "Other CV",
  "Respitory", "Diabetes", "Other endocrine", "Kidney", "Cancer",
  "Infection", "Covid-19", "Gastro", "Neuro", "Heme", "Dementia", "Not specified",
  "Other Non-CV"
)

rsdata <- rsdata %>% mutate(
  sos_deathcause_cat =
    case_when(
      sos_out_death == "No" ~ NA_real_,
      str_detect(sos_deathcause, "I21|I22") ~ 1,
      str_detect(sos_deathcause, "I2[0-5]") ~ 2,
      str_detect(sos_deathcause, "I110|I130|I132|I255|I420|I423|I425|I426|I427|I428|I429|I43|I50|J81|K761|R57") ~ 3,
      str_detect(sos_deathcause, "I6[0-4]") ~ 4,
      str_detect(sos_deathcause, "I0[5-8]|I3[4-9]|Q22|Q23[0-3]|Z95[2-4]") ~ 5,
      str_detect(sos_deathcause, "I48") ~ 6,
      str_detect(sos_deathcause, "I") ~ 7,
      str_detect(sos_deathcause, "J0[0-6]|J09|J1[0-8]|J4[0-4]") ~ 8,
      str_detect(sos_deathcause, "E1[0-4]") ~ 9,
      str_detect(sos_deathcause, "E0[0-7]|E1[5-6]|E2|E3[0-5]E7|E8|E9") ~ 10,
      str_detect(sos_deathcause, "N1[7-9]") ~ 11,
      str_detect(sos_deathcause, "C") ~ 12,
      str_detect(sos_deathcause, "A|B") ~ 13,
      str_detect(sos_deathcause, "U071|U072|U08|U09|U10|B342|B972") ~ 14,
      str_detect(sos_deathcause, "K") ~ 15,
      str_detect(sos_deathcause, "G") ~ 16,
      str_detect(sos_deathcause, "D[5-8]") ~ 17,
      str_detect(sos_deathcause, "F0[0-4]") ~ 18,
      str_detect(sos_deathcause, "R99") ~ 19,
      TRUE ~ 20
    ),

  # cut 1 yr
  sos_deathcause_cat_1y =
    factor(case_when(
      sos_out_death == "No" | sos_outtime_death > 365 * 1 ~ NA_real_,
      TRUE ~ sos_deathcause_cat
    ),
    levels = 1:20, labels = levscd
    ),
  # cut 5 yr
  sos_deathcause_cat_5y =
    factor(case_when(
      sos_out_death == "No" | sos_outtime_death > 365 * 5 ~ NA_real_,
      TRUE ~ sos_deathcause_cat
    ),
    levels = 1:20, labels = levscd
    ),
  sos_deathcause_cat = factor(sos_deathcause_cat, levels = 1:20, labels = levscd),
  sos_deathcause_cat2 =
    factor(case_when(
      sos_out_death == "No" ~ 1,
      sos_deathcause_cat %in% c("MI", "IHD excl MI", "HF", "Stroke", "VHD", "AF", "Other CV") ~ 2,
      TRUE ~ 3
    ), levels = 1:3, labels = c("Alive", "CV", "Non-CV")),
  # cut 1 yr
  sos_deathcause_cat2_1y = if_else(sos_outtime_death <= 365 * 1, sos_deathcause_cat2, factor("Alive")),
  # cut 5 yr
  sos_deathcause_cat2_5y = if_else(sos_outtime_death <= 365 * 5, sos_deathcause_cat2, factor("Alive")),
  sos_deathcause_catcv_cr = create_crevent(sos_deathcause_cat2,
    sos_deathcause_cat2,
    eventvalues = c("CV", "Non-CV")
  ),
  # cut 1 yr
  sos_deathcause_catcv_cr_1y = if_else(sos_outtime_death > 365 * 1, 0, sos_deathcause_catcv_cr),
  # cut 5 yr
  sos_deathcause_catcv_cr_5y = if_else(sos_outtime_death > 365 * 5, 0, sos_deathcause_catcv_cr),
  sos_deathcause_catnoncv_cr = create_crevent(sos_deathcause_cat2,
    sos_deathcause_cat2,
    eventvalues = c("Non-CV", "CV")
  ),
  # cut 1 yr
  sos_deathcause_catnoncv_cr_1y = if_else(sos_outtime_death > 365 * 1, 0, sos_deathcause_catnoncv_cr),
  # cut 5 yr
  sos_deathcause_catnoncv_cr_5y = if_else(sos_outtime_death > 365 * 5, 0, sos_deathcause_catnoncv_cr),
  sos_outtime_death_1y = ifelse(sos_outtime_death > 365 * 1, 365 * 1, sos_outtime_death),
  sos_outtime_death_5y = ifelse(sos_outtime_death > 365 * 5, 365 * 5, sos_outtime_death)
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
