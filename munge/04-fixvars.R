
levscd <- c(
  "MI", "IHD excl MI", "PVD", "HF", "Stroke", "AAD", "PAD", "VHD", "AF", "Other CV",
  "Respiratory", "DM", "Endocrine excl DM", "Renal", "Cancer",
  "Covid-19", "Infection", "Gastro", "Neuro", "Heme", "Dementia", "Muscoloskeletal",
  "Accidents", "Suicide", "Unknown",
  "Other Non-CV"
)

rsdata <- rsdata %>% mutate(
  shf_indexyear_cat = case_when(
    shf_indexyear <= 2009 ~ "2000-2009",
    shf_indexyear <= 2014 ~ "2010-2014",
    shf_indexyear <= 2018 ~ "2015-2018",
    shf_indexyear <= 2021 ~ "2019-2021"
  ),
  sos_deathcause_cat =
    case_when(
      sos_out_death == "No" ~ NA_real_,
      str_detect(sos_deathcause, "^(I21|I22)") ~ 1,
      str_detect(sos_deathcause, "^(I2[0-5])") ~ 2,
      str_detect(sos_deathcause, "^(I2[6-8])") ~ 3,
      str_detect(sos_deathcause, "^(I110|I130|I132|I255|I420|I423|I425|I426|I427|I428|I429|I43|I50|J81|K761|R570)") ~ 4,
      str_detect(sos_deathcause, "^(I6[0-4])") ~ 5,
      str_detect(sos_deathcause, "^(I71|I72|I79[0-1])") ~ 6,
      str_detect(sos_deathcause, "^(I70|I73|I74|I77|I78|I79[2-8])") ~ 7,
      str_detect(sos_deathcause, "^(I0[5-8]|I3[4-9]|Q22|Q23[0-3]|Q23[5-9])") ~ 8,
      str_detect(sos_deathcause, "^(I48)") ~ 9,
      str_detect(sos_deathcause, "^I") ~ 10,
      str_detect(sos_deathcause, "^J") ~ 11,
      str_detect(sos_deathcause, "^(E1[0-4])") ~ 12,
      str_detect(sos_deathcause, "^(E0[0-7]|E1[5-6]|E2|E3[0-5]|E7|E8|E9)") ~ 13,
      str_detect(sos_deathcause, "^(N1[7-9])") ~ 14,
      str_detect(sos_deathcause, "^C") ~ 15,
      str_detect(sos_deathcause, "^(U071|U072|U08|U09|U10|B342|B972)") ~ 16,
      str_detect(sos_deathcause, "^(A|B)") ~ 17,
      str_detect(sos_deathcause, "^K") ~ 18,
      str_detect(sos_deathcause, "^G") ~ 19,
      str_detect(sos_deathcause, "^(D[5-8])") ~ 20,
      str_detect(sos_deathcause, "^(F0[0-4]|R54)") ~ 21,
      str_detect(sos_deathcause, "^M") ~ 22,
      str_detect(sos_deathcause, "^(V|W|X[0-5])") ~ 23,
      str_detect(sos_deathcause, "^(X[6-7]|X8[0-4])") ~ 24,
      str_detect(sos_deathcause, "^R99") ~ 25,
      TRUE ~ 26
    ),
  # cut 1 yr
  sos_deathcause_cat_1y =
    factor(case_when(
      sos_out_death == "No" | sos_outtime_death > 365 * 1 ~ NA_real_,
      TRUE ~ sos_deathcause_cat
    ),
    levels = 1:26, labels = levscd
    ),
  # cut 5 yr
  sos_deathcause_cat_5y =
    factor(case_when(
      sos_out_death == "No" | sos_outtime_death > 365 * 5 ~ NA_real_,
      TRUE ~ sos_deathcause_cat
    ),
    levels = 1:26, labels = levscd
    ),
  sos_deathcause_cat = factor(sos_deathcause_cat, levels = 1:26, labels = levscd),
  sos_deathcause_cat2 =
    factor(case_when(
      sos_out_death == "No" ~ 4,
      sos_deathcause_cat %in% c("MI", "IHD excl MI", "PVD", "HF", "Stroke", "VHD", "AF", "AAD", "PAD", "Other CV") ~ 1,
      sos_deathcause_cat %in% c("Unknown") ~ 3,
      TRUE ~ 2
    ), levels = 1:4, labels = c("CV", "Non-CV", "Unknown", "Alive")),
  # cut 1 yr
  sos_deathcause_cat2_1y = if_else(sos_outtime_death <= 365 * 1, sos_deathcause_cat2, factor("Alive")),
  # cut 5 yr
  sos_deathcause_cat2_5y = if_else(sos_outtime_death <= 365 * 5, sos_deathcause_cat2, factor("Alive")),
  sos_deathcause_catcv_cr = case_when(
    sos_deathcause_cat2 == "CV" ~ 1,
    sos_deathcause_cat2 %in% c("Non-CV", "Unknwon") ~ 2,
    TRUE ~ 0
  ),
  # cut 1 yr
  sos_deathcause_catcv_cr_1y = if_else(sos_outtime_death > 365 * 1, 0, sos_deathcause_catcv_cr),
  # cut 5 yr
  sos_deathcause_catcv_cr_5y = if_else(sos_outtime_death > 365 * 5, 0, sos_deathcause_catcv_cr),
  sos_deathcause_catnoncv_cr = case_when(
    sos_deathcause_cat2 == "Non-CV" ~ 1,
    sos_deathcause_cat2 %in% c("CV", "Unknwon") ~ 2,
    TRUE ~ 0
  ),
  # cut 1 yr
  sos_deathcause_catnoncv_cr_1y = if_else(sos_outtime_death > 365 * 1, 0, sos_deathcause_catnoncv_cr),
  # cut 5 yr
  sos_deathcause_catnoncv_cr_5y = if_else(sos_outtime_death > 365 * 5, 0, sos_deathcause_catnoncv_cr),
  sos_outtime_death_1y = ifelse(sos_outtime_death > 365 * 1, 365 * 1, sos_outtime_death),
  sos_outtime_death_5y = ifelse(sos_outtime_death > 365 * 5, 365 * 5, sos_outtime_death)
)


# income

inc <- rsdata %>%
  group_by(shf_indexyear) %>%
  summarise(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
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
    scb_dispincome_cat = factor(case_when(
      scb_dispincome < `33%` ~ 1,
      scb_dispincome < `66%` ~ 2,
      scb_dispincome >= `66%` ~ 3
    ),
    levels = 1:3,
    labels = c("Lowest tertile", "Middle tertile", "Highest tertile")
    )
  ) %>%
  select(-`33%`, -`66%`)

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))


rsdata <- create_crvar(rsdata, "shf_ef_cat")
rsdata <- create_crvar(rsdata, "shf_sex")
