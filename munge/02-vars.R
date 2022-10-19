

# Variables for tabs/mods -------------------------------------------------


tabvars <- c(
  # demo
  "shf_indexyear",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "shf_followuphfunit", "shf_followuplocation_cat",

  # clinical factors and lab measurments
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmiimp",
  "shf_bmiimp_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # treatments
  "shf_rasiarni",
  "shf_mra",
  "shf_digoxin",
  "shf_diuretic",
  "shf_nitrate",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "shf_device_cat",

  # comorbs
  "shf_smoke",
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_pad",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd",
  "sos_com_kidney",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat2"
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "shf_indexyear",
  "shf_age",
  "shf_nyha",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_bmiimp",
  "shf_smoke"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]
