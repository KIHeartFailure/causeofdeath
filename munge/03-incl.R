

# Inclusion/exclusion criteria --------------------------------------------------------

rsdata <- rsdata400
flow <- c("Number of posts in SHFDB4", nrow(rsdata))

rsdata <- rsdata %>%
  filter(!(shf_source == "New SHF" & shf_type == "Follow-up"))
flow <- rbind(flow, c("Exclude follow-up visits for New SwedeHF", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

colnames(flow) <- c("Criteria", "N")
