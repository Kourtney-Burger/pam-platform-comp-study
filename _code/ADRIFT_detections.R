# Load packages
library(dplyr)

blues <- load("_data/ADRIFT/Detections/blueBin_adrift.rdata")
fins <- load("_data/ADRIFT/Detections/finBin_adrift.rdata")

# Step 1: Rename columns to distinguish species
bm <- bmHourly_adrift %>%
  rename(bm_presence = species) %>%
  select(UTC, bm_presence, DriftName)

bp <- finBin_adrift %>%
  rename(bp_presence = species) %>%
  select(UTC, bp_presence, DriftName)

# Step 2: Merge by datetime
combined <- full_join(bm, bp, by = c("UTC", "DriftName"))

# Step 3 (Optional): Replace NAs with 0 in presence columns if needed
bm_bp_presence_ADRIFT <- combined %>%
  mutate(
    bm_presence = ifelse(is.na(bm_presence), 0, 1),
    bp_presence = ifelse(is.na(bp_presence), 0, 1)
  )

# Step 4: Create summary table
summary_table <- bm_bp_presence_ADRIFT %>%
  group_by(DriftName) %>%
  summarize(
    n_total = n(),
    bm_pct = mean(bm_presence == 1) * 100,
    bp_pct = mean(bp_presence == 1) * 100,
    both_pct = mean(bm_presence == 1 & bp_presence == 1) * 100
  ) %>%
  arrange(desc(both_pct))  # Order by highest to lowest both_pct

saveRDS(summary_table, "_data/ADRIFT/bm_bp_presence_summary.rds")
