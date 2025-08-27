# the objective is to follow participants from their first test

# -------------------------------------------------

# Code lifesyle change varaibles

df_change <- data_cvd |>
  mutate(
    # Exercise change (categorical)
    Exercise_change_first_to_second = case_when(
      is.na(Exercise_cat) | is.na(Exercise_cat_second) ~ NA_character_,
      Exercise_cat == "Low" & Exercise_cat_second %in% c("Moderate", "Good") ~
        "Increase",
      Exercise_cat == "Moderate" & Exercise_cat_second == "Good" ~ "Increase",
      Exercise_cat == "Good" & Exercise_cat_second %in% c("Low", "Moderate") ~
        "Decrease",
      Exercise_cat == "Moderate" & Exercise_cat_second == "Low" ~ "Decrease",
      TRUE ~ "No change"
    ),

    Exercise_change_first_to_last = case_when(
      is.na(Exercise_cat) | is.na(Exercise_cat_last) ~ NA_character_,
      Exercise_cat == "Low" & Exercise_cat_last %in% c("Moderate", "Good") ~
        "Increase",
      Exercise_cat == "Moderate" & Exercise_cat_last == "Good" ~ "Increase",
      Exercise_cat == "Good" & Exercise_cat_last %in% c("Low", "Moderate") ~
        "Decrease",
      Exercise_cat == "Moderate" & Exercise_cat_last == "Low" ~ "Decrease",
      TRUE ~ "No change"
    ),

    # Health change (categorical)
    Health_change_first_to_second = case_when(
      is.na(Health_cat) | is.na(Health_cat_second) ~ NA_character_,
      Health_cat == "Low" & Health_cat_second %in% c("Moderate", "Good") ~
        "Increase",
      Health_cat == "Moderate" & Health_cat_second == "Good" ~ "Increase",
      Health_cat == "Good" & Health_cat_second %in% c("Low", "Moderate") ~
        "Decrease",
      Health_cat == "Moderate" & Health_cat_second == "Low" ~ "Decrease",
      TRUE ~ "No change"
    ),

    Health_change_first_to_last = case_when(
      is.na(Health_cat) | is.na(Health_cat_last) ~ NA_character_,
      Health_cat == "Low" & Health_cat_last %in% c("Moderate", "Good") ~
        "Increase",
      Health_cat == "Moderate" & Health_cat_last == "Good" ~ "Increase",
      Health_cat == "Good" & Health_cat_last %in% c("Low", "Moderate") ~
        "Decrease",
      Health_cat == "Moderate" & Health_cat_last == "Low" ~ "Decrease",
      TRUE ~ "No change"
    ),

    # BMI change (using ±1 BMI unit threshold)
    BMI_change_first_to_second = case_when(
      is.na(BMI) | is.na(BMI_second) ~ NA_character_,
      BMI_second - BMI <= -1 ~ "Increase", # BMI decrease ≥1 unit
      BMI_second - BMI >= 1 ~ "Decrease", # BMI increase ≥1 unit
      TRUE ~ "No change"
    ),

    BMI_change_first_to_last = case_when(
      is.na(BMI) | is.na(BMI_last) ~ NA_character_,
      BMI_last - BMI <= -1 ~ "Increase",
      BMI_last - BMI >= 1 ~ "Decrease",
      TRUE ~ "No change"
    ),

    # VO2 change (using ±3 ml/kg/min threshold)
    VO2_change_first_to_second = case_when(
      is.na(Astrand_rel_VO2) | is.na(Astrand_rel_VO2_second) ~ NA_character_,
      Astrand_rel_VO2_second - Astrand_rel_VO2 >= 3 ~ "Increase", # VO2 increase ≥3
      Astrand_rel_VO2_second - Astrand_rel_VO2 <= -3 ~ "Decrease", # VO2 decrease ≥3
      TRUE ~ "No change"
    ),

    VO2_change_first_to_last = case_when(
      is.na(Astrand_rel_VO2) | is.na(Astrand_rel_VO2_last) ~ NA_character_,
      Astrand_rel_VO2_last - Astrand_rel_VO2 >= 3 ~ "Increase",
      Astrand_rel_VO2_last - Astrand_rel_VO2 <= -3 ~ "Decrease",
      TRUE ~ "No change"
    ),
    n_tests_within_5_fac_vs_ctrl
  ) |>
  # Convert all change variables to factors with consistent levels
  mutate(
    Exercise_change_first_to_second = factor(
      Exercise_change_first_to_second,
      levels = c("Decrease", "No change", "Increase")
    ),
    Exercise_change_first_to_last = factor(
      Exercise_change_first_to_last,
      levels = c("Decrease", "No change", "Increase")
    ),
    Health_change_first_to_second = factor(
      Health_change_first_to_second,
      levels = c("Decrease", "No change", "Increase")
    ),
    Health_change_first_to_last = factor(
      Health_change_first_to_last,
      levels = c("Decrease", "No change", "Increase")
    ),
    BMI_change_first_to_second = factor(
      BMI_change_first_to_second,
      levels = c("Decrease", "No change", "Increase")
    ),
    BMI_change_first_to_last = factor(
      BMI_change_first_to_last,
      levels = c("Decrease", "No change", "Increase")
    ),
    VO2_change_first_to_second = factor(
      VO2_change_first_to_second,
      levels = c("Decrease", "No change", "Increase")
    ),
    VO2_change_first_to_last = factor(
      VO2_change_first_to_last,
      levels = c("Decrease", "No change", "Increase")
    )
  )


# ----------------------------------------

# Complete code for both main and sensitivity analyses
df_final_analysis <- df_change |>
  # Ensure 1:4 matching if desired
  group_by(id_cluster) |>
  filter(
    n() == 5,
    sum(treated == "HPA") == 1,
    sum(treated == "Control") == 4
  ) |>
  # Only keep clusters where HPA person has 2 tests
  filter(any(n_tests_within_5_fac_vs_ctrl == "HPA, 2 tests")) |>

  # Assign first and second test dates to controls
  mutate(
    assigned_first_test_date = case_when(
      treated == "Control" ~ first(first_test_date[treated == "HPA"]),
      treated == "HPA" ~ first_test_date
    ),

    assigned_second_test_date = case_when(
      treated == "Control" ~ first(second_test_date[treated == "HPA"]),
      treated == "HPA" ~ second_test_date
    ),
    # Assign same days_first_to_second from HPA to controls
    assigned_days_first_to_second = case_when(
      treated == "Control" ~ first(days_first_to_second[treated == "HPA"]),
      treated == "HPA" ~ days_first_to_second
    ),
    BMI_change_first_to_second = case_when(
      treated == "Control" ~
        first(BMI_change_first_to_second[treated == "HPA"]),
      treated == "HPA" ~ BMI_change_first_to_second
    ),

    VO2_change_first_to_second = case_when(
      treated == "Control" ~
        first(VO2_change_first_to_second[treated == "HPA"]),
      treated == "HPA" ~ VO2_change_first_to_second
    )
  ) |>
  ungroup() |>

  # Remove deaths between tests
  filter(is.na(DODSDAT) | DODSDAT >= assigned_second_test_date) |>

  # Create all necessary variables
  mutate(
    # Between-test event indicator
    cvd_event_between_tests = case_when(
      is.na(cvd_art_after_HPA_indatum) ~ 0,
      cvd_art_after_HPA_indatum >= assigned_first_test_date &
        cvd_art_after_HPA_indatum < assigned_second_test_date ~
        1,
      TRUE ~ 0
    ),

    # Time variables
    year_test1 = year(assigned_first_test_date),
    year_test2 = year(assigned_second_test_date),
    age_baseline_test1 = as.numeric(difftime(
      first_test_date,
      DateOfBirth,
      units = "days"
    )) /
      365.25,

    # Second test follow-up variables
    baseline_second_test = assigned_second_test_date,
    censor_second_10yrs = pmin(
      baseline_second_test + days(3652),
      as.Date("2024-06-05"),
      na.rm = TRUE
    ),
    risk_end_second_10yrs = pmin(
      cvd_art_after_HPA_indatum,
      DODSDAT,
      censor_second_10yrs,
      na.rm = TRUE
    ),
    risk_time_second_10yrs = as.numeric(difftime(
      risk_end_second_10yrs,
      baseline_second_test,
      units = "days"
    )),
    risk_flag_second_10yrs = case_when(
      is.na(cvd_art_after_HPA_indatum) ~ 0,
      cvd_art_after_HPA_indatum > baseline_second_test &
        cvd_art_after_HPA_indatum <= risk_end_second_10yrs ~
        1,
      TRUE ~ 0
    ),

    # Age variables for second test
    age_enter_second = as.numeric(difftime(
      baseline_second_test,
      DateOfBirth,
      units = "days"
    )) /
      365.25,
    age_exit_second = as.numeric(difftime(
      risk_end_second_10yrs,
      DateOfBirth,
      units = "days"
    )) /
      365.25
  ) |>

  # Select variables needed for both analyses
  select(
    # ID variables
    LopNr,
    id_cluster,
    treated,

    # Test information
    n_tests_within_5_fac_vs_ctrl,
    first_test_date,
    assigned_first_test_date,
    second_test_date,
    assigned_second_test_date,
    days_first_to_second,
    assigned_days_first_to_second,
    year_test1,
    year_test2,

    # Demographics
    age_baseline_test1,
    Age,
    Sex,
    EducationLevel,
    income_pct_diff_from_median,
    birth_cohort,

    # Health history
    cvd_art_before_HPA_flag,
    comorbidity,
    cvd_event_between_tests,

    # Lifestyle variables at first test
    BMI,
    BMI_cat,
    Astrand_rel_VO2,
    vo2_cat,
    Health,
    Health_cat,
    ExerciseAnswer,
    Exercise_cat,

    # Lifestyle variables at second test
    BMI_second,
    BMI_cat_second,
    Astrand_rel_VO2_second,
    vo2_cat_second,
    Health_second,
    Health_cat_second,
    ExerciseAnswer_second,
    Exercise_cat_second,

    # Change variables
    Exercise_change_first_to_second,
    Health_change_first_to_second,
    BMI_change_first_to_second,
    VO2_change_first_to_second,

    # Original risk variables (main analysis from first test)
    risk_time_art_10yrs,
    risk_art_flag_10yrs,

    # New risk variables (sensitivity analysis from second test)
    risk_time_second_10yrs,
    risk_flag_second_10yrs,
    age_enter_second,
    age_exit_second
  )
