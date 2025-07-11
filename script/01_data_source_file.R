#############
# Source file for data
#############

library(tidyverse)

data_cvd <- read_csv(here::here(
  "..",
  "data",
  "df_paper_3_CVD_2025-03-05.csv"
)) |>
  left_join(
    read_csv(here::here("..", "data", "registeruttagdata_2025-01-16.csv")) |>
      select(LopNr, id_cluster)
  ) |>
  left_join(
    read_csv(here::here("..", "data", "df_n_tests_CVD_2025-03-05.csv")) |>
      select(
        LopNr,
        n_tests_total,
        n_tests_within_5,
        n_tests_within_10,
        composite_score_within_5,
        composite_score_within_10,
        composite_score_total,
        ExerciseAnswer,
        TobaccoSmoking,
        Health,
        StressOverall,
        Astrand_rel_VO2,
        BMI
      )
  )


data_cvd <-
  data_cvd |>
  group_by(Year) |>
  mutate(
    median_income = median(CSFVI, na.rm = TRUE),
    income_pct_diff_from_median = (CSFVI / median_income - 1) * 100
  ) |>
  ungroup() |>
  select(-median_income) |>
  mutate(
    # birt cohort
    birth_cohort = case_when(
      FodelseAr <= 1939 ~ "≤39",
      FodelseAr >= 1940 & FodelseAr <= 1949 ~ "1940–49",
      FodelseAr >= 1950 & FodelseAr <= 1959 ~ "1950–59",
      FodelseAr >= 1960 & FodelseAr <= 1969 ~ "1960–69",
      FodelseAr >= 1970 & FodelseAr <= 1979 ~ "1970–79",
      FodelseAr >= 1980 & FodelseAr <= 1989 ~ "1980–89",
      FodelseAr >= 1990 ~ "≥1990"
    ),
    birth_cohort = factor(
      birth_cohort,
      levels = c(
        "≤39",
        "1940–49",
        "1950–59",
        "1960–69",
        "1970–79",
        "1980–89",
        "≥1990"
      )
    ),

    #age_enter = Age,
    # Age at event
    #age_exit = year(risk_end_art_10yrs) - FodelseAr,
    # age_exit = if_else(age_exit - age_enter == 0, 1 + age_exit, age_exit),
    age_group = case_when(
      Age < 35 ~ "<35",
      Age >= 35 & Age < 50 ~ "35–49",
      Age >= 50 ~ "≥50",
      TRUE ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c(
        "<35",
        "35–49",
        "≥50"
      )
    ),
    treated = if_else(treated == 1, "HPA", "Control"),
    treated = factor(treated, levels = c("Control", "HPA")),
    #treated = factor(treated, levels = c(0, 1), labels = c("Control", "HPA")),
    n_tests_within_5_fac = case_when(
      is.na(n_tests_within_5) ~ "Control",
      n_tests_within_5 == 0 ~ "Control",
      n_tests_within_5 == 1 ~ "HPA, 1 test",
      n_tests_within_5 == 2 ~ "HPA, 2 tests",
      n_tests_within_5 > 2 ~ "HPA, ≥3 tests"
    ),
    n_tests_within_5_fac = factor(
      n_tests_within_5_fac,
      levels = c("Control", "HPA, 1 test", "HPA, 2 tests", "HPA, ≥3 tests")
    ),
    n_tests_within_10_fac = case_when(
      is.na(n_tests_within_10) ~ "Control",
      n_tests_within_10 == 0 ~ "Control",
      n_tests_within_10 == 1 ~ "HPA, 1 test",
      n_tests_within_10 == 2 ~ "HPA, 2 tests",
      n_tests_within_10 > 2 ~ "HPA, ≥3 tests"
    ),
    n_tests_within_10_fac = factor(
      n_tests_within_10_fac,
      levels = c("Control", "HPA, 1 test", "HPA, 2 tests", "HPA, ≥3 tests")
    ),
    Sex = if_else(Sex == 1, "Men", "Women"),
    Sex = factor(Sex, c("Men", "Women")),
    comorbidity = case_when(
      CCIw == 1 ~ "1",
      CCIw == 2 ~ "2",
      CCIw > 2 ~ "=>3",
      TRUE ~ "0"
    ),

    across(
      c(
        n_tests_total,
        n_tests_within_5,
        composite_score_within_5,
        composite_score_total
      ),
      ~ if_else(is.na(.), 0, .)
    ),
    EducationLevel = case_when(
      edu %in% c("1", "2") ~ "Primary",
      edu %in% c("3", "4") ~ "Secondary",
      #edu == "5" ~ "Tertiary <2 years",
      edu %in% c("5", "6", "7") ~ "Tertiary",
      TRUE ~ NA_character_
    ),
    EducationLevel = factor(
      EducationLevel,
      levels = c("Primary", "Secondary", "Tertiary")
    ),
    ssyk_WBHL_group = case_when(
      # is.na(Ssyk1_majorlevel) ~ "Unknown",
      Ssyk1_majorlevel %in% c(1, 2, 3) ~ "White-collar high-skilled",
      Ssyk1_majorlevel %in% c(4, 5) ~ "White-collar low-skilled",
      Ssyk1_majorlevel %in% c(6, 7) ~ "Blue-collar high-skilled",
      Ssyk1_majorlevel %in% c(8, 9) ~ "Blue-collar low-skilled",
      TRUE ~ NA_character_
    ),
    ssyk_WBHL_group = factor(
      ssyk_WBHL_group,
      levels = c(
        "White-collar high-skilled",
        "White-collar low-skilled",
        "Blue-collar high-skilled",
        "Blue-collar low-skilled"
        # "Unknown"
      )
    ),
    cvd_art_before_HPA_flag = if_else(cvd_art_before_HPA_flag == 1, 1, 0),

    #KommunSize = if_else(is.na(KommunSize), "Unknown", KommunSize),
    KommunSize = factor(
      KommunSize,
      levels = c(
        "Metropolitan municipalities",
        "Dense municipalities",
        "Rural municipalities"
      )
    ),
    # Place_of_origin = if_else(is.na(Place_of_origin), "Unknown", Place_of_origin),
    Place_of_origin = factor(
      Place_of_origin,
      levels = c("Outside Europe", "Europe", "Sweden")
    ),
    Civil_status = factor(Civil_status, levels = c("Partner", "Single")),
    SSYK = factor(
      SSYK,
      levels = c(
        "Managers",
        "Science and engineering",
        "Health care",
        "Education",
        "Other professionals",
        "Associate professionals",
        "Administration and customer service",
        "Manufacturing",
        "Mechanical manufacturing",
        "Building",
        "Agriculture and forestry",
        "Service and shop sales",
        "Transport",
        "Cleaners",
        "Personal care",
        "Other elementary occupations",
        "Military"
      )
    ),
    comorbidity = factor(comorbidity, levels = c("0", "1", "2", "=>3")),
    Income_Sources_Category = factor(
      Income_Sources_Category,
      levels = c(">3", "1", "2 to 3")
    ),
    Turnover_Rate_Category = factor(
      Turnover_Rate_Category,
      levels = c("<10%", "≥20%", "10% to <20%")
    ),
    IncomeLevel_CSFVI = factor(
      IncomeLevel_CSFVI,
      levels = c("<60%", "60–79%", "80–119%", "120–199%", "≥200%")
    ),
    Income_Sources_Category = factor(
      Income_Sources_Category,
      levels = c("1", "2", ">3")
    ),
    Turnover_Rate_Category = factor(
      Turnover_Rate_Category,
      levels = c("<10%", "10% to <20%", "≥20%")
    ),
    Operating_Profit_Margin_Category = factor(
      Operating_Profit_Margin_Category,
      levels = c("<-5%", "-5% to <0%", "0% to 5%", ">5%")
    ),
    Number_of_employees_Category = factor(
      Number_of_employees_Category,
      levels = c("1 to 9", "10 to 49", "50 to 249", "≥250")
    ),
    cvd_art_before_HPA_flag = if_else(
      is.na(cvd_art_before_HPA_flag),
      0,
      cvd_art_before_HPA_flag
    ),
    # life-style variables
    BMI_cat_tmp = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal weight",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obesity",
      TRUE ~ NA_character_ # controls stay NA for now
    ),

    ## repeat for the other exposures
    vo2_cat_tmp = case_when(
      Astrand_rel_VO2 < 20 ~ "Very low",
      Astrand_rel_VO2 >= 20 & Astrand_rel_VO2 < 30 ~ "Low",
      Astrand_rel_VO2 >= 30 & Astrand_rel_VO2 < 40 ~ "Moderate",
      Astrand_rel_VO2 >= 40 & Astrand_rel_VO2 < 50 ~ "High",
      Astrand_rel_VO2 >= 50 ~ "Very high",
      TRUE ~ NA_character_
    ),
    Health_cat_tmp = case_when(
      Health %in% 1:2 ~ "Low",
      Health == 3 ~ "Moderate",
      Health %in% 4:5 ~ "Good",
      TRUE ~ NA_character_
    ),
    Exercise_cat_tmp = case_when(
      ExerciseAnswer %in% 1:2 ~ "Low",
      ExerciseAnswer == 3 ~ "Moderate",
      ExerciseAnswer %in% 4:5 ~ "Good",
      TRUE ~ NA_character_
    ),
    n_tests_within_5_fac_vs_ctrl = case_when(
      is.na(n_tests_within_5) ~ NA_character_,
      n_tests_within_5 == 0 ~ NA_character_,
      n_tests_within_5 == 1 ~ "HPA, 1 test",
      n_tests_within_5 == 2 ~ "HPA, 2 tests",
      n_tests_within_5 > 2 ~ "HPA, ≥3 tests"
    ),
    n_tests_within_5_fac_vs_ctrl = fct_drop(n_tests_within_5_fac_vs_ctrl)
  ) %>%

  ## copy the treated subject’s category to every row in its cluster, treated with NA becomes NA in theire controls
  group_by(id_cluster) %>%
  mutate(
    BMI_cat = factor(
      replace_na(BMI_cat_tmp, first(na.omit(BMI_cat_tmp))),
      levels = c("Underweight", "Normal weight", "Overweight", "Obesity")
    ),
    vo2_cat = factor(
      replace_na(vo2_cat_tmp, first(na.omit(vo2_cat_tmp))),
      levels = c("Very low", "Low", "Moderate", "High", "Very high")
    ),
    Health_cat = factor(
      replace_na(Health_cat_tmp, first(na.omit(Health_cat_tmp))),
      levels = c("Low", "Moderate", "Good")
    ),
    Exercise_cat = factor(
      replace_na(Exercise_cat_tmp, first(na.omit(Exercise_cat_tmp))),
      levels = c("Low", "Moderate", "Good")
    ),
    n_tests_within_5_fac_vs_ctrl = factor(
      replace_na(
        n_tests_within_5_fac_vs_ctrl,
        first(na.omit(n_tests_within_5_fac_vs_ctrl))
      ),
      levels = c("HPA, 1 test", "HPA, 2 tests", "HPA, ≥3 tests")
    )
  ) %>%
  ungroup()

# create dataset wit matched controls for those who have NA in kommunsize and place of origin are removed

#data_cvd <-
#data_cvd |> drop_na(KommunSize, Place_of_origin) |>
#  group_by(id_cluster) |>
#  mutate(n_cluster = n()) |>
#  ungroup() |> filter(n_cluster > 4)
