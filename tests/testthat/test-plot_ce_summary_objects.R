# add_missing_columns tests:----
test_that("add_missing_columns generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    "a" = 1:5,
    "b" = LETTERS[1:5],
    "c" = 6:10
  )
  # Get results:
  df_results <- add_missing_columns(
    .df_check_ = df_test,
    .string_columns_ = c("icer_label", "dominance"),
    .numerics_columns_ = c("qalys", "costs")
  )
  # Define expected:
  df_expcted <- data.frame(
    "a" = 1:5, "b" = c("A", "B", "C", "D", "E"), "c" = 6:10,
    "qalys" = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    "costs" = c(NA_real_, NA_real_, NA_real_, NA_real_,  NA_real_),
    "icer_label" = c(NA_character_, NA_character_, NA_character_,
                     NA_character_, NA_character_),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_,
                    NA_character_)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    "a" = 1:5,
    "b" = LETTERS[1:5],
    "c" = 6:10
  )
  # Get results:
  df_results <- add_missing_columns(
    .df_check_ = df_test,
    .string_columns_ = c("dominance"),
    .numerics_columns_ = c("qalys", "costs")
  )
  # Define expected:
  df_expcted <- data.frame(
    "a" = 1:5, "b" = c("A", "B", "C", "D", "E"), "c" = 6:10,
    "qalys" = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    "costs" = c(NA_real_, NA_real_, NA_real_, NA_real_,  NA_real_),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_,
                    NA_character_)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("identify_dominance generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(20, 25, 20, 30)
  )
  # Get results:
  df_results <- identify_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 20, 30),
    "delta.e" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "delta.c" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "icer" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "dominance" = c(NA, "SD", NA, NA),
    "icer_diag" = c(NA,"SD'ed by three", NA, NA)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(20, 25, 28, 30)
  )
  # Get results:
  df_results <- identify_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "delta.c" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "icer" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("identify_all_dominance generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  # Get results:
  df_results <- identify_all_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "delta.c" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "icer" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "dominance" = c("SD", "SD", "SD", NA),
    "icer_diag" = c("SD'ed by two", "SD'ed by four", "SD'ed by four", NA)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(20, 25, 28, 30)
  )
  # Get results:
  df_results <- identify_all_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "delta.c" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "icer" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c(NA_character_, NA_character_, NA_character_, NA_character_)
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("calculate_icers generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  # Get results:
  df_results <- calculate_icers(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, -3, 1, -3),
    "icer" = c(NA, -6, 2, -1.5),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(20, 25, 28, 30)
  )
  # Get results:
  df_results <- calculate_icers(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, 5, 3, 2),
    "icer" = c(NA, 10, 6, 1),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("identify_e_dominance generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, -3, 1, -3),
    "icer" = c(NA, -6, 2, -1.5),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Get results:
  df_results <- identify_e_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA, 0.5, NA, 2),
    "delta.c" = c(NA, -3, NA, -3),
    "icer" = c(NA, -6, NA, -1.5),
    "dominance" = c(NA, NA, "ED", NA),
    "icer_diag" = c("reference", "ICER vs one", "ED'ed by four",
                    "ICER vs three")
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, 5, 3, 2),
    "icer" = c(NA, 10, 6, 1),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Get results:
  df_results <- identify_e_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA, NA, NA, 2),
    "delta.c" = c(NA, NA, NA, 2),
    "icer" = c(NA, NA, NA, 1),
    "dominance" = c(NA, "ED", "ED", NA),
    "icer_diag" = c("reference", "ED'ed by three", "ED'ed by four",
                    "ICER vs three")
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("identify_all_e_dominance generates expected results", {

  # Define test data.frame:
  df_test <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, -3, 1, -3),
    "icer" = c(NA, -6, 2, -1.5),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Get results:
  df_results <- identify_all_e_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(25, 22, 23, 20),
    "delta.e" = c(NA, 0.5, NA, 2.5),
    "delta.c" = c(NA, -3, NA, -2),
    "icer" = c(NA, -6, NA, -0.8),
    "dominance" = c(NA, NA, "ED", NA),
    "icer_diag" = c("reference", "ICER vs one", "ED'ed by four", "ICER vs two")
  )
  # Run test:
  expect_identical(
    df_results, df_expcted
  )

  # Define test data.frame:
  df_test <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA, 0.5, 0.5, 2),
    "delta.c" = c(NA, 5, 3, 2),
    "icer" = c(NA, 10, 6, 1),
    "dominance" = c(NA_character_, NA_character_, NA_character_, NA_character_),
    "icer_diag" = c("reference", "ICER vs one", "ICER vs two", "ICER vs three")
  )
  # Get results:
  df_results <- identify_all_e_dominance(
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c(2, 2.5, 3, 5),
    "Costs" = c(20, 25, 28, 30),
    "delta.e" = c(NA, NA, NA, 3),
    "delta.c" = c(NA, NA, NA, 10),
    "icer" = c(NA, NA, NA, 3.3333),
    "dominance" = c(NA, "ED", "ED", NA),
    "icer_diag" = c("reference", "ED'ed by three", "ED'ed by four",
                    "ICER vs one")
  )
  df_results[4, "icer"] <- round(df_results[4, "icer"], digits = 4)
  # Run test:
  expect_identical(
    df_results, df_expcted
  )
})

test_that("plot_cea_results_table generates expected outputs", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  # Get results:
  df_results <- plot_cea_results_table(
    .df_effects_ = NULL,
    .df_costs_ = NULL,
    .interventions_labels_ = NULL,
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .currency_symbol_ = "\u0024"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c("2.0", "2.5", "3.0", "5.0"),
    "Costs" = c("$25", "$22", "$23", "$20"),
    "delta.e" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "delta.c" = c(NA_real_, NA_real_, NA_real_, NA_real_),
    "icer" = c("SD", "SD", "SD", NA)
  )
  class(df_expcted) <- c(class(df_expcted), "cea_tbl")

  # Run tests:
  expect_identical(df_results, df_expcted)
  expect_true("cea_tbl" %in% class(df_results))

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 40, 23, 90)
  )
  # Get results:
  df_results <- plot_cea_results_table(
    .df_effects_ = NULL,
    .df_costs_ = NULL,
    .interventions_labels_ = NULL,
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .currency_symbol_ = "\u0024"
  )
  # Define expected:
  df_expcted <- data.frame(
    "Intervention" = c("one", "two", "three", "four"),
    "QALYs" = c("2.0", "2.5", "3.0", "5.0"),
    "Costs" = c("$25", "$40", "$23", "$90"),
    "delta.e" = c(NA, NA, NA, "2"),
    "delta.c" = c(NA, NA, NA, "$67"),
    "icer" = c("SD", "SD", NA, "$34")
  )
  class(df_expcted) <- c(class(df_expcted), "cea_tbl")
  # Run test:
  expect_identical(df_results, df_expcted)
  expect_true("cea_tbl" %in% class(df_results))
})

test_that("plot_cea_results_table generates gt objects", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  # Get results:
  df_results <- plot_cea_results_table(
    .df_effects_ = NULL,
    .df_costs_ = NULL,
    .interventions_labels_ = NULL,
    .df_outcomes_ = df_test,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .output_type_ = "html"
  )
  # Run tests:
  expect_true("gt_tbl" %in% class(df_results))
})

test_that("plot_cea_results_table returns no errors or warnings", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  # Run test:
  expect_silent(
    plot_cea_results_table(
      .df_effects_ = NULL,
      .df_costs_ = NULL,
      .interventions_labels_ = NULL,
      .df_outcomes_ = df_test,
      .label_effects_ = "QALYs",
      .label_costs_ = "Costs"
    )
  )
})

test_that("plot_cea_results_table returns errors with erroneous inputs", {

  # Define test data.frame:
  df_test <- data.frame(
    'Intervention' = c("one", "two", "three", "four"),
    'QALYs' = c(2, 2.5, 3, 5),
    'Costs' = c(25, 22, 23, 20)
  )
  ## Wrong type of output:
  expect_error(
    plot_cea_results_table(
      .df_effects_ = NULL,
      .df_costs_ = NULL,
      .interventions_labels_ = NULL,
      .df_outcomes_ = df_test,
      .label_effects_ = "QALYs",
      .label_costs_ = "Costs",
      .wtp_key_values_ = 20000,
      .show_nmb_ = TRUE,
      .show_pce_ = FALSE,
      .output_type_ = "table"
    )
  )
})
