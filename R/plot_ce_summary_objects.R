#' Check plot_cea_results_table Function, and Supporting Functions', Inputs
#'
#' @inheritParams plot_cea_results_table
#'
#' @return Returns nothing or prints error messages as execution is halted
#' when a check fails.
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' check_icers_functions_inputs(
#'   df_outcomes = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs",
#'   .df_effects_ = NULL,
#'   .df_costs_ = NULL,
#'   .interventions_labels_ = NULL
#' )
#' check_icers_functions_inputs(
#'   df_outcomes = NULL,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs",
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = NULL
#' )
#' }
check_icers_functions_inputs <- function(
    .df_outcomes_,
    .label_effects_,
    .label_costs_,
    .output_type_ = NULL,
    .output_format_ = NULL,
    .df_effects_ = NULL,
    .df_costs_ = NULL,
    .interventions_labels_ = NULL,
    .wtp_key_values_ = NULL,
    .show_nmb_ = NULL,
    .show_pce_ = NULL,
    .show_evpi_ = NULL,
    .show_dominance_ = NULL,
    .format_values_ = NULL,
    .currency_symbol_ = NULL,
    .show_diag_ = NULL) {
  if(!is.null(.output_type_)) {
    assertthat::assert_that(
      all(assertthat::is.string(.output_type_),
          .output_type_ %in% c("dataframe", "html", "latex")),
      msg = paste(
        ".output_type_ can only take one of three values: 'dataframe', 'html', or",
        "'latex'."
      )
    )
    assertthat::assert_that(
      all(assertthat::is.string(.output_format_),
          .output_format_ %in% c("wide", "long")),
      msg = paste(
        ".output_format_ can only take one of two values: 'wide' or 'long'"
      )
    )
  }
  if(!is.null(.df_outcomes_)) {
    assertthat::assert_that(
      "data.frame" %in% class(.df_outcomes_),
      msg = ".df_outcomes_ is not a dataframe"
    )
    assertthat::assert_that(
      "Intervention" %in% colnames(.df_outcomes_),
      msg = ".df_outcomes_ is missing a column named 'Intervention'"
    )
  }
  if(all(!is.null(.label_effects_),
         !is.null(.label_costs_))) {
    for (input in c(".label_effects_", ".label_costs_")) {
      assertthat::assert_that(
        assertthat::is.string(get(input)),
        msg = paste(
          input, "is not a character scalar"
        )
      )
    }
  }
  if(all(!is.null(.df_effects_),
         !is.null(.df_costs_),
         !is.null(.interventions_labels_))) {
    for (input in c(".df_effects_", ".df_costs_")) {
      assertthat::assert_that(
        "data.frame" %in% class(get(input)),
        msg =  paste(
          input, "is not a dataframe."
        )
      )
    }
    assertthat::assert_that(
      assertthat::are_equal(
        x = dim(.df_effects_),
        y = dim(.df_costs_)
      ),
      msg = ".df_effects_ and .df_costs_ have unequal dimensions."
    )
    assertthat::assert_that(
      ncol(.df_effects_) > 1,
      msg = paste0(
        "This function creates an incremental Cost-Effectiveness plot which ",
        "requires Probabilistic Sensitivity Analysis (PSA) data for two or more ",
        "interventions."
      )
    )
    assertthat::assert_that(
      assertthat::are_equal(
        x = colnames(.df_effects_),
        y = colnames(.df_costs_)
      ),
      msg = paste(
        "The columns names in the dataframe passed to '.df_effects_' is not",
        "identical to, or not in the same order of, the names of the column in",
        "the dataframes passed to the '.df_costs_' argumnet."
      )
    )
    assertthat::assert_that(
      assertthat::are_equal(
        x = ncol(.df_effects_),
        y = length(.interventions_labels_)
      ),
      msg = paste(
        "Number of columns in .df_effects_/.df_costs_ is not equal to the length of",
        "names passed to the '.interventions_labels_' argument."
      )
    )
    assertthat::assert_that(
      assertthat::are_equal(
        x = colnames(.df_effects_),
        y = names(.interventions_labels_)
      ),
      msg = paste(
        "The names given to the interventions' labels (in the named vector) are",
        "not identical to, or not in the same order of, the names given to the",
        "columns in the dataframes passed to the '.df_effects_' and '.df_costs_'",
        "argumnets."
      )
    )
    for (input in c(".show_nmb_", ".show_pce_", ".show_evpi_", ".show_dominance_",
                    ".show_diag_", ".format_values_")) {
      if(!is.null(get(input))) {
        assertthat::assert_that(
          assertthat::is.flag(get(input)),
          msg = paste(
            input, "is not a logical scalar."
          )
        )
        if(input %in% c(".show_nmb_", ".show_pce_", ".show_evpi_")) {
          if(isTRUE(get(input))) {
            assertthat::assert_that(
              is.numeric(.wtp_key_values_),
              length(.wtp_key_values_) > 0 & length(.wtp_key_values_) < 3,
              msg = paste(
                "Please pass one or two threshold (WTP) values to the",
                "'.wtp_key_values_' argument. .wtp_key_values_ is required to",
                "estimate the NMB and/or PCE."
              )
            )
          }
        }
        if(input %in% ".format_values_") {
          assertthat::assert_that(
            assertthat::is.string(.currency_symbol_),
            msg = paste(
              ".currency_symbol_ is not a character scalar"
            )
          )
        }
      }
    }
  }
}

#' Check and add any missing columns expected by ICER computation functions
#'
#' @description This is a utility function that checks and necessary adds
#' columns to a dataframe.
#'
#' @param .df_check_ Dataframe for which the function will ensure existence, and
#' if required creation, of columns with given names in `.string_columns_` and
#' `.numerics_columns_` arguments.
#' @param .string_columns_ Character vector defining the names of columns of class
#' `character` which the function would check if exist in the `.df_check_`,
#' creating the ones that do not exist before binding them to `.df_check_`.
#' @param .numerics_columns_ Character vector  defining the names of columns of
#' class `numeric` which the function would check if exist in the `.df_check_`,
#' creating the ones that do not exist before binding them to `.df_check_`.
#'
#' @return A dataframe including all the data in `.df_check_` in addition to empty
#' columns with names passed to `.string_columns_` and `.numerics_columns_`.
#' @export
#'
#' @examples
#' \dontrun{
#' df_test <- data.frame(
#'   "a" = 1:5,
#'   "b" = LETTERS[1:5],
#'   "c" = 6:10
#' )
#' df2 <- add_missing_columns(
#'   .df_check_ = df_test,
#'   .string_columns_ = c("icer label", "dominance"),
#'   .numerics_columns_ = c("qalys", "costs")
#' )
#' }
add_missing_columns <- function(
    .df_check_,
    .string_columns_,
    .numerics_columns_) {

  # Sanity checks:
  assertthat::assert_that(
    "data.frame" %in% class(.df_check_),
    msg = ".df_check_ is not a dataframe"
  )
  for (input in c(".string_columns_", ".numerics_columns_")) {
    assertthat::assert_that(
      if(!is.null(get(input))) {
        "character" %in% class(get(input))
      } else {
        is.null(get(input))
      },
      msg = paste(
        input, "is not a vector of characters"
      )
    )
  }

  # Add an Id column if not existing:
  df_colnames <- colnames(.df_check_)

  # Check for missing columns:
  missing_nms <- setdiff(c(.numerics_columns_, .string_columns_), df_colnames)

  # In case there were missing columns:
  if(!length(missing_nms) == 0) {
    # Create missing columns:
    .df_check_ <- .df_check_ %>%
      cbind(
        missing_nms %>%
          `names<-`(missing_nms) %>%
          purrr::map(
            .f = function(colname) {
              ifelse(colname %in% .numerics_columns_, NA_real_, NA_character_)
            }
          )
      )
  }

  return(.df_check_)
}

#' Identify dominated interventions
#'
#' @description
#' This function identifies dominated interventions, ones that are more
#' effective but cheaper compared to the decision options ranked by effects.
#' Strong dominance rules out any of the competing interventions if there is
#' another intervention that is both more effective and less costly.
#'
#' @inheritParams plot_cea_results_table
#'
#' @return A dataframe or table containing intervention names, mean costs and
#' effects corresponding to each intervention and information about which
#' interventions were strongly dominated.
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' df_dominance1 <- identify_dominance(
#'   .df_outcomes_ = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' df_dominance2 <- identify_dominance(
#'   .df_outcomes_ = df_dominance1,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' }
identify_dominance <- function(
    .df_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs") {

  # Sanity checks:
  check_icers_functions_inputs(
    .df_outcomes_ = .df_outcomes_,
    .label_effects_ = .label_effects_,
    .label_costs_ = .label_costs_
  )

  # Check if missing key columns and create them if so:
  df_dominance <- add_missing_columns(
    .df_check_ = .df_outcomes_,
    .string_columns_ = c("dominance", "icer_diag"),
    .numerics_columns_ = c("delta.e", "delta.c", "icer")
  )

  # Identify dominated interventions:
  df_dominance <- df_dominance[order(
    df_dominance[[.label_effects_]], df_dominance[[.label_costs_]]
  ), ]

  if(sum(is.na(df_dominance$dominance)) > 0){

    ## Subset data to ignore interventions strongly dominated
    subset_data <- df_dominance[is.na(df_dominance$dominance), ]

    ## Get the leading costs values
    costs_lead <- c(subset_data[[.label_costs_]][-1], NA)

    ## Get possibly strongly dominating intervention
    intervention_lead <- subset_data$Intervention[-1]
    SD_label <- c(paste(
      "SD'ed by",
      intervention_lead
    ), NA)

    ## Identify strong dominance and label them accordingly
    subset_data$dominance <- ifelse(
      test = costs_lead < subset_data[[.label_costs_]],
      yes = "SD",
      no = subset_data$dominance
    )

    ## Provide feedback
    subset_data$icer_diag <- ifelse(
      test = costs_lead < subset_data[[.label_costs_]],
      yes = SD_label,
      no = subset_data$icer_diag
    )

    ## Merge calculation to main results table
    df_dominance[is.na(df_dominance$dominance), ] <- subset_data
  }

  return(df_dominance)
}

#' Identify all dominated interventions
#'
#' @description
#' This function iteratively employs the \link{identify_dominance} function to
#' identify all strongly dominated interventions.
#'
#' @inheritParams plot_cea_results_table
#'
#' @return A dataframe or table containing intervention names, mean costs and
#' effects corresponding to each intervention and information about which
#' interventions were strongly dominated.
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' df_dominance <- identify_all_dominance(
#'   .df_outcomes_ = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' }
identify_all_dominance <- function(
    .df_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs") {

  # Check if missing key columns and create them if so:
  df_dominance <- add_missing_columns(
    .df_check_ = .df_outcomes_,
    .string_columns_ = c("dominance", "icer_diag"),
    .numerics_columns_ = c("delta.e", "delta.c", "icer")
  )

  # Loop over the data until all strong dominance were identified:
  while ("SD" %in% (identify_dominance(
    .df_outcomes_ = df_dominance %>%
    dplyr::filter(is.na(.data[["dominance"]])),
    .label_effects_ = .label_effects_,
    .label_costs_ = .label_costs_
  ) %>%
  purrr::pluck("dominance"))) {

    ## Identify strong dominance:
    df_dominance <- identify_dominance(
      .df_outcomes_ = df_dominance,
      .label_effects_ = .label_effects_,
      .label_costs_ = .label_costs_
    )
  }

  return(df_dominance)
}

#' Calculate the Incremental Cost Effectiveness Ratio ICER(s) and effects and
#' costs differentials
#'
#' @description
#' This function estimates the Incremental Cost Effectiveness Ratio ICER(s).
#'
#' @inheritParams plot_cea_results_table
#'
#' @return A dataframe or table containing intervention names, mean outcomes
#' (costs and effects), differential outcomes corresponding to each intervention
#' and the Incremental Cost Effectiveness Ratios (ICER)s for qualified
#' comparators.
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' df_icer <- calculate_icers(
#'   .df_outcomes_ = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' }
calculate_icers <- function(
    .df_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs") {

  # Sanity checks:
  check_icers_functions_inputs(
    .df_outcomes_ = .df_outcomes_,
    .label_effects_ = .label_effects_,
    .label_costs_ = .label_costs_
  )

  # Check if missing key columns and create them if so:
  df_icer <- add_missing_columns(
    .df_check_ = .df_outcomes_,
    .string_columns_ = c("dominance", "icer_diag"),
    .numerics_columns_ = c("delta.e", "delta.c", "icer")
  )

  # Calculate ICERs:
  df_icer <- df_icer[order(
    df_icer[[.label_effects_]], df_icer[[.label_costs_]]
  ), ]

  if(sum(is.na(df_icer$dominance)) > 0){

    ## Subset data to ignore interventions strongly or extendedly dominated
    subset_data <- df_icer[is.na(df_icer$dominance), ]

    ## Compute differential outcomes, `delta.c` and `delta.e`
    subset_data$delta.c <- c(NA, diff(subset_data[[.label_costs_]]))
    subset_data$delta.e <- c(NA, diff(subset_data[[.label_effects_]]))

    ## Compute the Incremental Cost Effectiveness Ratios (ICER)s
    subset_data$icer <- subset_data$delta.c / subset_data$delta.e

    ## Provide feedback
    subset_data$icer_diag[!is.na(subset_data$icer)] <- paste0(
      "ICER vs ",
      subset_data$Intervention[-nrow(subset_data)]
    )
    subset_data$icer_diag[is.na(subset_data$icer)] <- "reference"

    df_icer[is.na(df_icer$dominance), ] <- subset_data
  }

  return(df_icer)
}

#' Identify extendedly (weakly) dominated interventions
#'
#' @description
#' This function employs the \link{calculate_icers} functions to identify
#' waekly dominated interventions, interventions with Incremental Cost
#' Effectiveness Ratio (ICER) higher than more expensive/effective
#' interventions.
#' Extended dominance (weak dominance) rules out any intervention that has an
#' incremental cost-effectiveness ratio that is greater than that of a more
#' effective intervention.
#'
#' @inheritParams plot_cea_results_table
#'
#' @return A dataframe or table containing the information included in
#' `.df_outcomes_` in addition to information about which interventions were
#' extendedly dominated.
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' df_icer <- calculate_icers(
#'   .df_outcomes_ = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' df_e_dominance <- identify_e_dominance(
#'   .df_outcomes_ = df_icer,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' df_icer2 <- calculate_icers(
#'   .df_outcomes_ = df_e_dominance,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' }
identify_e_dominance <- function(
    .df_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs") {

  # Sanity checks:
  check_icers_functions_inputs(
    .df_outcomes_ = .df_outcomes_,
    .label_effects_ = .label_effects_,
    .label_costs_ = .label_costs_
  )

  # Check if missing key columns and create them if so:
  df_e_dominance <- add_missing_columns(
    .df_check_ = .df_outcomes_,
    .string_columns_ = c("dominance", "icer_diag"),
    .numerics_columns_ = NULL
  )

  # Identify extendedly dominated interventions:
  if(sum(is.na(df_e_dominance$dominance) & !is.na(df_e_dominance$icer)) > 0){

    ## Subset data to ignore interventions strongly dominated
    subset_data <- df_e_dominance[
      is.na(df_e_dominance$dominance) & !is.na(df_e_dominance$icer),
    ]

    ## Get the leading icers values
    icers_lead <- c(subset_data[["icer"]][-1],
                    subset_data[["icer"]][nrow(subset_data)])

    ## Get possibly strongly dominating intervention
    intervention_lead <- subset_data$Intervention[-1]
    ED_label <- c(paste(
      "ED'ed by",
      intervention_lead
    ), NA)

    ## Identify strong dominance and label them accordingly
    subset_data$dominance <- ifelse(
      test = icers_lead < subset_data[["icer"]],
      yes = "ED",
      no = subset_data$dominance
    )

    ## Provide feedback
    subset_data$icer_diag <- ifelse(
      test = icers_lead < subset_data[["icer"]],
      yes = ED_label,
      no = subset_data$icer_diag
    )

    ## Clear ICERs and outcomes differentials
    for(col_name in c("icer", "delta.e", "delta.c")) {
      subset_data[[col_name]] <- ifelse(
        test = icers_lead < subset_data[["icer"]],
        yes = NA,
        no = subset_data[[col_name]]
      )
    }

    ## Merge calculation to main results table
    df_e_dominance[is.na(df_e_dominance$dominance) &
                     !is.na(df_e_dominance$icer), ] <- subset_data
  }

  return(df_e_dominance)
}

#' Identify all extendedly (weakly) dominated interventions
#'
#' @description
#' This function iteratively employs the \link{identify_e_dominance} and
#' \link{calculate_icers} functions to identify all waekly dominated
#' interventions.
#'
#' @inheritParams plot_cea_results_table
#'
#' @return A dataframe or table containing the information included in
#' `.df_outcomes_` in addition to information about which interventions were
#' extendedly dominated.
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat %>% unname(),
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e) %>% unname(),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c) %>% unname()
#' )
#' df_icer <- calculate_icers(
#'   .df_outcomes_ = df_outcomes,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' df_e_dominance <- identify_all_e_dominance(
#'   .df_outcomes_ = df_icer,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' df_icer2 <- calculate_icers(
#'   .df_outcomes_ = df_e_dominance,
#'   .label_effects_ = "QALYs",
#'   .label_costs_ = "Costs"
#' )
#' }
identify_all_e_dominance <- function(
    .df_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs") {

  # Check if missing key columns and create them if so:
  df_e_dominance <- add_missing_columns(
    .df_check_ = .df_outcomes_,
    .string_columns_ = c("dominance", "icer_diag"),
    .numerics_columns_ = NULL
  )

  # Loop over the data until all stong dominance were identified:
  while ("ED" %in% (identify_e_dominance(
    .df_outcomes_ = df_e_dominance %>%
    dplyr::filter(is.na(.data[["dominance"]])),
    .label_effects_ = .label_effects_,
    .label_costs_ = .label_costs_
  ) %>%
  purrr::pluck("dominance"))) {

    ## Identify strong dominance:
    df_e_dominance <- calculate_icers(
      .df_outcomes_ = identify_e_dominance(
        .df_outcomes_ = df_e_dominance,
        .label_effects_ = .label_effects_,
        .label_costs_ = .label_costs_
      ),
      .label_effects_ = .label_effects_,
      .label_costs_ = .label_costs_
    )
  }

  return(df_e_dominance)
}

#' Generate HTML or Latex table
#'
#' @description This function creates a publication ready version of the
#' data.frame created by the \link{plot_cea_results_table} function.
#'
#' @param .df_cea_results_ Object of class "icer_tbl" returned by
#' \link{plot_cea_results_table}.
#' @param .output_contents_ Character vector specifying the contents of the
#' generated Cost-Effectiveness Results table. The default is `"all"` which
#' returns the average values of the outcomes, incremental analysis values,
#' Net Monetary Benefit (NMB), Probability of Cost-Effectiveness (PCE),
#' and Expected Value of Perfect Information (EVPI). This argument accepts any
#' of the arguments`c("means", "incremental", "NMB", "PCE", "EVPI")`.
#' @param .output_grouped_ Logical scalar for whether to make row groups as
#' columns. Default is `FALSE`.
#' @param .output_group_ Character scalar specifying the name of the column on
#' which the generated output is grouped. Default is `NULL` where only the
#' original row groups are converted to a column. This argument is ignored if
#' `.output_grouped_` is `FALSE`.
#' @inheritParams plot_cea_results_table
#'
#' @return Object of class "gt_tbl"
#'
#' @examples
#' \dontrun{
#' df_cea_results_wide <- plot_cea_results_table(
#'    .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'    .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'    .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                          `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'    .df_outcomes_ = NULL,
#'    .wtp_key_values_ = 20000,
#'    .show_nmb_ = TRUE,
#'    .show_pce_ = TRUE,
#'    .show_evpi_ = TRUE,
#'    .format_values_ = TRUE,
#'    .currency_symbol_ = "\u00A3",
#'    .show_diag_ = FALSE,
#'    .output_format_ = "wide"
#' )
#' gt_cea_results_wide <- generate_gt_table(
#'    .df_cea_results_ = df_cea_results_wide,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = 20000,
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .output_format_ = "wide"
#' )
#' df_cea_results_long <- plot_cea_results_table(
#'    .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'    .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'    .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                          `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'    .df_outcomes_ = NULL,
#'    .wtp_key_values_ = 20000,
#'    .show_nmb_ = TRUE,
#'    .show_pce_ = TRUE,
#'    .show_evpi_ = TRUE,
#'    .format_values_ = TRUE,
#'    .currency_symbol_ = "\u00A3",
#'    .show_diag_ = FALSE,
#'    .output_format_ = "long"
#' )
#' gt_cea_results_long1 <- generate_gt_table(
#'    .df_cea_results_ = df_cea_results_long,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = 20000,
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .output_format_ = "long",
#'    .output_grouped_ = FALSE,
#'    .output_group_ = NULL
#' )
#' gt_cea_results_long2 <- generate_gt_table(
#'    .df_cea_results_ = df_cea_results_long,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = 20000,
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .output_format_ = "long",
#'    .output_grouped_ = TRUE,
#'    .output_group_ = NULL
#' )
#' }
generate_gt_table <- function(
    .df_cea_results_,
    .label_effects_,
    .label_costs_,
    .wtp_key_values_,
    .currency_symbol_,
    .output_type_,
    .output_format_,
    .output_title_ = NULL,
    .output_subtitle_ = NULL,
    .output_contents_ = "all",
    .output_grouped_ = FALSE,
    .output_group_ = NULL) {
  ## Sanity checks:
  assertthat::assert_that(
    "icer_tbl" %in% class(.df_cea_results_),
    msg = paste(
      ".df_cea_results_ is not of class icer_tbl. icer_tbl is a data.frame",
      "generated by the function plot_cea_results_table if called using the",
      "default value of the argument .output_type_."
    )
  )
  ## Check if a subset of the data is required:
  if("all" %in% .output_contents_) {
    .output_contents_ <- c("means", "incremental", "NMB", "PCE", "EVPI")
  } else if(
    any(c("means", "incremental", "NMB", "PCE", "EVPI") %in% .output_contents_)
  ) {
    ### Subset the table as required:
    if(.output_format_ == "long") {
      input_contents <- .df_cea_results_[[1]]
    } else {
      input_contents <- colnames(.df_cea_results_)
    }
    if(!"means" %in% .output_contents_) {
      input_contents <- input_contents[
        !input_contents %in% c(.label_costs_, .label_effects_)
      ]
    }
    if(!"incremental" %in% .output_contents_) {
      input_contents <- input_contents[
        !input_contents %in% c("delta.c", "delta.e", "icer")
      ]
    }
    if(!"NMB" %in% .output_contents_) {
      input_contents <- input_contents[
        -grep(
          pattern = "NMB@",
          x = input_contents
        )
      ]
    }
    if(!"PCE" %in% .output_contents_) {
      input_contents <- input_contents[
        -grep(
          pattern = "PCE@",
          x = input_contents
        )
      ]
    }
    if(!"EVPI" %in% .output_contents_) {
      input_contents <- input_contents[
        -grep(
          pattern = "EVPI@",
          x = input_contents
        )
      ]
    }
    if(.output_format_ == "long") {
      .df_cea_results_ <- .df_cea_results_[
        .df_cea_results_[[1]] %in% input_contents,
      ]
    } else {
      .df_cea_results_ <- .df_cea_results_[
        , input_contents
      ]
    }
  } else {
    .output_contents_ <- c("means", "incremental", "NMB", "PCE", "EVPI")
  }

  ## Check if grouped data is required and grouping variable exists:
  if(isTRUE(.output_grouped_)) {
    if(!is.null(.output_group_)) {
      if(is.null(.df_cea_results_[[.output_group_]])) {
        .output_group_ <- NULL
      }
    }
  }

  ## Identify NMB, PCE or EVPI if not passed to function:
  nmb_col_index <- if(.output_format_ == "long") {
    grep(
      pattern = "NMB@",
      x = .df_cea_results_[[1]]
    )
  } else {
    grep(
      pattern = "NMB@",
      x = colnames(.df_cea_results_)
    )
  }
  pce_col_index <- if(.output_format_ == "long") {
    grep(
      pattern = "PCE@",
      x = .df_cea_results_[[1]]
    )
  } else {
    grep(
      pattern = "PCE@",
      x = colnames(.df_cea_results_)
    )
  }
  evpi_col_index <- if(.output_format_ == "long") {
    grep(
      pattern = "EVPI@",
      x = .df_cea_results_[[1]]
    )
  } else {
    grep(
      pattern = "EVPI@",
      x = colnames(.df_cea_results_)
    )
  }
  ## Identify Incremental indexes in case a subset is required:
  inc_col_index <- if(.output_format_ == "long") {
    grep(
      pattern = "delta.e|delta.c|icer",
      x = .df_cea_results_[[1]]
    )
  } else {
    grep(
      pattern = "delta.e|delta.c|icer",
      x = colnames(.df_cea_results_)
    )
  }

  ## Prepare WTP related column names:
  wtp_col_names <- paste0(
    .currency_symbol_,
    format(
      x = .wtp_key_values_,
      trim = TRUE,
      big.mark = ","
    )
  )

  ## Build table footer:
  table_footnote <- NULL
  if("incremental" %in% .output_contents_) {
    table_footnote <- "**ICER**: Incremental Cost-Effectiveness Ratio."
  }

  if(.output_format_ == "long") {
    ## Long formatted tables:

    ### Proper rows' names:
    if("incremental" %in% .output_contents_) {
      delta.e_name <- paste("Incremental", .label_effects_)
      delta.c_name <- paste("Incremental", .label_costs_)
    }

    ### Edit NMB, PCE and EVPI from the measures column:
    .df_cea_results_[[" "]] <- gsub(
      pattern = "NMB@|PCE@|EVPI@",
      replacement = "",
      x = .df_cea_results_[[" "]],
      ignore.case = FALSE
    )
    for (wtp_value in 1:length(.wtp_key_values_)) {
      .df_cea_results_[[" "]] <- gsub(
        pattern = .wtp_key_values_[wtp_value],
        replacement = wtp_col_names[wtp_value],
        x = .df_cea_results_[[" "]],
        ignore.case = FALSE
      )
    }
    if(isTRUE(.output_grouped_)) {
      .df_cea_results_ <- .df_cea_results_ %>%
        `class<-`("data.frame") %>%
        dplyr::mutate(
          "row_group" = dplyr::case_when(
            .data[[" "]] %in% c(.label_costs_, .label_effects_) ~
              "Mean Outcomes",
            dplyr::row_number() %in% inc_col_index ~
              "Incremental Analysis",
            dplyr::row_number() %in% nmb_col_index ~
              "NMB",
            dplyr::row_number() %in% pce_col_index ~
              "PCE",
            dplyr::row_number() %in% evpi_col_index ~
              "EVPI",
            TRUE ~ NA
          )
        ) %>%
        {
          if(!is.null(.output_group_)) {
            dplyr::mutate(
              .data = .,
              "row_group" = paste(
                .data[[.output_group_]],
                "-" ,
                .data[["row_group"]]
              ),
              {{.output_group_}} := NULL
            ) %>%
              dplyr::group_by(
                .data = .,
                .data[["row_group"]]
              )
          } else {
            dplyr::group_by(
              .data = .,
              .data[["row_group"]]
            )
          }
        }
    } else {
      .df_cea_results_ <- .df_cea_results_ %>%
        `class<-`("data.frame")
    }

    .df_cea_results_ <- .df_cea_results_ %>%
      {
        if("incremental" %in% .output_contents_) {
          dplyr::mutate(
            .data = .,
            " " = dplyr::case_when(
              .data[[" "]] == "delta.e" ~ delta.e_name,
              .data[[" "]] == "delta.c" ~ delta.c_name,
              .data[[" "]] == "icer" ~ "ICER",
              TRUE ~ .data[[" "]]
            )
          )
        } else {
          .
        }
      }

    ### Make .df_cea_results_ a gt object and edit columns names:
    gt_cea_results <- gt::sub_missing(
      data = gt::gt(
        data = .df_cea_results_,
        rowname_col = " "
      ),
      missing_text = "_"
    )

    ### Get NMB and Probability being cost-effective column names:
    if(length(evpi_col_index) > 0) {
      if(!.output_grouped_) {
        gt_cea_results <- gt::tab_row_group(
          data = gt_cea_results,
          label = gt::md("**EVPI**"),
          rows = evpi_col_index
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**EVPI**: Expected Value of Perfect Information."
      )
    }
    if(length(pce_col_index) > 0) {
      if(!.output_grouped_) {
        gt_cea_results <- gt::tab_row_group(
          data = gt_cea_results,
          label = gt::md("**PCE**"),
          rows = pce_col_index
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**PCE**: Probability Cost-Effectiveness."
      )
    }
    if(length(nmb_col_index) > 0) {
      if(!.output_grouped_) {
        gt_cea_results <- gt::tab_row_group(
          data = gt_cea_results,
          label = gt::md("**NMB**"),
          rows = nmb_col_index
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**NMB**: Net Monetory Benefit."
      )
    }

    ### Other row groups:
    if(!isTRUE(.output_grouped_)) {
      gt_cea_results <- gt_cea_results %>%
        {
          if("incremental" %in% .output_contents_) {
            gt::tab_row_group(
              data = .,
              label = gt::md("**Incremental Analysis**"),
              rows = inc_col_index
            )
          } else{
            .
          }
        } %>%
        {
          if("means" %in% .output_contents_) {
            gt::tab_row_group(
              data = .,
              label = gt::md("**Mean Outcomes**"),
              rows = 1:2
            )
          } else{
            .
          }
        }
    } else {
      gt_cea_results <- gt_cea_results %>%
        gt::tab_style(
          style = gt::cell_text(
            weight = "bold",
            v_align = "middle"
          ),
          locations = list(
            gt::cells_column_labels(),
            gt::cells_row_groups()
          )
        ) %>%
        gt::tab_options(
          row_group.as_column = TRUE
        )
    }

    ## Make columns bold and add footnote(s) to table:
    if("incremental" %in% .output_contents_) {
      dominance <- .df_cea_results_[.df_cea_results_[[" "]] == "ICER", ]
      if("SD" %in% dominance) {
        table_footnote <- c(
          table_footnote,
          "**SD**: Stongly dominated by a competing option."
        )
      }
      if("ED" %in% dominance) {
        table_footnote <- c(
          table_footnote,
          "**ED**: Weakly (or extendedly) dominated by a competing option."
        )
      }
    }

  } else {
    ## Wide formatted tables:

    ### Proper columns' names:
    delta.e_name <- .label_effects_
    delta.c_name <- .label_costs_

    ### Make .df_cea_results_ a gt object and edit columns names:
    gt_cea_results <- gt::sub_missing(
      data = gt::gt(
        data = .df_cea_results_
      ),
      missing_text = "_"
    ) %>%
      {
        if("incremental" %in% .output_contents_) {
          gt::cols_label(
            .data = .,
            "delta.e" = delta.e_name,
            "delta.c" = delta.c_name,
            "icer" = "ICER"
          )
        } else{
          .
        }
      } %>%
      gt::cols_align(
        data = .,
        align = "center",
        columns = 2:ncol(.df_cea_results_)
      ) %>%
      {
        if("incremental" %in% .output_contents_) {
          gt::tab_spanner(
            data = .,
            label = "Incremental",
            columns = inc_col_index
          )
        } else{
          .
        }
      } %>%
      {
        if("means" %in% .output_contents_) {
          gt::tab_spanner(
            data = .,
            label = "Mean",
            columns = 2:3
          )
        } else{
          .
        }
      }

    ### Get NMB and Probability being cost-effective and EVPI column names
    if(length(nmb_col_index) > 0) {
      gt_cea_results <- gt::tab_spanner(
        data = gt_cea_results,
        label = "NMB",
        columns = nmb_col_index
      )

      for (wtp_value in 1:length(.wtp_key_values_)) {
        col_name <- colnames(.df_cea_results_)[nmb_col_index][wtp_value]
        new_col_name <- wtp_col_names[wtp_value]
        gt_cea_results <- gt::cols_label(
          .data = gt_cea_results,
          {{col_name}} := new_col_name
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**NMB**: Net Monetory Benefit."
      )
    }
    if(length(pce_col_index) > 0) {
      gt_cea_results <- gt::tab_spanner(
        data = gt_cea_results,
        label = "PCE",
        columns = pce_col_index
      )

      for (wtp_value in 1:length(.wtp_key_values_)) {
        col_name <- colnames(.df_cea_results_)[pce_col_index][wtp_value]
        new_col_name <- wtp_col_names[wtp_value]
        gt_cea_results <- gt::cols_label(
          .data = gt_cea_results,
          {{col_name}} := new_col_name
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**PCE**: Probability Cost-Effectiveness."
      )
    }
    if(length(evpi_col_index) > 0) {
      gt_cea_results <- gt::tab_spanner(
        data = gt_cea_results,
        label = "EVPI",
        columns = evpi_col_index
      )

      for (wtp_value in 1:length(.wtp_key_values_)) {
        col_name <- colnames(.df_cea_results_)[evpi_col_index][wtp_value]
        new_col_name <- wtp_col_names[wtp_value]
        gt_cea_results <- gt::cols_label(
          .data = gt_cea_results,
          {{col_name}} := new_col_name
        )
      }

      #### Update footnote:
      table_footnote <- c(
        table_footnote,
        "**EVPI**: Expected Value of Perfect Information."
      )
    }

    ## Make columns bold and add footnote(s) to table:
    if("incremental" %in% .output_contents_) {
      if("SD" %in% .df_cea_results_$icer ) {
        table_footnote <- c(
          table_footnote,
          "**SD**: Stongly dominated by a competing option."
        )
      }
      if("ED" %in% .df_cea_results_$icer) {
        table_footnote <- c(
          table_footnote,
          "**ED**: Weakly (or extendedly) dominated by a competing option."
        )
      }
    }
  }

  gt_cea_results <- gt::tab_source_note(
    data = gt_cea_results,
    source_note = gt::md(
      table_footnote %>%
        sort() %>%
        paste(collapse = " ")
    )
  ) %>%
    gt::cols_label_with(
      fn = function(x) {
        gt::md(
          paste0("**", x, "**")
        )
      }
    )

  ## Add title/subtitle:
  gt_cea_results <- gt_cea_results %>%
    {if(!is.null(.output_title_) & is.null(.output_subtitle_)){
      gt::tab_header(
        data = .,
        title = gt::md(.output_title_)
      )
    } else if(!is.null(.output_title_) & !is.null(.output_subtitle_)){
      gt::tab_header(
        data = .,
        title = gt::md(.output_title_),
        subtitle = gt::md(.output_subtitle_)
      )
    } else if(is.null(.output_title_) & !is.null(.output_subtitle_)){
      gt::tab_header(
        data = .,
        title = "",
        subtitle = gt::md(.output_subtitle_))
    } else {
      .
    }} %>%
    gt::opt_align_table_header(
      align = "left"
    )

  ## Change output if required:
  if(.output_type_ == "latex") {
    gt_cea_results <- gt::as_latex(
      data = gt_cea_results
    )
  }

  return(gt_cea_results)
}

#' Draw Cost Effectiveness Results Table
#'
#' @description
#' This function employs the \link{identify_all_dominance},
#' \link{calculate_icers} and \link{identify_all_e_dominance} functions to
#' draw a cost-effectiveness results table. This function allows users to either
#' pass a table of the mean values of the outcomes (costs and effects), or the
#' Probabilistic Sensitivity Analysis (PSA) costs and effects data. The latter
#' gives the user the option to add the probability-of-being-cost-effective and
#' the Net Monetary Benefits (NMB) to the returned table. Therefore, in addition
#' to the other arguments this function strictly accepts either the set
#' of `.df_effects_`, `.df_costs_`, and `.interventions_labels_` or `.df_outcomes_`.
#' The function will ignore `.df_outcomes_` if all four arguments were passed.
#'
#' @param .df_effects_ Dataframe or tibble containing the effects values from PSA.
#' The number of columns should equate to the number of interventions names
#' passed to `.interventions_labels_` while the number of rows should equate the
#' number of PSA simulations to be summarised. Also, the names of the columns in
#' `.df_effects_` should be identical to and appear in the same order to those of
#' the interventions' names in `.interventions_labels_`.
#' @param .df_costs_ Dataframe or tibble containing the costs values from PSA.
#' The dimensions (rows and columns) and column names of `.df_costs_` should be
#' identical to those of `.df_effects_`.
#' @param .interventions_labels_ Named Character vector containing the names of
#' all interventions. The names (of the elements (the labels) in the named
#' vector) should be identical to and in the same order of the column names in
#' the dataframes passed to the `.df_effects_` and `.df_costs_` arguments.
#' @param .df_outcomes_ Dataframe or tibble containing average costs and effects
#' data.
#' @param .wtp_key_values_ Numeric vector specifying the willingness-to-pay (WTP)
#' values to be used in estimating the NMB and probability of the optimal choice
#' being cost-effective. This argument takes effect if `.show_nmb_` is `TRUE`,
#' and accepts a maximum of two values. Default is `c(20000, 30000)`.
#' @param .highlight_optimal_choices_ Logical for whether to report NMB, PCE or
#' EVPI for the optimal choice. Default is `TRUE`. If `FALSE`, the values of
#' NMB, PCE or EVPI (if any was requested by setting `.show_nmb_`, `.show_pce_`
#' or `.show_evpi_` to `TRUE`) will be reported for all options.
#' @param .show_nmb_ Logical for whether to show the expected Net Monetary
#' Benefits (NMB) in the returned table. Requires `.wtp_key_values_` not to be
#' `NULL`.
#' @param .show_pce_ Logical for whether to show the
#' probability-of-being-cost-effective in the returned table. Requires
#' `.wtp_key_values_` not to be `NULL`.
#' @param .show_evpi_ Logical for whether to show individual Expected Value of
#' Perfect Information (EVPI) in the returned table. Requires `.wtp_key_values_`
#' not to be `NULL`.
#' @param .show_dominance_ Logical for whether to show dominance information in
#' the returned table.
#' @param .format_values_ Logical for whether to return a formatted table.
#' Default is `TRUE`.
#' @param .output_format_ Character scalar taking one of two options: `"wide"`
#' or `"long"` specifying whether the generated results table would be in a wide
#' or long format, respectively.
#' @param .output_title_ Character scalar specifying the title of the generated
#' table Ignored if `.output_type_` is the default `"dataframe"`.
#' @param .output_subtitle_ Character scalar specifying the subtitle of the
#' generated table. Ignored if `.output_type_` is the default `"dataframe"`.
#' @param .show_diag_ Logical for whether to keep calculations diagnostics
#' column.
#' @inheritParams generate_cea_results_tables
#'
#' @return A object of class gt_tbl or a dataframe of class "icer_tbl"
#' containing intervention names, outcomes (costs and effects) mean values,
#' outcomes differential values, Incremental Cost Effectiveness Ratios (ICER)s
#' for qualified comparators and information about which interventions were
#' extendedly dominated (if requested by the user). Upon request, and with the
#' necessary inputs, the returned table could include the NMB and probability of
#' an intervention being cost-effective at user-defined WTP values.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_outcomes <- data.frame(
#'   'Intervention' = ShinyPSA::Hyperphosphatemia_PSA$treat,
#'   'QALYs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$e),
#'   'Costs' = colMeans(ShinyPSA::Hyperphosphatemia_PSA$c)
#' )
#' cea_results_table1.1 <- plot_cea_results_table(
#'   .df_effects_ = NULL,
#'   .df_costs_ = NULL,
#'   .interventions_labels_ = NULL,
#'   .df_outcomes_ = df_outcomes,
#'   .format_values_ = TRUE,
#'   .show_diag_ = FALSE,
#'   .output_format_ = "wide"
#' )
#' cea_results_table1.2 <- plot_cea_results_table(
#'   .df_effects_ = NULL,
#'   .df_costs_ = NULL,
#'   .interventions_labels_ = NULL,
#'   .df_outcomes_ = df_outcomes,
#'   .format_values_ = TRUE,
#'   .output_type_ = "html",
#'   .output_title_ = "Cost-Effectiveness Results",
#'   .output_subtitle_ = "Table shows cost-effectiveness analysis results",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table2 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = NULL,
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .show_diag_ = FALSE
#' )
#' identical(cea_results_table1.1, cea_results_table2)
#' cea_results_table3 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .format_values_ = TRUE,
#'   .show_diag_ = FALSE
#' )
#' cea_results_table4 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .show_dominance_ = FALSE,
#'   .format_values_ = FALSE,
#'   .show_diag_ = FALSE
#' )
#' cea_results_table5 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table6 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.1 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.2 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.3 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = TRUE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.4 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.5 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = TRUE,
#'   .format_values_ = TRUE,
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table7.6 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = TRUE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .output_format_ = "long",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table8 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(20000, 30000),
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "latex",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table9 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hyperphosphatemia_PSA$e,
#'   .df_costs_ = ShinyPSA::Hyperphosphatemia_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hyperphosphatemia_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hyperphosphatemia_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = NULL,
#'   .show_nmb_ = FALSE,
#'   .show_pce_ = FALSE,
#'   .show_evpi_ = FALSE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table10 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hypothetical_PSA$e,
#'   .df_costs_ = ShinyPSA::Hypothetical_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hypothetical_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hypothetical_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(12980, 20000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = TRUE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "html",
#'   .output_format_ = "long",
#'   .show_diag_ = FALSE
#' )
#' cea_results_table11 <- plot_cea_results_table(
#'   .df_effects_ = ShinyPSA::Hypothetical_PSA$e,
#'   .df_costs_ = ShinyPSA::Hypothetical_PSA$c,
#'   .interventions_labels_ = ShinyPSA::Hypothetical_PSA$treat %>%
#'                           `names<-`(colnames(ShinyPSA::Hypothetical_PSA$e)),
#'   .df_outcomes_ = NULL,
#'   .wtp_key_values_ = c(12980, 20000),
#'   .show_nmb_ = TRUE,
#'   .show_pce_ = TRUE,
#'   .show_evpi_ = TRUE,
#'   .format_values_ = TRUE,
#'   .currency_symbol_ = "\u00A3",
#'   .output_type_ = "dataframe",
#'   .output_format_ = "long",
#'   .show_diag_ = FALSE
#' )
#' }
plot_cea_results_table <- function(
    .df_effects_,
    .df_costs_,
    .interventions_labels_,
    .df_outcomes_ = NULL,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .wtp_key_values_ = c(20000, 30000),
    .highlight_optimal_choices_ = TRUE,
    .show_nmb_ = TRUE,
    .show_pce_ = TRUE,
    .show_evpi_ = TRUE,
    .show_dominance_ = TRUE,
    .format_values_ = TRUE,
    .currency_symbol_ = "\u00A3",
    .output_type_ = "dataframe",
    .output_format_ = "wide",
    .output_title_ = NULL,
    .output_subtitle_ = NULL,
    .show_diag_ = FALSE) {
  # Default output type to dataframe if NULL:----
  if(is.null(.output_type_)) .output_type_ <- "dataframe"
  if(is.null(.output_format_)) .output_format_ <- "wide"

  # If .df_outcomes_ was passed to the function:----
  if(all(!is.null(.df_outcomes_),
         !is.null(.label_effects_),
         !is.null(.label_costs_),
         is.null(.df_effects_))) {

    ## Sanity checks:----
    check_icers_functions_inputs(
      .df_outcomes_ = .df_outcomes_,
      .label_effects_ = .label_effects_,
      .label_costs_ = .label_costs_,
      .output_type_ = .output_type_,
      .output_format_ = .output_format_
    )
    df_outcomes_colnames <- colnames(.df_outcomes_)
    required_colnames <- c("Intervention", {{.label_effects_}}, {{.label_costs_}})
    missing_cols <- required_colnames[
      !required_colnames %in% df_outcomes_colnames
    ]
    missing_cols_label <- paste0(
      missing_cols,
      collapse = ", "
    )
    assertthat::assert_that(
      length(missing_cols) == 0,
      msg = paste0(
        "The dataframe passed to the argument .df_outcomes_ is miising the ",
        "columns [", missing_cols_label, "]."
      )
    )

    ## Check if missing key columns and create them if so:----
    df_cea_results <- add_missing_columns(
      .df_check_ = .df_outcomes_,
      .string_columns_ = c("dominance", "icer_diag"),
      .numerics_columns_ = c("delta.e", "delta.c", "icer")
    )

  } else if(all(!is.null(.df_effects_),
                !is.null(.df_costs_),
                !is.null(.interventions_labels_),
                !is.null(.label_effects_),
                !is.null(.label_costs_))) {

    ## Sanity checks:----
    check_icers_functions_inputs(
      .df_outcomes_ = NULL,
      .label_effects_ = .label_effects_,
      .label_costs_ = .label_costs_,
      .df_effects_ = .df_effects_,
      .df_costs_ = .df_costs_,
      .interventions_labels_ = .interventions_labels_,
      .wtp_key_values_ = .wtp_key_values_,
      .show_nmb_ = .show_nmb_,
      .show_pce_ = .show_pce_,
      .show_evpi_ = .show_evpi_,
      .show_dominance_ = .show_dominance_,
      .format_values_ = .format_values_,
      .currency_symbol_ = .currency_symbol_,
      .output_type_ = .output_type_,
      .output_format_ = .output_format_,
      .show_diag_ = .show_diag_
    )

    ## Define results table:----
    df_cea_results <- data.frame(
      'Intervention' = colnames(.df_effects_)
    )
    df_cea_results[[.label_effects_]] <- colMeans(.df_effects_) %>%
      unname()
    df_cea_results[[.label_costs_]] <- colMeans(.df_costs_) %>%
      unname()

    df_cea_results <- add_missing_columns(
      .df_check_ = df_cea_results,
      .string_columns_ = c("dominance", "icer_diag"),
      .numerics_columns_ = c("delta.e", "delta.c", "icer")
    )

  } else {
    stop(
      paste(
        "Please supply either the interventions' labels and their corresponding",
        "effects and costs from the PSA, each in a separate dataframe to the",
        "'.interventions_labels_', '.df_effects_' and '.df_costs_' argumnets,",
        "respectively; or a summary table with interventions' names, and",
        "corresponding mean costs and effects to the '.df_outcomes_' argument."
      )
    )
  }

  # Identify dominated interventions:----
  df_cea_results <- df_cea_results %>%
    identify_all_dominance() %>%
    calculate_icers() %>%
    identify_all_e_dominance() %>%
    `row.names<-`(NULL)

  # Add NMB:----
  if(all(isTRUE(.show_nmb_),
         !is.null(.wtp_key_values_),
         !is.null(.df_effects_),
         !is.null(.df_costs_))) {

    df_cea_results <- purrr::map(
      .x = .wtp_key_values_ %>%
        `names<-`(.wtp_key_values_),
      .f = function(wtp_value){
        values_name = paste0("NMB@", wtp_value)
        df_enmb <- colMeans(.df_effects_ * wtp_value - .df_costs_)
        df_enmb <-  if(isTRUE(.highlight_optimal_choices_)) {
          data.frame(
            "V1" = names(df_enmb[which.max(df_enmb)]),
            "V2" = df_enmb[which.max(df_enmb)] |>
              unname()
          ) |>
            `colnames<-`(c("Intervention", values_name))
        } else {
          data.frame(
            "V1" = names(df_enmb),
            "V2" = df_enmb |>
              unname()
          ) |>
            `colnames<-`(c("Intervention", values_name))
        }
      }
    ) %>%
      purrr::reduce(
        .x = .,
        .f = dplyr::full_join,
        by = "Intervention"
      ) %>%
      dplyr::left_join(
        x = df_cea_results,
        y = .,
        by = "Intervention"
      )
  }

  # Add probability cost-effective:----
  if(all(isTRUE(.show_pce_),
         !is.null(.wtp_key_values_),
         !is.null(.df_effects_),
         !is.null(.df_costs_))) {

    df_cea_results <- purrr::map(
      .x = .wtp_key_values_ %>%
        `names<-`(.wtp_key_values_),
      .f = function(wtp_value){
        values_name = paste0("PCE@", wtp_value)
        df_nmb <- .df_effects_ * wtp_value - .df_costs_
        df_enmb <- colMeans(df_nmb)
        df_ceaf <- colMeans(
          do.call(pmax, df_nmb) == df_nmb
        )
        df_ceaf <-  if(isTRUE(.highlight_optimal_choices_)) {
          data.frame(
            "V1" = names(df_ceaf[which.max(df_enmb)]),
            "V2" = df_ceaf[which.max(df_enmb)] |>
              unname()
          ) |>
            `colnames<-`(c("Intervention", values_name))
        } else {
          data.frame(
            "V1" = names(df_ceaf),
            "V2" = df_ceaf |>
              unname()
          ) |>
            `colnames<-`(c("Intervention", values_name))
        }
      }
    ) %>%
      purrr::reduce(
        .x = .,
        .f = dplyr::full_join,
        by = "Intervention"
      ) %>%
      dplyr::left_join(
        x = df_cea_results,
        y = .,
        by = "Intervention"
      )
  }

  # Add EVPI:----
  if(all(isTRUE(.show_evpi_),
         !is.null(.wtp_key_values_),
         !is.null(.df_effects_),
         !is.null(.df_costs_))) {

    df_cea_results <- purrr::map(
      .x = .wtp_key_values_ %>%
        `names<-`(.wtp_key_values_),
      .f = function(wtp_value) {
        values_name = paste0("EVPI@", wtp_value)
        # Calculate the NMB per PSA iteration:
        df_nmb <- .df_effects_ * wtp_value - .df_costs_
        df_enmb <- colMeans(df_nmb)
        # Get the highest NMB values per PSA iteration:
        max_nmb <- do.call(pmax, df_nmb)
        # Calculate the EVPI: expectation of max NMBs - the maximum expected NMB:
        evpi <- mean(max_nmb) - max(df_enmb)
        # Identify optimal choice:
        optimal_choice <- names(df_enmb[which.max(df_enmb)])
        data.frame(
          "V1" = names(df_enmb[which.max(df_enmb)]),
          "V2" = evpi |>
            unname()
        ) |>
          `colnames<-`(c("Intervention", values_name))
      }
    ) %>%
      purrr::reduce(
        .x = .,
        .f = dplyr::full_join,
        by = "Intervention"
      ) %>%
      dplyr::left_join(
        x = df_cea_results,
        y = .,
        by = "Intervention"
      )
  }

  # Drop dominance and diag columns:----
  dominance <- df_cea_results$dominance
  df_cea_results$dominance <- NULL
  if(!isTRUE(.show_diag_) | isTRUE(.output_type_ != "dataframe")) {
    df_cea_results$icer_diag <- NULL
  }

  # Format table:----
  nmb_col_index <- grep(
    pattern = "NMB@",
    x = colnames(df_cea_results),
    ignore.case = TRUE,
    value = FALSE
  )
  pce_col_index <- grep(
    pattern = "PCE@",
    x = colnames(df_cea_results),
    ignore.case = TRUE,
    value = FALSE
  )
  evpi_col_index <- grep(
    pattern = "EVPI@",
    x = colnames(df_cea_results),
    ignore.case = TRUE,
    value = FALSE
  )
  if(isTRUE(.format_values_)) {
    currency_labeled_columns <- colnames(df_cea_results)[
      colnames(df_cea_results) %in% c(.label_costs_, "delta.c", "icer",
                                      colnames(df_cea_results)[nmb_col_index],
                                      colnames(df_cea_results)[evpi_col_index])
    ]
    effects_columns <- colnames(df_cea_results)[
      colnames(df_cea_results) %in% c(.label_effects_, "delta.e")
    ]
    pce_columns <- colnames(df_cea_results)[pce_col_index]

    for (costs_col in currency_labeled_columns) {
      df_cea_results[!is.na(df_cea_results[[costs_col]]), costs_col] <- paste0(
        .currency_symbol_,
        format(
          x = df_cea_results[!is.na(df_cea_results[[costs_col]]), costs_col],
          digits = 1,
          big.mark = ",",
          trim = TRUE,
          scientific = FALSE
        )
      )
    }
    for (effs_col in effects_columns) {
      df_cea_results[!is.na(df_cea_results[[effs_col]]), effs_col] <- format(
        x = df_cea_results[!is.na(df_cea_results[[effs_col]]), effs_col],
        digits = 4,
        big.mark = ",",
        trim = TRUE,
        scientific = FALSE
      )
    }
    if(length(pce_columns) > 0) {
      for (prb_col in pce_columns) {
        df_cea_results[!is.na(df_cea_results[[prb_col]]), prb_col] <- format(
          x = df_cea_results[!is.na(df_cea_results[[prb_col]]), prb_col],
          digits = 2,
          trim = TRUE,
          scientific = FALSE
        )
      }
    }
  }

  # Apply interventions labels:----
  if(!is.null(.interventions_labels_)) {
    df_cea_results$Intervention <- .interventions_labels_[
      df_cea_results$Intervention
    ]
  }

  # Add dominance information to the ICER column:----
  if(isTRUE(.show_dominance_)) {
    df_cea_results$icer <- ifelse(
      test = !is.na(df_cea_results$icer),
      yes = df_cea_results$icer,
      no = dominance
    )
  }

  # Long format:----
  if(.output_format_ == "long") {
    df_cea_results <- df_cea_results %>%
      tidyr::pivot_longer(
        cols = - tidyr::all_of("Intervention"),
        names_to = " "
      ) %>%
      tidyr::pivot_wider(
        names_from =
          tidyr::all_of("Intervention"),
        values_from = tidyr::all_of("value")
      )
  }

  # Assign "icer_tbl" to the df_cea_results class:----
  class(df_cea_results) <- c(class(df_cea_results), "icer_tbl")

  # Generate required outputs:----
  if(.output_type_ != "dataframe") {
    gt_cea_results <- generate_gt_table(
      .df_cea_results_ = df_cea_results,
      .label_effects_ = .label_effects_,
      .label_costs_ = .label_costs_,
      .wtp_key_values_ = .wtp_key_values_,
      .currency_symbol_ = .currency_symbol_,
      .output_type_ = .output_type_,
      .output_format_ = .output_format_,
      .output_title_ = .output_title_,
      .output_subtitle_ = .output_subtitle_
    )

    return(gt_cea_results)
  }

  return(df_cea_results)
}

#' Estimate Relative Values
#'
#' @description
#' This function replaces the absolute Net Monetary Benefit (NMB), Probability
#' of Cost-Effectiveness (PCE), and Expected Value of Perfect Information (EVPI)
#' resulting post calibration with the relative values compared to the simulated
#' truth.
#'
#' @param .df_combined_cea_results_ Dataframe containing the outputs from the
#' `plot_cea_results_table` function. This dataframe is expected to contain the
#' cost-effectiveness results from one calibration method in addition to the
#' simulated truth. It is also expected that this dataframe includes at least
#' three columns, two of which are named `"Method"` and `" "`, whereas the other
#' columns are named after the interventions under analysis.
#' @param .interventions_names_ Character vector specifying the names of the
#' interventions under analysis. It is expected that the names of the
#' interventions exist as the column names in `.df_combined_cea_results_`.
#' @param .simulated_truth_label_ Character scalar specifying the label used
#' under the 'Method' column in `.df_combined_cea_results_` to indicate which
#' rows represent values from the simulated truth.
#' @param .measures_ Character vector that take any combination of `"NMB@"`,
#' `"PCE@"` or `"EVPI@"`.
#' @inheritParams plot_cea_results_table
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' compute_relative_cea_values(
#'  .df_combined_cea_results_ = df_PSA_combo_cea_tab_calibs,
#'  .interventions_names_ = .calibR_object_$model_interventions$v_interv_names,
#'  .simulated_truth_label_ = "Truth",
#'  .measures_ = "NMB@",
#'  .currency_symbol_ = "\u00A3"
#'  )
#' }
compute_relative_cea_values <- function(
    .df_combined_cea_results_,
    .interventions_names_,
    .simulated_truth_label_,
    .measures_ = "NMB@",
    .currency_symbol_ = "\u00A3") {
  ## Sanity checks:----
  assertthat::assert_that(
    !is.null(.df_combined_cea_results_[["Method"]]),
    msg = ".df_combined_cea_results_ is missing a 'Method' column."
  )
  assertthat::assert_that(
    !is.null(.df_combined_cea_results_[[" "]]),
    msg = ".df_combined_cea_results_ is missing a ' ' column."
  )
  assertthat::assert_that(
    assertthat::is.string(.simulated_truth_label_),
    msg = ".simulated_truth_label_ is not a character scalar."
  )
  assertthat::assert_that(
    .df_combined_cea_results_ %>%
      dplyr::filter(grepl(.simulated_truth_label_, .data[["Method"]])) %>%
      nrow() > 0,
    msg = paste0(
      ".df_combined_cea_results_ does not seem to have ",
      .simulated_truth_label_,
      " values in the 'Method' column.")
  )
  assertthat::assert_that(
    all(.interventions_names_ %in% colnames(.df_combined_cea_results_)),
    msg = paste0(
      "Values for ",
      paste0(
        .interventions_names_[
          !.interventions_names_ %in% colnames(.df_combined_cea_results_)
        ],
        collapse = ", "
      ),
      " are missing from .df_combined_cea_results_."
    )
  )
  assertthat::assert_that(
    all(.measures_ %in% c("NMB", "PCE", "EVPI", "NMB@", "PCE@", "EVPI@")),
    msg = ".measures_ can only take any combination of 'NMB', 'PCE' and 'EVPI'."
  )

  ## Ensure measures has @ prefix:----
  measures <- ifelse(
    test = grepl(
      pattern = "@",
      x = .measures_
    ),
    yes = .measures_,
    no = paste0(.measures_, "@")
  )

  ## Calculate relative values:----
  df_relative_values <- purrr:::map(
    .x = measures,
    .f = function(.measure_) {
      purrr::map(
        .x = .interventions_names_,
        .f = function(.interv_) {
          # loop over each intervention:
          .df_combined_cea_results_ %>%
            # filter Net Benefit to estimate relative values:
            dplyr::filter(
              !grepl(.simulated_truth_label_, .data[["Method"]]),
              grepl(.measure_, .data[[" "]])
            ) %>%
            # make sure relative values are estimated correctly per method:
            dplyr::group_by(
              .data[["Method"]]
            ) %>%
            # estimate absolute difference between calibrations and truth:
            dplyr::mutate(
              # truth and calibration results are estimated by intervention:
              dplyr::across(
                .cols = .interv_,
                .fns = function(.x) {
                  # remove currency and "," from values to compute differences:
                  x_ <- gsub(
                    pattern = .currency_symbol_,
                    replacement = "",
                    x = .x
                  )
                  x_ <- gsub(
                    pattern = ",",
                    replacement = "",
                    x = x_
                  )
                  # convert characters/strings to numeric:
                  x_ <- as.numeric(x_)
                  # next, grab simulated truth values for same intervention:
                  y_ <- {
                    .df_combined_cea_results_ %>%
                      # filter Net Benefit to estimate relative values:
                      dplyr::filter(
                        grepl(.simulated_truth_label_, .data[["Method"]]),
                        grepl(.measure_, .data[[" "]])
                      ) %>%
                      {
                        # copy EVPI to all cells:
                        if(.measure_ == "EVPI@"){
                          df_evpi <- .
                          # identify the row(s) with EVPI values:
                          evpi_rows <- grep(
                            pattern = "EVPI@",
                            x = .df_combined_cea_results_ %>%
                              # filter Net Benefit to estimate relative values:
                              dplyr::filter(
                                grepl(.simulated_truth_label_, .data[["Method"]]),
                                grepl(.measure_, .data[[" "]])
                              ) %>%
                              purrr::pluck(1)
                          )
                          # assign all EVPI row to the identified value:
                          for (evpi_row in 1:length(evpi_rows)) {
                            # identify the column with EVPI values:
                            evpi_column <- which(
                              !is.na(
                                df_evpi[
                                  evpi_row,
                                  2:(length(.interventions_names_) + 1)
                                ]
                              )
                            )
                            # identify the EVPI values:
                            evpi_value <- df_evpi[
                              evpi_row,
                              evpi_column + 1
                            ] %>%
                              unlist() %>%
                              unname()
                            # copy EVPI values across the intervention rows:
                            df_evpi[
                              evpi_row,
                              2:(length(.interventions_names_) + 1)
                            ] <- evpi_value
                          }
                          df_evpi
                        } else {
                          .
                        }
                      } %>%
                      # mutate the values of the same intervention above:
                      dplyr::mutate(
                        dplyr::across(
                          .cols = .interv_,
                          .fns = function(.x) {
                            # remove the currency and "," to run calculations:
                            x_ = gsub(
                              pattern = .currency_symbol_,
                              replacement = "",
                              x = .x
                            )
                            x_ = gsub(
                              pattern = ",",
                              replacement = "",
                              x = x_
                            )
                            # convert the values to numeric for calculations:
                            x_ = as.numeric(x_)
                          }
                        )
                      ) %>%
                      # extract the values to estimate abs difference from truth:
                      dplyr::pull(
                        .data[[.interv_]]
                      )
                  }
                  # estimate absolute difference:
                  x_ = abs(x_ - y_)
                  x_
                }
              )
            ) %>%
            # remove grouping now that abs differences were obtained:
            dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                .cols = .interv_,
                .fns = function(.x) {
                  # re-apply the earlier formatting:
                  if(.measure_ %in% c("NMB@", "EVPI@")) {
                    scales::dollar(
                      x = .x,
                      prefix = .currency_symbol_
                    )
                  } else {
                    round(
                      x = .x,
                      digits = 2
                    ) %>%
                      as.character()
                  }
                }
              )
            ) %>%
            # If this was the first intervention in intervention's list:
            {if (.interv_ == .interventions_names_[1]) {
              # append the auxiliary columns from original data table:
              dplyr::select(
                .data = .,
                colnames(.df_combined_cea_results_)[
                  which(
                    !colnames(.df_combined_cea_results_) %in%
                      names(.interventions_names_)[
                        which(
                          names(.interventions_names_) != .interv_
                        )
                      ]
                  )
                ]
              )
            } else {
              dplyr::select(
                .data = .,
                dplyr::all_of(.interv_)
              )
            }}
        }
      ) %>%
        unname() %>%
        purrr::list_cbind()
    }
  ) %>%
    unname() %>%
    purrr::list_rbind() %>%
    # bind the rows of the other reported results:
    dplyr::bind_rows(
      .df_combined_cea_results_ %>%
        # filter Net Benefit to estimate relative values:
        dplyr::filter(
          !grepl(.simulated_truth_label_, .data[["Method"]]),
          !grepl(paste0(
            measures,
            collapse = "|"
          ),
          .data[[" "]]
          )
        ),
      .
    ) %>%
    # ensure data is ranked as needed:
    dplyr::mutate(
      "Ranking_measures" = dplyr::case_when(
        grepl("NMB@", .data[[" "]]) ~ 1,
        grepl("PCE@", .data[[" "]]) ~ 2,
        grepl("EVPI@", .data[[" "]]) ~ 3,
        TRUE ~ NA_real_
      ),
      "Ranking_methods" = dplyr::case_when(
        .data[["Method"]] == "Truth" ~ 0,
        .data[["Method"]] %in% c(
          "MCMC", "SIR", "IMIS"
        ) ~ 1,
        .data[["Method"]] %in% c(
          "FGS (LLK):", "FGS (SSE):", "RGS (LLK):", "RGS (SSE):",
          "LHS (LLK):", "LHS (SSE):"
        ) ~ 2,
        .data[["Method"]] %in% c(
          "GRG (LLK):", "GRG (SSE):", "GRG (LLK - unconverged):",
          "GRG (SSE - unconverged):", "NM (LLK):", "NM (SSE):",
          "NM (LLK - unconverged):", "NM (SSE - unconverged):", "SANN (LLK):",
          "SANN (SSE):"
        ) ~ 3
      )
    ) %>%
    dplyr::arrange(
      .data[["Ranking_methods"]], .data[["Method"]], .data[["Ranking_measures"]]
    ) %>%
    dplyr::select(
      -dplyr::all_of(c("Ranking_methods", "Ranking_measures"))
    ) %>%
    # bind the simulated truth results to the top of the table:
    dplyr::bind_rows(
      .df_combined_cea_results_ %>%
        # filter Net Benefit to estimate relative values:
        dplyr::filter(
          grepl(.simulated_truth_label_, .data[["Method"]])
        ),
      .
    )

  return(df_relative_values)
}

#' Generate Cost-Effectiveness Analysis Results Tables
#'
#' @description
#' Draw Cost-Effectiveness Analysis Results Tables from Probabilistic
#' Sensitivity Analysis (PSA) outputs. These PSA outputs are expected to be post
#' one or more calibration processes and possibly the simulated truth.
#'
#' @param .ls_calibration_results_ List containing the PSA of the calibration
#' results.
#' @param .ls_PSA_results_ List containing the costs and effects PSA outputs.
#' @param .model_interventions_ Character Vector specifying the names of the
#' interventions associated with the decision problem and outputs of the model.
#' @param .model_outcomes_ Character vector specifying the labels of the
#' outcomes produced by the model, usually "costs" and "effects" or "QALYs".
#' @param .label_effects_ Character indicating the name of the column containing
#' the mean values of the effects. Default is `"QALYs"`.
#' @param .label_costs_ Character indicating the name of the column containing
#' the mean values of the costs. Default is `"Costs"`.
#' @param .wtp_key_values_ Numeric vector specifying the willingness-to-pay
#' (WTP) values to be used in estimating the NMB and probability of the optimal
#' choice being cost-effective. Default is `c(20000, 30000)`.
#' @param .highlight_optimal_choices_ Logical for whether to report NMB, PCE or
#' EVPI for the optimal choice. Default is `TRUE`. If `FALSE`, the values of
#' NMB, PCE or EVPI will be reported for all options.
#' @param .currency_symbol_ Character scalar representing the Hex code of the
#' currency symbol to use in labeling relevant results. Default is `"\u00A3"`
#' for Sterling (GBP). Use `"\u20AC"`for Euros or `"\u0024"` for US Dollars.
#' @param .output_type_ Character scalar that takes one of three options:
#' `"html"`, `"latex"` or `"dataframe"`. Default is `"dataframe"`. If `"html"`
#' or `"latex"` were passed to this argument, the function calls
#' \link{generate_gt_table} internally to generate the required output. The
#' `"html"` is suitable for shiny application; whereas, `"latex"`is suitable for
#' PDF document.
#' @param .full_output_format_ Character scalar taking one of two options:
#' `"wide"` or `"long"` specifying whether the generated results table would be
#' in a wide or long format, respectively. Default is `"long"`.
#' @param .add_simulated_truth_ Logical scalar for whether to add results from
#' the Simulated Truth PSA data to the generated results tables. Default is
#' `TRUE`.
#' @param .truth_PSA_output_list_path_ Character scalar specifying the path
#' where the list containing the Simulated Truth PSA data. The path is expected
#' to lead to a `.RDS` file representing the list containing the PSA data. This
#' list is expected to contain two dataframes named "e" or "effects" and "c" or
#' "costs", containing the effects and costs PSA data, respectively; and a
#' character vector containing the names of the interventions. This argument is
#' ignored if `.add_simulated_truth_` is set to `FALSE`.
#' @param .truth_PSA_output_list_ List containing the Simulated Truth PSA data.
#' This list is expected to contain two dataframes named "e" or "effects" and
#' "c" or "costs", containing the effects and costs PSA data, respectively; and
#' a character vector containing the names of the interventions. This argument
#' is ignored if `.add_simulated_truth_` is set to `FALSE` or if
#' `.truth_PSA_output_list_path_` is set to `NULL`.
#' @param .generate_partial_cea_table_ Logical for whether to generate CEA
#' tables with fewer results. Default is `TRUE`.
#' @param .partial_cea_table_groups_ Character vector specifying what CEA
#' measures to report on the tables with fewer results. Default is
#' `c("NMB", "PCE", "EVPI")`, but it can take any or all of these values.
#' @param .generate_relative_values_ Logical for whether the produced CEA
#' results table should be relative to the Simulated Truth or not. Default is
#' `TRUE`.
#' @param .relative_values_data_ Character vector specifying what CEA measures
#' to report on the tables with fewer results. Default is `c("NMB")`, but it can
#' take any or all of`c("NMB", "PCE", "EVPI")`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' PSA_summary_tables1 <- generate_cea_results_tables(
#'    .ls_calibration_results_ = CR_CRS_2P1T$calibration_results,
#'    .ls_PSA_results_ = CR_CRS_2P1T$PSA_results,
#'    .model_interventions_ = CR_CRS_2P1T$model_interventions$v_interv_names,
#'    .model_outcomes_ = CR_CRS_2P1T$model_interventions$v_interv_outcomes,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = c(20000, 30000),
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .full_output_format_ = "long",
#'    .add_simulated_truth_ = TRUE,
#'    .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
#'    .truth_PSA_output_list_ = NULL,
#'    .generate_partial_cea_table_ = TRUE,
#'    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
#'    .generate_relative_values_ = TRUE,
#'    .relative_values_data_ = c("NMB")
#' )
#' PSA_summary_tables2 <- generate_cea_results_tables(
#'    .ls_calibration_results_ = CR_CRS_2P1T$calibration_results,
#'    .ls_PSA_results_ = CR_CRS_2P1T$PSA_results,
#'    .model_interventions_ = CR_CRS_2P1T$model_interventions$v_interv_names,
#'    .model_outcomes_ = CR_CRS_2P1T$model_interventions$v_interv_outcomes,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = c(20000, 30000),
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .full_output_format_ = "long",
#'    .add_simulated_truth_ = TRUE,
#'    .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
#'    .truth_PSA_output_list_ = NULL,
#'    .generate_partial_cea_table_ = TRUE,
#'    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
#'    .generate_relative_values_ = TRUE,
#'    .relative_values_data_ = c("NMB", "PCE")
#' )
#' PSA_summary_tables3 <- generate_cea_results_tables(
#'    .ls_calibration_results_ = CR_CRS_2P1T$calibration_results,
#'    .ls_PSA_results_ = CR_CRS_2P1T$PSA_results,
#'    .model_interventions_ = CR_CRS_2P1T$model_interventions$v_interv_names,
#'    .model_outcomes_ = CR_CRS_2P1T$model_interventions$v_interv_outcomes,
#'    .label_effects_ = "QALYs",
#'    .label_costs_ = "Costs",
#'    .wtp_key_values_ = c(20000, 30000),
#'    .currency_symbol_ = "\u00A3",
#'    .output_type_ = "html",
#'    .full_output_format_ = "long",
#'    .add_simulated_truth_ = TRUE,
#'    .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
#'    .truth_PSA_output_list_ = NULL,
#'    .generate_partial_cea_table_ = TRUE,
#'    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
#'    .generate_relative_values_ = TRUE,
#'    .relative_values_data_ = c("NMB", "PCE", "EVPI")
#' )
#' }
generate_cea_results_tables = function(
    .ls_calibration_results_,
    .ls_PSA_results_,
    .model_interventions_,
    .model_outcomes_,
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .wtp_key_values_ = c(20000, 30000),
    .highlight_optimal_choices_ = FALSE,
    .currency_symbol_ = "\u00A3",
    .output_type_ = "html",
    .full_output_format_ = "long",
    .add_simulated_truth_ = TRUE,
    .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
    .truth_PSA_output_list_ = NULL,
    .generate_partial_cea_table_ = TRUE,
    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
    .generate_relative_values_ = TRUE,
    .relative_values_data_ = c("NMB")) {
  ## Sanity checks:----
  if(isTRUE(.add_simulated_truth_)) {
    if(!is.null(.truth_PSA_output_list_path_)) {
      assertthat::assert_that(
        assertthat::is.string(.truth_PSA_output_list_path_),
        msg = paste(
          ".truth_PSA_output_list_path_ is not a character string."
        )
      )
      assertthat::assert_that(
        assertthat::is.readable(.truth_PSA_output_list_path_),
        msg = paste0(
          "PSA output (list) does not exist in the path '",
          .truth_PSA_output_list_path_,
          "'."
        )
      )
    }
    ### Read or assign Simulated Truth PSA data:----
    truth_PSA_output_list <- if(!is.null(.truth_PSA_output_list_path_)) {
      readRDS(
        file = .truth_PSA_output_list_path_
      )
    } else if(!is.null(.truth_PSA_output_list_)) {
      .truth_PSA_output_list_
    } else {
      stop(
        paste0(
          "Please either pass .truth_PSA_output_list_path_ or ",
          ".truth_PSA_output_list_ to generate relative results tables."
        )
      )
    }
    assertthat::assert_that(
      is.list(truth_PSA_output_list),
      msg = paste0(
        "truth_PSA_output_list is not a list."
      )
    )
    assertthat::assert_that(
      all(
        any(!is.null(truth_PSA_output_list[["effects"]]),
            !is.null(truth_PSA_output_list[["e"]])),
        any(!is.null(truth_PSA_output_list[["costs"]]),
            !is.null(truth_PSA_output_list[["c"]])),
        any(!is.null(truth_PSA_output_list[["Interventions"]]),
            !is.null(truth_PSA_output_list[["treats"]]))
      ),
      msg = paste0(
        "Simulated Truth list should contain the effects and costs PSA ouputs ",
        "in addition to the decision problem interventions. These dataframes ",
        "and string vector should be named `effects` or `e`, `costs` or `c` ",
        "and `Interventions` or `treats`, respectively."
      )
    )
    assertthat::assert_that(
      all(
        any(if(!is.null(truth_PSA_output_list[["effects"]])) {
          "data.frame" %in% class(truth_PSA_output_list[["effects"]])
        },
        if(!is.null(truth_PSA_output_list[["e"]])) {
          "data.frame" %in% class(truth_PSA_output_list[["e"]])
        }),
        any(if(!is.null(truth_PSA_output_list[["costs"]])) {
          "data.frame" %in% class(truth_PSA_output_list[["costs"]])
        },
        if(!is.null(truth_PSA_output_list[["c"]])) {
          "data.frame" %in% class(truth_PSA_output_list[["c"]])
        }),
        any(if(!is.null(truth_PSA_output_list[["Interventions"]])) {
          is.character(truth_PSA_output_list[["Interventions"]])
        },
        if(!is.null(truth_PSA_output_list[["treats"]])) {
          is.character(truth_PSA_output_list[["treats"]])
        })
      ),
      msg = paste0(
        "Simulated Truth list should contain dataframes for the effects and ",
        "costs PSA ouputs in addition a vector of the decision problem ",
        "interventions."
      )
    )
  }
  if(isTRUE(.generate_partial_cea_table_)) {
    assertthat::assert_that(
      is.character(.partial_cea_table_groups_),
      msg = ".partial_cea_table_groups_ is not a character vector."
    )
    assertthat::assert_that(
      all(.partial_cea_table_groups_ %in% c("NMB", "PCE", "EVPI")),
      msg = ".partial_cea_table_groups_ can take any of 'NMB', 'PCE' and 'EVPI'."
    )
    if(isTRUE(.generate_relative_values_)) {
      assertthat::assert_that(
        is.character(.relative_values_data_),
        msg = ".relative_values_data_ is not a character vector."
      )
      assertthat::assert_that(
        all(.relative_values_data_ %in% .partial_cea_table_groups_),
        msg = paste(
          "Some of the required relative values groups or measures passed to the",
          "argument '.relative_values_data_'  are not in the groups or measures",
          "requested in the partial tables, and passed to the argument",
          "'.partial_cea_table_groups_'."
        )
      )
    }
  }

  ## Create outputs list:----
  ls_outputs <- NULL

  ## Extract PSA results per outcome (costs/QALYs) for post processing:----
  ls_PSA_costs_effects <- purrr::map(
    .x = .ls_PSA_results_,
    .f = function(.res_) {
      # loop through PSA results of each calibration method:
      c(
        purrr::map(
          .x = .model_outcomes_,
          .f = function(.outcome_) {
            # loop through PSA outcomes (costs and effects) to use in PSA:
            conseq_df <- .res_ %>%
              # select all costs or effects columns:
              dplyr::select(dplyr::contains(.outcome_)) %>%
              # remove outcome portion in the column name:
              dplyr::rename_with(.fn = function(.x) {
                stringr::str_remove(
                  string = .x,
                  pattern = paste0(".", .outcome_))},
                .cols = dplyr::everything())
          }
        ),
        "Interventions" = list(
          .model_interventions_
        )
      )
    }
  )

  ## Generate summary tables from the PSA Results:----
  ### Full tables:----
  #### Calibration methods tables:----
  # .calibR_object_$PSA_summary_tables[["Full"]]
  ls_outputs[["Full"]] <- purrr::map(
    # to group PSA summary objects by calibration methods category:
    .x = .ls_calibration_results_ %>%
      names(.) %>%
      `names<-`(., .),
    .f = function(.calib_category_) {
      purrr::map(
        # grab the methods names from the calibration results outputs:
        .x = .ls_calibration_results_[[.calib_category_]] %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_method) {
          # Loop through each calibration method:
          plot_cea_results_table(
            .df_effects_ = ls_PSA_costs_effects[[.calib_method]][["effects"]],
            .df_costs_ = ls_PSA_costs_effects[[.calib_method]][["costs"]],
            .interventions_labels_ = ls_PSA_costs_effects[[.calib_method]][["Interventions"]] %>%
              `names<-`(colnames(ls_PSA_costs_effects[[.calib_method]][["effects"]])),
            .highlight_optimal_choices_ = .highlight_optimal_choices_,
            .wtp_key_values_ = .wtp_key_values_,
            .currency_symbol_ = .currency_symbol_,
            .output_type_ = .output_type_,
            .output_format_ = .full_output_format_,
            .output_title_ = paste(
              "Calibration methods PSA results -", .calib_method
            ),
            .output_subtitle_ = "The table shows cost-effectiveness results."
          )
        }
      )
    }
  )

  #### Simulated truth table:----
  if(isTRUE(.add_simulated_truth_)) {
    ls_outputs$Full[["True"]] <- plot_cea_results_table(
      .df_effects_ = truth_PSA_output_list[["e"]],
      .df_costs_ = truth_PSA_output_list[["c"]],
      .interventions_labels_ = truth_PSA_output_list[["treats"]] %>%
        `names<-`(colnames(truth_PSA_output_list[["e"]])),
      .highlight_optimal_choices_ = .highlight_optimal_choices_,
      .wtp_key_values_ = .wtp_key_values_,
      .currency_symbol_ = .currency_symbol_,
      .output_type_ = .output_type_,
      .output_format_ = .full_output_format_,
      .output_title_ = "Cost-Effectiveness Analysis Results - Simulated Truth",
      .output_subtitle_ = "The table shows all cost-effectiveness results."
    )
  }

  ### Partial tables:----
  if(isTRUE(.generate_partial_cea_table_)) {
    #### Calibration methods tables:----
    ls_outputs[["Partials"]] <- purrr::map(
      # to group PSA summary objects by calibration methods category
      .x = .ls_calibration_results_ %>%
        names(.) %>%
        `names<-`(., .),
      .f = function(.calib_category_) {
        purrr::map(
          # grab the methods names from the calibration results outputs:
          .x = .ls_calibration_results_[[.calib_category_]] %>%
            names(.) %>%
            `names<-`(., .),
          .f = function(.calib_method) {
            ####### Loop through each calibration method:----
            plot_cea_results_table(
              .df_effects_ = ls_PSA_costs_effects[[.calib_method]][["effects"]],
              .df_costs_ = ls_PSA_costs_effects[[.calib_method]][["costs"]],
              .interventions_labels_ = ls_PSA_costs_effects[[.calib_method]][["Interventions"]] %>%
                `names<-`(colnames(ls_PSA_costs_effects[[.calib_method]][["effects"]])),
              .wtp_key_values_ = .wtp_key_values_,
              .highlight_optimal_choices_ = .highlight_optimal_choices_,
              .currency_symbol_ = .currency_symbol_,
              .output_type_ = "dataframe",
              .output_format_ = .full_output_format_
            ) %>% generate_gt_table(
              .label_effects_ = .label_effects_,
              .label_costs_ = .label_costs_,
              .wtp_key_values_ = .wtp_key_values_,
              .currency_symbol_ = .currency_symbol_,
              .output_type_ = .output_type_,
              .output_format_ = .full_output_format_,
              .output_title_ = paste(
                "Cost-Effectiveness Analysis Results -", .calib_method
              ),
              .output_subtitle_ = paste(
                "The table shows all cost-effectiveness results."
              ),
              .output_contents_ = .partial_cea_table_groups_
            )
          }
        )
      }
    )

    #### Simulated truth table:----
    if(isTRUE(.add_simulated_truth_)) {
      ls_outputs$Partials[["True"]] <- plot_cea_results_table(
        .df_effects_ = truth_PSA_output_list[["e"]],
        .df_costs_ = truth_PSA_output_list[["c"]],
        .interventions_labels_ = truth_PSA_output_list[["treats"]] %>%
          `names<-`(colnames(truth_PSA_output_list[["e"]])),
        .wtp_key_values_ = .wtp_key_values_,
        .highlight_optimal_choices_ = .highlight_optimal_choices_,
        .currency_symbol_ = .currency_symbol_,
        .output_type_ = "dataframe",
        .output_format_ = .full_output_format_
      ) %>% generate_gt_table(
        .label_effects_ = .label_effects_,
        .label_costs_ = .label_costs_,
        .wtp_key_values_ = .wtp_key_values_,
        .currency_symbol_ = .currency_symbol_,
        .output_type_ = .output_type_,
        .output_format_ = .full_output_format_,
        .output_title_ = "Simulated truth PSA results",
        .output_subtitle_ = "The table shows a subset of the generated results.",
        .output_contents_ = .partial_cea_table_groups_
      )
    }
  }
  ### Partial Combined table:----
  if(isTRUE(.generate_partial_cea_table_)) {
    #### Combined partial calibration methods table:----
    df_PSA_combo_cea_tab_calibs <- c(
      if(isTRUE(.add_simulated_truth_)) {
        "True" = list(
          plot_cea_results_table(
            .df_effects_ = truth_PSA_output_list[["e"]],
            .df_costs_ = truth_PSA_output_list[["c"]],
            .interventions_labels_ = truth_PSA_output_list[["treats"]] %>%
              `names<-`(colnames(truth_PSA_output_list[["e"]])),
            .wtp_key_values_ = .wtp_key_values_,
            .highlight_optimal_choices_ = .highlight_optimal_choices_,
            .currency_symbol_ = .currency_symbol_,
            .output_type_ = "dataframe",
            .output_format_ = .full_output_format_
          ) %>%
            `class<-`("data.frame") %>%
            dplyr::mutate(
              "Method" = "Truth"
            )
        )
      } else {
        NULL
      },
      "Calibration Results" = purrr::map(
        # to group PSA summary objects by calibration methods category
        .x = .ls_calibration_results_ %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map(
            # grab the methods names from the calibration results outputs:
            .x = .ls_calibration_results_[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method) {
              # loop through each calibration method:
              plot_cea_results_table(
                .df_effects_ = ls_PSA_costs_effects[[.calib_method]][["effects"]],
                .df_costs_ = ls_PSA_costs_effects[[.calib_method]][["costs"]],
                .interventions_labels_ = ls_PSA_costs_effects[[.calib_method]][["Interventions"]] %>%
                  `names<-`(colnames(ls_PSA_costs_effects[[.calib_method]][["effects"]])),
                .wtp_key_values_ = .wtp_key_values_,
                .highlight_optimal_choices_ = .highlight_optimal_choices_,
                .currency_symbol_ = .currency_symbol_,
                .output_type_ = "dataframe",
                .output_format_ = .full_output_format_
              ) %>%
                `class<-`("data.frame") %>%
                dplyr::mutate(
                  "Method" = .calib_method
                )
            }
          ) %>%
            purrr::list_rbind()
        }
      )
    ) %>%
      purrr::list_rbind() %>%
      # apply clearer calibration methods' names:
      dplyr::mutate(
        "Method" = dplyr::case_when(
          Method %in% c("log_likelihood_RGS", "LLK_RGS") ~ "RGS (LLK):",
          Method %in% c("wSumSquareError_RGS", "SSE_RGS") ~ "RGS (SSE):",
          Method %in% c("log_likelihood_FGS", "LLK_FGS") ~ "FGS (LLK):",
          Method %in% c("wSumSquareError_FGS", "SSE_FGS") ~ "FGS (SSE):",
          Method %in% c("log_likelihood_LHS", "LLK_LHS") ~ "LHS (LLK):",
          Method %in% c("wSumSquareError_LHS", "SSE_LHS") ~ "LHS (SSE):",
          Method %in% c("NM_LLK_0", "NM_LLK_RGS", "NM_LLK_FGS", "NM_LLK_LHS")
          ~ "NM (LLK):",
          Method %in% c("NM_SSE_0", "NM_SSE_RGS", "NM_SSE_FGS", "NM_SSE_LHS")
          ~ "NM (SSE):",
          Method %in% c("NM_LLK_1") ~ "NM (LLK - unconverged):",
          Method %in% c("NM_SSE_1") ~ "NM (SSE - unconverged):",
          Method %in% c("BFGS_LLK_0", "BFGS_LLK_RGS", "BFGS_LLK_FGS", "BFGS_LLK_LHS")
          ~ "GRG (LLK):",
          Method %in% c("BFGS_SSE_0", "BFGS_SSE_RGS", "BFGS_SSE_FGS", "BFGS_SSE_LHS")
          ~ "GRG (SSE):",
          Method %in% c("BFGS_LLK_1") ~ "GRG (LLK - unconverged):",
          Method %in% c("BFGS_SSE_1") ~ "GRG (SSE - unconverged):",
          Method %in% c("SANN_LLK_", "SANN_LLK_RGS", "SANN_LLK_FGS", "SANN_LLK_LHS")
          ~ "SANN (LLK):",
          Method %in% c("SANN_SSE_", "SANN_SSE_RGS", "SANN_SSE_FGS", "SANN_SSE_LHS")
          ~ "SANN (SSE):",
          TRUE ~ Method
        )
      ) %>%
      dplyr::mutate(
        "Ranking" = dplyr::case_when(
          .data[["Method"]] == "Truth" ~ 0,
          .data[["Method"]] %in% c(
            "MCMC", "SIR", "IMIS"
          ) ~ 1,
          .data[["Method"]] %in% c(
            "FGS (LLK):", "FGS (SSE):", "RGS (LLK):", "RGS (SSE):",
            "LHS (LLK):", "LHS (SSE):"
          ) ~ 2,
          .data[["Method"]] %in% c(
            "GRG (LLK):", "GRG (SSE):", "GRG (LLK - unconverged):",
            "GRG (SSE - unconverged):", "NM (LLK):", "NM (SSE):",
            "NM (LLK - unconverged):", "NM (SSE - unconverged):", "SANN (LLK):",
            "SANN (SSE):"
          ) ~ 3
        )
      ) %>%
      dplyr::arrange(
        .data[["Ranking"]], .data[["Method"]]
      ) %>%
      dplyr::select(
        -dplyr::all_of("Ranking")
      )

    ##### Beautified tables:----
    ###### Absolute values:----
    ls_outputs[["Combined"]][["Absolute"]] <- df_PSA_combo_cea_tab_calibs %>%
      `class<-`(c(class(.), "icer_tbl")) %>%
      generate_gt_table(
        .label_effects_ = .label_effects_,
        .label_costs_ = .label_costs_,
        .wtp_key_values_ = .wtp_key_values_,
        .currency_symbol_ = .currency_symbol_,
        .output_type_ = .output_type_,
        .output_format_ = "long",
        .output_title_ = paste(
          "Cost-Effectiveness Analysis Results - Truth vs Calibrated"
        ),
        .output_subtitle_ = paste(
          "The table shows a subset of the cost-effectiveness results."
        ),
        .output_contents_ = .partial_cea_table_groups_,
        .output_grouped_ = TRUE,
        .output_group_ = "Method"
      )

    ###### Relative values:----
    if(all(
      isTRUE(.add_simulated_truth_),
      isTRUE(.generate_relative_values_)
    )) {
      ####### Prepare relative values table:----
      df_PSA_combo_cea_tab_calibs_rel <- compute_relative_cea_values(
        .df_combined_cea_results_ = df_PSA_combo_cea_tab_calibs,
        .interventions_names_ = .model_interventions_,
        .simulated_truth_label_ = "Truth",
        .measures_ = .relative_values_data_,
        .currency_symbol_ = .currency_symbol_
      )

      ####### Generate beautified relative table:----
      ls_outputs[["Combined"]][["Relative"]] <- df_PSA_combo_cea_tab_calibs_rel %>%
        `class<-`(c(class(.), "icer_tbl")) %>%
        generate_gt_table(
          .label_effects_ = .label_effects_,
          .label_costs_ = .label_costs_,
          .wtp_key_values_ = .wtp_key_values_,
          .currency_symbol_ = .currency_symbol_,
          .output_type_ = .output_type_,
          .output_format_ = "long",
          .output_title_ = paste(
            "Cost-Effectiveness Analysis Results - Truth vs Calibrated"
          ),
          .output_subtitle_ = paste0(
            "The table shows a subset of the cost-effectiveness results, ",
            "including the **",
            paste0(
              .relative_values_data_[-length(.relative_values_data_)],
              collapse = "**, **"
            ),
            paste0(
              if(length(.relative_values_data_) > 1) "** and **" else "",
              .relative_values_data_[length(.relative_values_data_)]
            ),
            "** values from different callibration methods **relative to the ",
            "simulated truth**."
          ),
          .output_contents_ = .partial_cea_table_groups_,
          .output_grouped_ = TRUE,
          .output_group_ = "Method"
        )
    }
    #### Combined partial calibration class table:----
    ls_PSA_combo_cea_tab_calibs_grp <- purrr::map(
      # to group PSA summary objects by calibration methods category
      .x = .ls_calibration_results_ %>%
        names(.) %>%
        `names<-`(., .),
      .f = function(.calib_category_) {
        c(
          "True" = list(
            plot_cea_results_table(
              .df_effects_ = truth_PSA_output_list[["e"]],
              .df_costs_ = truth_PSA_output_list[["c"]],
              .interventions_labels_ = truth_PSA_output_list[["treats"]] %>%
                `names<-`(colnames(truth_PSA_output_list[["e"]])),
              .wtp_key_values_ = .wtp_key_values_,
              .highlight_optimal_choices_ = .highlight_optimal_choices_,
              .currency_symbol_ = .currency_symbol_,
              .output_type_ = "dataframe",
              .output_format_ = .full_output_format_
            ) %>%
              `class<-`("data.frame") %>%
              dplyr::mutate(
                "Method" = "Truth"
              )
          ),
          "Calibration Results" = purrr::map(
            # grab the methods names from the calibration results outputs:
            .x = .ls_calibration_results_[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method) {
              # loop through each calibration method:
              plot_cea_results_table(
                .df_effects_ = ls_PSA_costs_effects[[.calib_method]][["effects"]],
                .df_costs_ = ls_PSA_costs_effects[[.calib_method]][["costs"]],
                .interventions_labels_ = ls_PSA_costs_effects[[.calib_method]][["Interventions"]] %>%
                  `names<-`(colnames(ls_PSA_costs_effects[[.calib_method]][["effects"]])),
                .wtp_key_values_ = .wtp_key_values_,
                .highlight_optimal_choices_ = .highlight_optimal_choices_,
                .currency_symbol_ = .currency_symbol_,
                .output_type_ = "dataframe",
                .output_format_ = .full_output_format_
              ) %>%
                `class<-`("data.frame") %>%
                dplyr::mutate(
                  "Method" = .calib_method
                )
            }
          )
        ) %>%
          purrr::list_rbind() %>%
          # apply clearer calibration methods' names:
          dplyr::mutate(
            "Method" = dplyr::case_when(
              Method %in% c("log_likelihood_RGS", "LLK_RGS") ~ "RGS (LLK):",
              Method %in% c("wSumSquareError_RGS", "SSE_RGS") ~ "RGS (SSE):",
              Method %in% c("log_likelihood_FGS", "LLK_FGS") ~ "FGS (LLK):",
              Method %in% c("wSumSquareError_FGS", "SSE_FGS") ~ "FGS (SSE):",
              Method %in% c("log_likelihood_LHS", "LLK_LHS") ~ "LHS (LLK):",
              Method %in% c("wSumSquareError_LHS", "SSE_LHS") ~ "LHS (SSE):",
              Method %in% c("NM_LLK_0", "NM_LLK_RGS", "NM_LLK_FGS", "NM_LLK_LHS")
              ~ "NM (LLK):",
              Method %in% c("NM_SSE_0", "NM_SSE_RGS", "NM_SSE_FGS", "NM_SSE_LHS")
              ~ "NM (SSE):",
              Method %in% c("NM_LLK_1") ~ "NM (LLK - unconverged):",
              Method %in% c("NM_SSE_1") ~ "NM (SSE - unconverged):",
              Method %in% c("BFGS_LLK_0", "BFGS_LLK_RGS", "BFGS_LLK_FGS", "BFGS_LLK_LHS")
              ~ "GRG (LLK):",
              Method %in% c("BFGS_SSE_0", "BFGS_SSE_RGS", "BFGS_SSE_FGS", "BFGS_SSE_LHS")
              ~ "GRG (SSE):",
              Method %in% c("BFGS_LLK_1") ~ "GRG (LLK - unconverged):",
              Method %in% c("BFGS_SSE_1") ~ "GRG (SSE - unconverged):",
              Method %in% c("SANN_LLK_", "SANN_LLK_RGS", "SANN_LLK_FGS", "SANN_LLK_LHS")
              ~ "SANN (LLK):",
              Method %in% c("SANN_SSE_", "SANN_SSE_RGS", "SANN_SSE_FGS", "SANN_SSE_LHS")
              ~ "SANN (SSE):",
              TRUE ~ Method
            )
          ) %>%
          dplyr::mutate(
            "Ranking" = dplyr::case_when(
              .data[["Method"]] == "Truth" ~ 0,
              .data[["Method"]] %in% c(
                "MCMC", "SIR", "IMIS"
              ) ~ 1,
              .data[["Method"]] %in% c(
                "FGS (LLK):", "FGS (SSE):", "RGS (LLK):", "RGS (SSE):",
                "LHS (LLK):", "LHS (SSE):"
              ) ~ 2,
              .data[["Method"]] %in% c(
                "GRG (LLK):", "GRG (SSE):", "GRG (LLK - unconverged):",
                "GRG (SSE - unconverged):", "NM (LLK):", "NM (SSE):",
                "NM (LLK - unconverged):", "NM (SSE - unconverged):", "SANN (LLK):",
                "SANN (SSE):"
              ) ~ 3
            )
          ) %>%
          dplyr::arrange(
            .data[["Ranking"]], .data[["Method"]]
          ) %>%
          dplyr::select(
            -dplyr::all_of("Ranking")
          )
      }
    )

    ##### Absolute values:----
    ls_outputs[["Comb_grp"]][["Absolute"]] <- purrr::map(
      .x = ls_PSA_combo_cea_tab_calibs_grp %>%
        names(.) %>%
        `names<-`(., .),
      .f = function(.calib_category_) {
        calib_class <- if(.calib_category_ == "random") {
          "Unguided"
        } else if(.calib_category_ == "directed") {
          "Guided"
        } else if(.calib_category_ == "bayesian") {
          "Bayesian"
        } else {
          .calib_category_
        }
        ls_PSA_combo_cea_tab_calibs_grp[[.calib_category_]] %>%
          `class<-`(c(class(.), "icer_tbl")) %>%
          generate_gt_table(
            .label_effects_ = .label_effects_,
            .label_costs_ = .label_costs_,
            .wtp_key_values_ = .wtp_key_values_,
            .currency_symbol_ = .currency_symbol_,
            .output_type_ = .output_type_,
            .output_format_ = "long",
            .output_title_ = paste(
              "Cost-Effectiveness Analysis Results - Truth vs", calib_class,
              "Calibration Methods"
            ),
            .output_subtitle_ = paste(
              "The table shows a subset of the cost-effectiveness results"
            ),
            .output_contents_ = .partial_cea_table_groups_,
            .output_grouped_ = TRUE,
            .output_group_ = "Method"
          )
      }
    )


    ##### Relative values:----
    if(all(
      isTRUE(.add_simulated_truth_),
      isTRUE(.generate_relative_values_)
    )) {
      ###### Prepare relative values table:----
      ls_PSA_combo_cea_tab_calibs_rel_grp <- purrr::map(
        .x = ls_PSA_combo_cea_tab_calibs_grp,
        .f = function(.df_PSA_combo_cea_tab_calibs_grp_) {
          compute_relative_cea_values(
            .df_combined_cea_results_ = .df_PSA_combo_cea_tab_calibs_grp_,
            .interventions_names_ = .model_interventions_,
            .simulated_truth_label_ = "Truth",
            .measures_ = .relative_values_data_,
            .currency_symbol_ = .currency_symbol_
          )
        }
      )

      ###### Generate beautified relative table:----
      ls_outputs[["Comb_grp"]][["Relative"]] <- purrr::map(
        .x = ls_PSA_combo_cea_tab_calibs_rel_grp %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          calib_class <- if(.calib_category_ == "random") {
            "Unguided"
          } else if(.calib_category_ == "directed") {
            "Guided"
          } else if(.calib_category_ == "bayesian") {
            "Bayesian"
          } else {
            .calib_category_
          }
          ls_PSA_combo_cea_tab_calibs_rel_grp[[.calib_category_]] %>%
            `class<-`(c(class(.), "icer_tbl")) %>%
            generate_gt_table(
              .label_effects_ = .label_effects_,
              .label_costs_ = .label_costs_,
              .wtp_key_values_ = .wtp_key_values_,
              .currency_symbol_ = .currency_symbol_,
              .output_type_ = .output_type_,
              .output_format_ = "long",
              .output_title_ = paste(
                "Cost-Effectiveness Analysis Results - Truth vs", calib_class,
                "Calibration Methods"
              ),
              .output_subtitle_ = paste0(
                "The table shows a subset of the cost-effectiveness results,",
                "including the **",
                paste0(
                  .relative_values_data_[-length(.relative_values_data_)],
                  collapse = "**, **"
                ),
                paste0(
                  if(length(.relative_values_data_) > 1) "** and **" else "",
                  .relative_values_data_[length(.relative_values_data_)]
                ),
                "** values from **", calib_class,
                "** callibration methods **relative to the simulated truth**."
              ),
              .output_contents_ = .partial_cea_table_groups_,
              .output_grouped_ = TRUE,
              .output_group_ = "Method"
            )
        }
      )
    }
  }

  return(ls_outputs)
}
