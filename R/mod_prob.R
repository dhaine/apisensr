# Probabilistic Analysis Module UI

#' @title   UI Module for Probabilistic Analysis tab
#' @description  A shiny Module to render the Probabilistic Analysis tab (selection bias
#' analysis `probsens.sel`, bias analysis for unmeasured confounder `probsens.conf`, and
#' misclassification bias analysis `probsens`).
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_prob
#'
#' @keywords internal
#' @noRd
#' @import episensr
#' @import ggplot2
#' @importFrom shiny NS tagList
#' @importFrom shinyjs runjs
#' @importFrom rhandsontable hot_to_r rHandsontableOutput renderRHandsontable rhandsontable
mod_prob_ui <- function(id, label = "tab_prob") {
  ns <- NS(id)

  material_tab_content(
      tab_id = id,
      material_row(
          material_column(
              width = 4,
              material_card(
                  material_dropdown(
                      ns("prob_type"),
                      label = "Choose bias analysis:",
                      choices = c(
                          "Selection bias" = "probsens_sel",
                          "Unmeasured confounder" = "probsens_conf",
                          "Misclassification bias" = "probsens"
                      ),
                      color = "#d50000"
                  ),
                  "Observed data",
                  div(id = "obs-table-prob",
                      rhandsontable::rHandsontableOutput(ns('two_by_two_prob')),
                      material_button(
                          input_id = ns("reset_table"),
                          label = "Table back to example",
                          icon = "restore",
                          color = "red accent-4"
                      )
                      ),
                  br(),
                  div(
                      id = "side-panel-prob",
                      conditionalPanel(
                          condition = 'input.prob_type == "probsens"',
                          ns = ns,
                          material_radio_button(
                              input_id = ns("misclassProb_type"),
                              label = "Misclassification of:",
                              choices = c("exposure", "outcome"),
                              selected = "exposure",
                              color = "#ff5131"),
                          br(),
                          material_number_box(
                              input_id = ns("chosen_seed"),
                              label = "Please select a seed",
                              min_value = 1,
                              max_value = 1000000,
                              initial_value = 1,
                              color = "#ff5131"
                          ),
                          material_slider(
                              input_id = ns("reps"),
                              label = "Number of replications to run:",
                              min_value = 10000,
                              max_value = 100000,
                              step_size = 5000,
                              initial_value = 25000,
                              color = "#ff5131"),
                          material_switch(
                              input_id = ns("diff"),
                              off_label = "Non-differential misclassification",
                              on_label = "Differential misclassification",
                              initial_value = FALSE,
                              color = "#ff5131"),
                          br(),
                          material_checkbox(
                              input_id = ns("discard"),
                              label = "Discard draws of negative adjusted counts.",
                              initial_value = TRUE,
                              color = "#9b0000"),
                          conditionalPanel(
                              condition = 'input.diff == 1',
                              ns = ns,
                              material_slider(
                                  input_id = ns("corr_se"),
                                  label = "Correlation between case and non-case sensitivities:",
                                  min_value = 0,
                                  max_value = 1,
                                  step_size = 0.1,
                                  initial_value = 0.8,
                                  color = "#ff5131"
                              ),
                              material_slider(
                                  input_id = ns("corr_sp"),
                                  label = "Correlation between case and non-case specificities:",
                                  min_value = 0,
                                  max_value = 1,
                                  step_size = 0.1,
                                  initial_value = 0.8,
                                  color = "9b0000"
                              )
                          ),
                          material_button(
                              input_id = "help_probsens",
                              label = "Help",
                              icon = "help",
                              color = "orange"),
                          br(),
                          material_dropdown(
                              input_id = ns("plot_probsens"),
                              label = "Plot of output/parameters:",
                              choices = c(
                                  "Relative risk - systematic error" = "rr",
                                  "Odds ratio - systematic error" = "or",
                                  "Relative risk - systematic and random error" = "rr_tot",
                                  "Odds ratio - systematic and random error" = "or_tot",
                                  "Sensitivity for cases" = "seca",
                                  "Sensitivity of exposed" = "seexp",
                                  "Specificity for cases" = "spca",
                                  "Specificity of exposed" = "spexp"),
                              selected = "rr",
                              color = "#ff5131")
                      ),
                      conditionalPanel(
                          condition = 'input.prob_type == "probsens_sel"',
                          ns = ns,
                          material_slider(
                              input_id = ns("reps_sel"),
                              label = "Number of replications to run:",
                              min_value = 10000,
                              max_value = 100000,
                              step_size = 5000,
                              initial_value = 25000,
                              color = "#ff5131"),
                          material_switch(
                              input_id = ns("or_case"),
                              off_label = "Using selection bias odds",
                              on_label = "Using selection probabilities",
                              initial_value = FALSE,
                              color = "#ff5131"),
                          br(),
                          material_button(
                              input_id = "help_probsens_sel",
                              label = "Help",
                              icon = "help",
                              color = "orange"),
                          material_dropdown(
                              input_id = ns("plot_probsens_sel"),
                              label = "Plot of output/parameters",
                              choices = c(
                                  "Odds ratio - systematic error" = "or",
                                  "Odds ratio - systematic and random error" = "or_tot",
                                  "Selection odds ratio" = "or_sel"),
                              selected = "or",
                              color = "#ff5131")
                      ),
                      conditionalPanel(
                          condition = 'input.prob_type == "probsens_conf"',
                          ns = ns,
                          material_slider(
                              input_id = ns("reps_conf"),
                              label = "Number of replications to run:",
                              min_value = 10000,
                              max_value = 100000,
                              step_size = 5000,
                              initial_value = 25000,
                              color = "#ff5131"),
                          material_slider(
                              input_id = ns("corr_conf"),
                              label = "Correlation between the exposure-specific confounder prevalences:",
                              min_value = 0,
                              max_value = 1,
                              step_size = 0.1,
                              initial_value = 0.8,
                              color = "#ff5131"),
                          material_checkbox(
                              input_id = ns("discard_conf"),
                              label = "Discard draws of negative adjusted counts.",
                              initial_value = TRUE,
                              color = "#ff5131"),
                          br(),
                          material_button(
                              input_id = "help_probsens_conf",
                              label = "Help",
                              icon = "help",
                              color = "orange"),
                          material_dropdown(
                              input_id = ns("plot_probsens_conf"),
                              label = "Plot of output/parameters",
                              choices = c(
                                  "Relative risk - systematic error" = "rr",
                                  "Odds ratio - systematic error" = "or",
                                  "Relative risk - systematic and random error" = "rr_tot",
                                  "Odds ratio - systematic and random error" = "or_tot",
                                  "Distribution of prevalence of exposure among the exposed" = "prev.exp",
                                  "Distribution of prevalence of exposure among the unexposed" = "prev.nexp",
                                  "Distribution of confounder-disease relative risk (or confounder-exposure odds ratio)" = "risk"),
                              selected = "rr",
                              color = "#9b0000")
                      ),
                      ## Alpha level
                      material_slider(
                          ns("alpha"),
                          HTML("&alpha;-level:"),
                          min_value = 0.01,
                          max_value = 0.2,
                          step_size = 0.01,
                          initial_value = 0.05,
                          color = "#ff5131")
                      )
              )
          ),
          material_column(
              width = 4,
              material_card(
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens"',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("seca_parms"),
                          label = "Distribution, sensitivity of exposure classification among those with the outcome:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"),
                          selected = "trapezoidal",
                          color = "#ff5131"),
                      conditionalPanel(
                          condition = 'input.seca_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_seca_C"), "Constant value:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_seca_U"), "Minimum and maximum:",
                                          0.7, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_seca_Tr1"),
                                          "Lower and upper limit:", 0.7, 0.9),
                          mod_parms_ui(ns("parms_seca_Tr2"), "Mode:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_seca_Tz1"),
                                          "Minimum and maximum:", 0.75, 1),
                          mod_parmsrge_ui(ns("parms_seca_Tz2"),
                                          "Lower and upper mode:", 0.85, 0.95)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_seca_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_seca_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_seca_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_seca_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_seca_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_seca_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.seca_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_seca_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 908, color = "#ff5131"),
                          material_number_box(ns("parms_seca_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 56, color = "#ff5131")
                      ),
                      conditionalPanel(
                          condition = 'input.diff == 1',
                          ns = ns,
                          material_dropdown(
                              input_id = ns("seexp_parms"),
                              label = "Distribution, sensitivity of exposure classification among those without the outcome:",
                              choices = c(
                                  "Constant" = "constant",
                                  "Uniform" = "uniform",
                                  "Triangular" = "triangular",
                                  "Trapezoidal" = "trapezoidal",
                                  "Logit-logistic" = "logit-logistic",
                                  "Logit-normal" = "logit-normal",
                                  "Beta" = "beta"),
                              selected = "trapezoidal",
                              color = "#ff5131"),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "constant"',
                              ns = ns,
                              mod_parms_ui(ns("parms_seexp_C"), "Constant value:", 0.8)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "uniform"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_seexp_U"), "Minimum and maximum:",
                                              0.7, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "triangular"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_seexp_Tr1"),
                                              "Lower and upper limit:", 0.7, 0.9),
                              mod_parms_ui(ns("parms_seexp_Tr2"), "Mode:", 0.8)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "trapezoidal"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_seexp_Tz1"),
                                              "Minimum and maximum:", 0.75, 1),
                              mod_parmsrge_ui(ns("parms_seexp_Tz2"),
                                              "Lower and upper mode:", 0.85, 0.95)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "logit-logistic"',
                              ns = ns,
                              mod_parms2a_ui(ns("parms_seexp_Ll1"), "Location:", 0, -5, 5),
                              mod_parms2a_ui(ns("parms_seexp_Ll2"), "Scale:", 0.8, -10, 10),
                              mod_parmsrge_ui(ns("parms_seexp_Ll3"),
                                              "Lower and upper bound shift:", 0.5, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "logit-normal"',
                              ns = ns,
                              mod_parms2a_ui(ns("parms_seexp_Ln1"), "Location:", 0, -5, 5),
                              mod_parms2a_ui(ns("parms_seexp_Ln2"), "Scale:", 0.8, -10, 10),
                              mod_parmsrge_ui(ns("parms_seexp_Ln3"),
                                              "Lower and upper bound shift:", 0.5, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.seexp_parms == "beta"',
                              ns = ns,
                              material_number_box(ns("parms_seexp_B1"),
                                                  "alpha:", min_value = 0, max_value = 10^6,
                                                  initial_value = 908, color = "#ff5131"),
                              material_number_box(ns("parms_seexp_B2"),
                                                  "beta:", min_value = 0, max_value = 10^6,
                                                  initial_value = 56, color = "#ff5131")
                          )
                      )
                  ),
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens_sel" & input.or_case == 0',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("or_parms"),
                          label = "Distribution of selection bias odds:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Log-logistic" = "log-logistic",
                              "Log-normal" = "log-normal"
                          ),
                          selected = "triangular",
                          color = "#ff5131"),
                      conditionalPanel(
                          condition = 'input.or_parms == "constant"',
                          ns = ns,
                          mod_parms2_ui(ns("parms_or_C"), "Constant value:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.or_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_or_U"), "Minimum and maximum:",
                                           0.35, 1.0, 0.01)
                      ),
                      conditionalPanel(
                          condition = 'input.or_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_or_Tr1"),
                                          "Lower and upper limit:", 0.35, 1.0, 0.01),
                          mod_parms2_ui(ns("parms_or_Tr2"), "Mode:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.or_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_or_Tz1"),
                                          "Minimum and maximum:", 0.3, 1, 0.01),
                          mod_parmsrge2_ui(ns("parms_or_Tz2"),
                                          "Lower and upper mode:", 0.4, 1, 0.01)
                      ),
                      conditionalPanel(
                          condition = 'input.or_parms == "log-logistic"',
                          ns = ns,
                          mod_parms2b_ui(ns("parms_or_Ll1"), "Shape:", 4),
                          mod_parms2b_ui(ns("parms_or_Ll2"), "Rate:", 0.5)
                      ),
                      conditionalPanel(
                          condition = 'input.or_parms == "log-normal"',
                          ns = ns,
                          mod_parms2c_ui(ns("parms_or_Ln1"), "Mean log:", 0),
                          mod_parms2c_ui(ns("parms_or_Ln2"), "SD log:", 0.5)
                      )
                  ),
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens_sel" & input.or_case == 1',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("cexp_parms"),
                          label = "Distribution of selection probability among cases exposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"
                          ),
                          selected = "triangular",
                          color = "#ff5131"),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_cexp_C"), "Constant value:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cexp_U"), "Minimum and maximum:",
                                           0.35, 1.0)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cexp_Tr1"),
                                          "Lower and upper limit:", 0.35, 1.0),
                          mod_parms_ui(ns("parms_cexp_Tr2"), "Mode:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cexp_Tz1"),
                                          "Minimum and maximum:", 0.3, 1),
                          mod_parmsrge_ui(ns("parms_cexp_Tz2"),
                                          "Lower and upper mode:", 0.4, 1)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_cexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_cexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_cexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_cexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_cexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_cexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.cexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_cexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 153, color = "#9b0000"),
                          material_number_box(ns("parms_cexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 6, color = "#9b0000")
                      ),
                      material_dropdown(
                          input_id = ns("cnexp_parms"),
                          label = "Distribution of selection probability among cases unexposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"
                          ),
                          selected = "triangular",
                          color = "#9b0000"),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_cnexp_C"), "Constant value:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cnexp_U"), "Minimum and maximum:",
                                           0.35, 1.0)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cnexp_Tr1"),
                                          "Lower and upper limit:", 0.35, 1.0),
                          mod_parms_ui(ns("parms_cnexp_Tr2"), "Mode:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_cnexp_Tz1"),
                                          "Minimum and maximum:", 0.3, 1),
                          mod_parmsrge_ui(ns("parms_cnexp_Tz2"),
                                          "Lower and upper mode:", 0.4, 1)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_cnexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_cnexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_cnexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_cnexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_cnexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_cnexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.cnexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_cnexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 153, color = "#9b0000"),
                          material_number_box(ns("parms_cnexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 6, color = "#9b0000")
                      )
                  ),
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens_conf"',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("prevexp_parms"),
                          label = "Distribution of prevalence of exposure among exposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"),
                          selected = "triangular",
                          color = "#ff5131"),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_prevexp_C"), "Constant value:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevexp_U"), "Minimum and maximum:",
                                          0.7, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevexp_Tr1"),
                                          "Lower and upper limit:", 0.7, 0.9),
                          mod_parms_ui(ns("parms_prevexp_Tr2"), "Mode:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevexp_Tz1"),
                                          "Minimum and maximum:", 0.75, 1),
                          mod_parmsrge_ui(ns("parms_prevexp_Tz2"),
                                          "Lower and upper mode:", 0.85, 0.95)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_prevexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_prevexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_prevexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_prevexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_prevexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_prevexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_prevexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 200, color = "#ff5131"),
                          material_number_box(ns("parms_prevexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 56, color = "#ff5131")
                      ),
                      material_dropdown(
                          input_id = ns("prevnexp_parms"),
                          label = "Distribution of prevalence of exposure among non-exposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"),
                          selected = "trapezoidal",
                          color = "#ff5131"),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_prevnexp_C"), "Constant value:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevnexp_U"), "Minimum and maximum:",
                                          0.7, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevnexp_Tr1"),
                                          "Lower and upper limit:", 0.7, 0.9),
                          mod_parms_ui(ns("parms_prevnexp_Tr2"), "Mode:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_prevnexp_Tz1"),
                                          "Minimum and maximum:", 0.03, 0.06),
                          mod_parmsrge_ui(ns("parms_prevnexp_Tz2"),
                                          "Lower and upper mode:", 0.04, 0.05)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_prevnexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_prevnexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_prevnexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_prevnexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_prevnexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_prevnexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.prevnexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_prevnexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 10, color = "#ff5131"),
                          material_number_box(ns("parms_prevnexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 16, color = "#ff5131")
                      )
                  )
              )
          ),
          material_column(
              width = 4,
              material_card(
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens"',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("spca_parms"),
                          label = "Distribution, specificity of exposure classification among those with the outcome:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"),
                          selected = "trapezoidal",
                          color = "#9b0000"),
                      conditionalPanel(
                          condition = 'input.spca_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_spca_C"), "Constant value:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_spca_U"), "Minimum and maximum:",
                                          0.7, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_spca_Tr1"),
                                          "Lower and upper limit:", 0.7, 0.9),
                          mod_parms_ui(ns("parms_spca_Tr2"), "Mode:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_spca_Tz1"),
                                          "Minimum and maximum:", 0.75, 1),
                          mod_parmsrge_ui(ns("parms_spca_Tz2"),
                                          "Lower and upper mode:", 0.85, 0.95)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_spca_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_spca_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_spca_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_spca_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_spca_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_spca_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.spca_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_spca_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 153, color = "#9b0000"),
                          material_number_box(ns("parms_spca_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 6, color = "#9b0000")
                      ),
                      conditionalPanel(
                          condition = 'input.diff == 1',
                          ns = ns,
                          material_dropdown(
                              input_id = ns("spexp_parms"),
                              label = "Distribution, specificity of exposure classification among those without the outcome:",
                              choices = c(
                                  "Constant" = "constant",
                                  "Uniform" = "uniform",
                                  "Triangular" = "triangular",
                                  "Trapezoidal" = "trapezoidal",
                                  "Logit-logistic" = "logit-logistic",
                                  "Logit-normal" = "logit-normal",
                                  "Beta" = "beta"),
                              selected = "trapezoidal",
                              color = "#9b0000"),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "constant"',
                              ns = ns,
                              mod_parms_ui(ns("parms_spexp_C"), "Constant value:", 0.8)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "uniform"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_spexp_U"), "Minimum and maximum:",
                                              0.7, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "triangular"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_spexp_Tr1"),
                                              "Lower and upper limit:", 0.7, 0.9),
                              mod_parms_ui(ns("parms_spexp_Tr2"), "Mode:", 0.8)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "trapezoidal"',
                              ns = ns,
                              mod_parmsrge_ui(ns("parms_spexp_Tz1"),
                                              "Minimum and maximum:", 0.75, 1),
                              mod_parmsrge_ui(ns("parms_spexp_Tz2"),
                                              "Lower and upper mode:", 0.85, 0.95)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "logit-logistic"',
                              ns = ns,
                              mod_parms2a_ui(ns("parms_spexp_Ll1"), "Location:", 0, -5, 5),
                              mod_parms2a_ui(ns("parms_spexp_Ll2"), "Scale:", 0.8, -10, 10),
                              mod_parmsrge_ui(ns("parms_spexp_Ll3"),
                                              "Lower and upper bound shift:", 0.5, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "logit-normal"',
                              ns = ns,
                              mod_parms2a_ui(ns("parms_spexp_Ln1"), "Location:", 0, -5, 5),
                              mod_parms2a_ui(ns("parms_spexp_Ln2"), "Scale:", 0.8, -10, 10),
                              mod_parmsrge_ui(ns("parms_spexp_Ln3"),
                                              "Lower and upper bound shift:", 0.5, 0.9)
                          ),
                          conditionalPanel(
                              condition = 'input.spexp_parms == "beta"',
                              ns = ns,
                              material_number_box(ns("parms_spexp_B1"),
                                                  "alpha:", min_value = 0, max_value = 10^6,
                                                  initial_value = 908, color = "#ff5131"),
                              material_number_box(ns("parms_spexp_B2"),
                                                  "beta:", min_value = 0, max_value = 10^6,
                                                  initial_value = 56, color = "#ff5131")
                          )
                      )
                  ),
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens_sel" & input.or_case == 1',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("ncexp_parms"),
                          label = "Distribution of selection probability among noncases exposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"
                          ),
                          selected = "triangular",
                          color = "#9b0000"),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_ncexp_C"), "Constant value:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncexp_U"), "Minimum and maximum:",
                                           0.35, 1.0)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncexp_Tr1"),
                                          "Lower and upper limit:", 0.35, 1.0),
                          mod_parms_ui(ns("parms_ncexp_Tr2"), "Mode:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncexp_Tz1"),
                                          "Minimum and maximum:", 0.3, 1),
                          mod_parmsrge_ui(ns("parms_ncexp_Tz2"),
                                          "Lower and upper mode:", 0.4, 1)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_ncexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_ncexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_ncexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_ncexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_ncexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_ncexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.ncexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_ncexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 153, color = "#9b0000"),
                          material_number_box(ns("parms_ncexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 6, color = "#9b0000")
                      ),
                      material_dropdown(
                          input_id = ns("ncnexp_parms"),
                          label = "Distribution of selection probability among noncases unexposed:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal",
                              "Beta" = "beta"
                          ),
                          selected = "triangular",
                          color = "#9b0000"),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "constant"',
                          ns = ns,
                          mod_parms_ui(ns("parms_ncnexp_C"), "Constant value:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncnexp_U"), "Minimum and maximum:",
                                           0.35, 1.0)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncnexp_Tr1"),
                                          "Lower and upper limit:", 0.35, 1.0),
                          mod_parms_ui(ns("parms_ncnexp_Tr2"), "Mode:", 0.43)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge_ui(ns("parms_ncnexp_Tz1"),
                                          "Minimum and maximum:", 0.3, 1),
                          mod_parmsrge_ui(ns("parms_ncnexp_Tz2"),
                                          "Lower and upper mode:", 0.4, 1)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_ncnexp_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_ncnexp_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_ncnexp_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_ncnexp_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_ncnexp_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_ncnexp_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.ncnexp_parms == "beta"',
                          ns = ns,
                          material_number_box(ns("parms_ncnexp_B1"),
                                              "alpha:", min_value = 0, max_value = 10^6,
                                              initial_value = 153, color = "#9b0000"),
                          material_number_box(ns("parms_ncnexp_B2"),
                                              "beta:", min_value = 0, max_value = 10^6,
                                              initial_value = 6, color = "#9b0000")
                      )
                  ),
                  conditionalPanel(
                      condition = 'input.prob_type == "probsens_conf"',
                      ns = ns,
                      material_dropdown(
                          input_id = ns("risk_parms"),
                          label = "Distribution of confounder-disease relative risk or confounder-exposure odds ratio:",
                          choices = c(
                              "Constant" = "constant",
                              "Uniform" = "uniform",
                              "Triangular" = "triangular",
                              "Trapezoidal" = "trapezoidal",
                              "Logit-logistic" = "logit-logistic",
                              "Logit-normal" = "logit-normal"),
                          selected = "triangular",
                          color = "#9b0000"),
                      conditionalPanel(
                          condition = 'input.risk_parms == "constant"',
                          ns = ns,
                          mod_parms2_ui(ns("parms_risk_C"), "Constant value:", 0.8)
                      ),
                      conditionalPanel(
                          condition = 'input.risk_parms == "uniform"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_risk_U"), "Minimum and maximum:",
                                          0.7, 0.9, 0.01)
                      ),
                      conditionalPanel(
                          condition = 'input.risk_parms == "triangular"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_risk_Tr1"),
                                          "Lower and upper limit:", 0.6, 0.7, 0.01),
                          mod_parms2_ui(ns("parms_risk_Tr2"), "Mode:", 0.63)
                      ),
                      conditionalPanel(
                          condition = 'input.risk_parms == "trapezoidal"',
                          ns = ns,
                          mod_parmsrge2_ui(ns("parms_risk_Tz1"),
                                          "Minimum and maximum:", 0.75, 1, 0.01),
                          mod_parmsrge2_ui(ns("parms_risk_Tz2"),
                                          "Lower and upper mode:", 0.85, 0.95, 0.01)
                      ),
                      conditionalPanel(
                          condition = 'input.risk_parms == "logit-logistic"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_risk_Ll1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_risk_Ll2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_risk_Ll3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      ),
                      conditionalPanel(
                          condition = 'input.risk_parms == "logit-normal"',
                          ns = ns,
                          mod_parms2a_ui(ns("parms_risk_Ln1"), "Location:", 0, -5, 5),
                          mod_parms2a_ui(ns("parms_risk_Ln2"), "Scale:", 0.8, -10, 10),
                          mod_parmsrge_ui(ns("parms_risk_Ln3"),
                                          "Lower and upper bound shift:", 0.5, 0.9)
                      )
                  )
              )
          ),
      material_column(
          width = 8,
          material_card(
              verbatimTextOutput(ns("summary_prob")),
              verbatimTextOutput(ns("warnings_prob")),
              verbatimTextOutput(ns("message_prob")),
              plotOutput(ns("plot_res"), width = "600px")
          )
      )
      )
  )
}

# Module Server

#' @rdname mod_prob
#' @noRd
#' @keywords internal

mod_prob_server <- function(input, output, session) {
    ns <- session$ns

    global_seed = reactive(input$chosen_seed)
    ## reactives can only be in observers or other reactives.
    ## setting priority to 5 helps set seed before `renderPrint` is executed
    observe({
                set.seed(global_seed())
            }, priority = 5)

    DF = reactive({
                      if (input$prob_type == "probsens") {
                          data.frame(Exposed = c(45, 257), Unexposed = c(94, 945),
                                     row.names = c("Cases", "Noncases"))
                      } else if (input$prob_type == "probsens_sel") {
                          data.frame(Exposed = c(136, 297), Unexposed = c(107, 165),
                                     row.names = c("Cases", "Non-cases"))
                      } else if (input$prob_type == "probsens_conf") {
                          data.frame(Exposed = c(105, 527), Unexposed = c(85, 93),
                                     row.names = c("Cases", "Non-cases"))
                      }
                  })

    output$two_by_two_prob = rhandsontable::renderRHandsontable({
                                                                    input$reset_table # trigger rendering on reset
                                                                    rhandsontable::rhandsontable(DF(), stretchH = "all")
                                                 })

    episensrout = reactive({
                               mat <- as.matrix(rhandsontable::hot_to_r(req({input$two_by_two_prob})))
                               if (input$seca_parms == "trapezoidal") {
                                   dist_seca <- c(callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Tz1")[2])
                               } else if (input$seca_parms == "logit-logistic") {
                                   dist_seca <- c(callModule(mod_parms2a_server,
                                                             "parms_seca_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_seca_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Ll3")[2])
                               } else if (input$seca_parms == "logit-normal") {
                                   dist_seca <- c(callModule(mod_parms2a_server,
                                                             "parms_seca_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_seca_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seca_Ln3")[2])
                               } else if (input$seca_parms == "triangular") {
                                   dist_seca <- c(callModule(mod_parmsrge_server,
                                                             "parms_seca_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_seca_Tr2"))
                               } else if (input$seca_parms == "uniform") {
                                   dist_seca <- callModule(mod_parmsrge_server,
                                                           "parms_seca_U")
                               } else if (input$seca_parms == "constant") {
                                   dist_seca <- callModule(mod_parms_server, "parms_seca_C")
                               } else if (input$seca_parms == "beta") {
                                   dist_seca <- c(input$parms_seca_B1, input$parms_seca_B2)
                               }
                               if (input$seexp_parms == "trapezoidal") {
                                   dist_seexp <- c(callModule(mod_parmsrge_server,
                                                              "parms_seexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Tz1")[2])
                               } else if (input$seexp_parms == "logit-logistic") {
                                   dist_seexp <- c(callModule(mod_parms2a_server,
                                                             "parms_seexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_seexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Ll3")[2])
                               } else if (input$seexp_parms == "logit-normal") {
                                   dist_seexp <- c(callModule(mod_parms2a_server,
                                                             "parms_seexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_seexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_seexp_Ln3")[2])
                               } else if (input$seexp_parms == "triangular") {
                                   dist_seexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_seexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_seexp_Tr2"))
                               } else if (input$seexp_parms == "uniform") {
                                   dist_seexp <- callModule(mod_parmsrge_server,
                                                           "parms_seexp_U")
                               } else if (input$seexp_parms == "constant") {
                                   dist_seexp <- callModule(mod_parms_server,
                                                            "parms_seexp_C")
                               } else if (input$seexp_parms == "beta") {
                                   dist_seexp <- c(input$parms_seexp_B1,
                                                   input$parms_seexp_B2)
                               }
                               if (input$spca_parms == "trapezoidal") {
                                   dist_spca <- c(callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Tz1")[2])
                               } else if (input$spca_parms == "logit-logistic") {
                                   dist_spca <- c(callModule(mod_parms2a_server,
                                                             "parms_spca_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_spca_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Ll3")[2])
                               } else if (input$spca_parms == "logit-normal") {
                                   dist_spca <- c(callModule(mod_parms2a_server,
                                                             "parms_spca_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_spca_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spca_Ln3")[2])
                               } else if (input$spca_parms == "triangular") {
                                   dist_spca <- c(callModule(mod_parmsrge_server,
                                                             "parms_spca_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_spca_Tr2"))
                               } else if (input$spca_parms == "uniform") {
                                   dist_spca <- callModule(mod_parmsrge_server,
                                                           "parms_spca_U")
                               } else if (input$spca_parms == "constant") {
                                   dist_spca <- callModule(mod_parms_server, "parms_spca_C")
                               } else if (input$spca_parms == "beta") {
                                   dist_spca <- c(input$parms_spca_B1, input$parms_spca_B2)
                               }
                               if (input$spexp_parms == "trapezoidal") {
                                   dist_spexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_spexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Tz1")[2])
                               } else if (input$spexp_parms == "logit-logistic") {
                                   dist_spexp <- c(callModule(mod_parms2a_server,
                                                             "parms_spexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_spexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Ll3")[2])
                               } else if (input$spexp_parms == "logit-normal") {
                                   dist_spexp <- c(callModule(mod_parms2a_server,
                                                             "parms_spexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_spexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_spexp_Ln3")[2])
                               } else if (input$spexp_parms == "triangular") {
                                   dist_spexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_spexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_spexp_Tr2"))
                               } else if (input$spexp_parms == "uniform") {
                                   dist_spexp <- callModule(mod_parmsrge_server,
                                                           "parms_spexp_U")
                               } else if (input$spexp_parms == "constant") {
                                   dist_spexp <- callModule(mod_parms_server,
                                                            "parms_spexp_C")
                               } else if (input$spexp_parms == "beta") {
                                   dist_spexp <- c(input$parms_spexp_B1,
                                                   input$parms_spexp_B2)
                               }
                               if (input$or_parms == "constant") {
                                   dist_orparms <- callModule(mod_parms_server,
                                                              "parms_or_C")
                               } else if (input$or_parms == "uniform") {
                                   dist_orparms <- callModule(mod_parmsrge2_server,
                                                              "parms_or_U")
                               } else if (input$or_parms == "triangular") {
                                   dist_orparms <- c(callModule(mod_parmsrge2_server,
                                                                "parms_or_Tr1"),
                                                     callModule(mod_parms2_server,
                                                                "parms_or_Tr2"))
                               } else if (input$or_parms == "trapezoidal") {
                                   dist_orparms <- c(callModule(mod_parmsrge2_server,
                                                                "parms_or_Tz1")[1],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_or_Tz2")[1],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_or_Tz2")[2],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_or_Tz1")[2])
                               } else if (input$or_parms == "log-logistic") {
                                   dist_orparms <- c(callModule(mod_parms2_server,
                                                                "parms_or_Ll1"),
                                                     callModule(mod_parms2_server,
                                                                "parms_or_Ll2"))
                               } else if (input$or_parms == "log-normal") {
                                   dist_orparms <- c(callModule(mod_parms2c_server,
                                                                "parms_or_Ln1"),
                                                     callModule(mod_parms2c_server,
                                                                "parms_or_Ln2"))
                               }
                               if (input$cexp_parms == "constant") {
                                   dist_cexp <- callModule(mod_parms_server,
                                                           "parms_cexp_C")
                               } else if (input$cexp_parms == "uniform") {
                                   dist_cexp <- callModule(mod_parmsrge_server,
                                                           "parms_cexp_U")
                               } else if (input$cexp_parms == "triangular") {
                                   dist_cexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_cexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_cexp_Tr2"))
                               } else if (input$cexp_parms == "trapezoidal") {
                                   dist_cexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_cexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Tz1")[2])
                               } else if (input$cexp_parms == "logit-logistic") {
                                   dist_cexp <- c(callModule(mod_parms2a_server,
                                                             "parms_cexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_cexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Ll3")[2])
                               } else if (input$cexp_parms == "logit-normal") {
                                   dist_cexp <- c(callModule(mod_parms2a_server,
                                                             "parms_cexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_cexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cexp_Ln3")[2])
                               } else if (input$cexp_parms == "beta") {
                                   dist_cexp <- c(input$parms_cexp_B1,
                                                  input$parms_cexp_B2)
                               }
                               if (input$cnexp_parms == "constant") {
                                   dist_cnexp <- callModule(mod_parms_server,
                                                           "parms_cnexp_C")
                               } else if (input$cnexp_parms == "uniform") {
                                   dist_cnexp <- callModule(mod_parmsrge_server,
                                                           "parms_cnexp_U")
                               } else if (input$cnexp_parms == "triangular") {
                                   dist_cnexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_cnexp_Tr2"))
                               } else if (input$cnexp_parms == "trapezoidal") {
                                   dist_cnexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Tz1")[2])
                               } else if (input$cnexp_parms == "logit-logistic") {
                                   dist_cnexp <- c(callModule(mod_parms2a_server,
                                                             "parms_cnexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_cnexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Ll3")[2])
                               } else if (input$cnexp_parms == "logit-normal") {
                                   dist_cnexp <- c(callModule(mod_parms2a_server,
                                                             "parms_cnexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_cnexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_cnexp_Ln3")[2])
                               } else if (input$cnexp_parms == "beta") {
                                   dist_cnexp <- c(input$parms_cnexp_B1,
                                                  input$parms_cnexp_B2)
                               }
                               if (input$ncexp_parms == "constant") {
                                   dist_ncexp <- callModule(mod_parms_server,
                                                           "parms_ncexp_C")
                               } else if (input$ncexp_parms == "uniform") {
                                   dist_ncexp <- callModule(mod_parmsrge_server,
                                                           "parms_ncexp_U")
                               } else if (input$ncexp_parms == "triangular") {
                                   dist_ncexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_ncexp_Tr2"))
                               } else if (input$ncexp_parms == "trapezoidal") {
                                   dist_ncexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Tz1")[2])
                               } else if (input$ncexp_parms == "logit-logistic") {
                                   dist_ncexp <- c(callModule(mod_parms2a_server,
                                                             "parms_ncexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_ncexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Ll3")[2])
                               } else if (input$ncexp_parms == "logit-normal") {
                                   dist_ncexp <- c(callModule(mod_parms2a_server,
                                                             "parms_ncexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_ncexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncexp_Ln3")[2])
                               } else if (input$ncexp_parms == "beta") {
                                   dist_ncexp <- c(input$parms_ncexp_B1,
                                                  input$parms_ncexp_B2)
                               }
                               if (input$ncnexp_parms == "constant") {
                                   dist_ncnexp <- callModule(mod_parms_server,
                                                           "parms_ncnexp_C")
                               } else if (input$ncnexp_parms == "uniform") {
                                   dist_ncnexp <- callModule(mod_parmsrge_server,
                                                           "parms_ncnexp_U")
                               } else if (input$ncnexp_parms == "triangular") {
                                   dist_ncnexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Tr1"),
                                                  callModule(mod_parms_server,
                                                             "parms_ncnexp_Tr2"))
                               } else if (input$ncnexp_parms == "trapezoidal") {
                                   dist_ncnexp <- c(callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Tz1")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Tz2")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Tz2")[2],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Tz1")[2])
                               } else if (input$ncnexp_parms == "logit-logistic") {
                                   dist_ncnexp <- c(callModule(mod_parms2a_server,
                                                             "parms_ncnexp_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_ncnexp_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Ll3")[2])
                               } else if (input$ncnexp_parms == "logit-normal") {
                                   dist_ncnexp <- c(callModule(mod_parms2a_server,
                                                             "parms_ncnexp_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_ncnexp_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_ncnexp_Ln3")[2])
                               } else if (input$ncnexp_parms == "beta") {
                                   dist_ncnexp <- c(input$parms_ncnexp_B1,
                                                  input$parms_ncnexp_B2)
                               }
                               if (input$prevexp_parms == "constant") {
                                   dist_prevexp <- callModule(mod_parms_server,
                                                              "parms_prevexp_C")
                               } else if (input$prevexp_parms == "uniform") {
                                   dist_prevexp <- callModule(mod_parmsrge_server,
                                                              "parms_prevexp_U")
                               } else if (input$prevexp_parms == "triangular") {
                                   dist_prevexp <- c(callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Tr1"),
                                                     callModule(mod_parms_server,
                                                                "parms_prevexp_Tr2"))
                               } else if (input$prevexp_parms == "trapezoidal") {
                                   dist_prevexp <- c(callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Tz1")[1],
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Tz2")[1],
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Tz2")[2],
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Tz1")[2])
                               } else if (input$prevexp_parms == "logit-logistic") {
                                   dist_prevexp <- c(callModule(mod_parms2a_server,
                                                                "parms_prevexp_Ll1"),
                                                     callModule(mod_parms2a_server,
                                                                "parms_prevexp_Ll2"),
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Ll3")[1],
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Ll3")[2])
                               } else if (input$prevexp_parms == "logit-normal") {
                                   dist_prevexp <- c(callModule(mod_parms2a_server,
                                                                "parms_prevexp_Ln1"),
                                                     callModule(mod_parms2a_server,
                                                                "parms_prevexp_Ln2"),
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Ln3")[1],
                                                     callModule(mod_parmsrge_server,
                                                                "parms_prevexp_Ln3")[2])
                               } else if (input$prevexp_parms == "beta") {
                                   dist_prevexp <- c(input$parms_prevexp_B1,
                                                     input$parms_prevexp_B2)
                               }
                               if (input$prevnexp_parms == "constant") {
                                   dist_prevnexp <- callModule(mod_parms_server,
                                                               "parms_prevnexp_C")
                               } else if (input$prevnexp_parms == "uniform") {
                                   dist_prevnexp <- callModule(mod_parmsrge_server,
                                                               "parms_prevnexp_U")
                               } else if (input$prevnexp_parms == "triangular") {
                                   dist_prevnexp <- c(callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Tr1"),
                                                      callModule(mod_parms_server,
                                                                 "parms_prevnexp_Tr2"))
                               } else if (input$prevnexp_parms == "trapezoidal") {
                                   dist_prevnexp <- c(callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Tz1")[1],
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Tz2")[1],
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Tz2")[2],
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Tz1")[2])
                               } else if (input$prevnexp_parms == "logit-logistic") {
                                   dist_prevnexp <- c(callModule(mod_parms2a_server,
                                                                 "parms_prevnexp_Ll1"),
                                                      callModule(mod_parms2a_server,
                                                                 "parms_prevnexp_Ll2"),
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Ll3")[1],
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Ll3")[2])
                               } else if (input$prevnexp_parms == "logit-normal") {
                                   dist_prevnexp <- c(callModule(mod_parms2a_server,
                                                                 "parms_prevnexp_Ln1"),
                                                      callModule(mod_parms2a_server,
                                                                 "parms_prevnexp_Ln2"),
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Ln3")[1],
                                                      callModule(mod_parmsrge_server,
                                                                 "parms_prevnexp_Ln3")[2])
                               } else if (input$prevnexp_parms == "beta") {
                                   dist_prevnexp <- c(input$parms_prevnexp_B1,
                                                      input$parms_prevnexp_B2)
                               }
                               if (input$risk_parms == "constant") {
                                   dist_risk <- callModule(mod_parms2_server,
                                                           "parms_risk_C")
                               } else if (input$risk_parms == "uniform") {
                                   dist_risk <- callModule(mod_parmsrge2_server,
                                                           "parms_risk_U")
                               } else if (input$risk_parms == "triangular") {
                                   dist_risk <- c(callModule(mod_parmsrge2_server,
                                                             "parms_risk_Tr1"),
                                                  callModule(mod_parms2_server,
                                                             "parms_risk_Tr2"))
                               } else if (input$risk_parms == "trapezoidal") {
                                   dist_risk <- c(callModule(mod_parmsrge2_server,
                                                             "parms_risk_Tz1")[1],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_risk_Tz2")[1],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_risk_Tz2")[2],
                                                  callModule(mod_parmsrge2_server,
                                                             "parms_risk_Tz1")[2])
                               } else if (input$risk_parms == "logit-logistic") {
                                   dist_risk <- c(callModule(mod_parms2a_server,
                                                             "parms_risk_Ll1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_risk_Ll2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_risk_Ll3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_risk_Ll3")[2])
                               } else if (input$risk_parms == "logit-normal") {
                                   dist_risk <- c(callModule(mod_parms2a_server,
                                                             "parms_risk_Ln1"),
                                                  callModule(mod_parms2a_server,
                                                             "parms_risk_Ln2"),
                                                  callModule(mod_parmsrge_server,
                                                             "parms_risk_Ln3")[1],
                                                  callModule(mod_parmsrge_server,
                                                             "parms_risk_Ln3")[2])
                               }

                               if (input$discard == 0) {
                                   throw_away <- FALSE
                               } else throw_away <- TRUE
                               if (input$discard_conf == 0) {
                                   throw_away_conf <- FALSE
                               } else throw_away_conf <- TRUE

                               if (input$prob_type == "probsens" & input$diff == 0) {
#                                   set.seed(global_seed())
                                   input$chosen_seed
                                   episensr::probsens(mat,
                                                      type = input$misclassProb_type,
                                                      reps = input$reps,
                                                      seca.parms = list(input$seca_parms,
                                                                        dist_seca),
                                                      spca.parms = list(input$spca_parms,
                                                                        dist_spca),
                                                      discard = throw_away,
                                                      alpha = input$alpha)
                               } else if (input$prob_type == "probsens" & input$diff == 1) {
                                   episensr::probsens(mat,
                                                      type = input$misclassProb_type,
                                                      reps = input$reps,
                                                      seca.parms = list(input$seca_parms,
                                                                        dist_seca),
                                                      seexp.parms = list(input$seexp_parms,
                                                                         dist_seexp),
                                                      spca.parms = list(input$spca_parms,
                                                                        dist_spca),
                                                      spexp.parms = list(input$spexp_parms,
                                                                         dist_spexp),
                                                      corr.se = input$corr_se,
                                                      corr.sp = input$corr_sp,
                                                      discard = throw_away,
                                                      alpha = input$alpha)
                               } else if (input$prob_type == "probsens_sel" &
                                          input$or_case == 0) {
                                   episensr::probsens.sel(mat,
                                                          reps = input$reps_sel,
                                                          or.parms = list(input$or_parms,
                                                                          dist_orparms),
                                                          alpha = input$alpha)
                               } else if (input$prob_type == "probsens_sel" &
                                          input$or_case == 1) {
                                   episensr::probsens.sel(mat,
                                                          reps = input$reps_sel,
                                                          case.exp = list(input$cexp_parms,
                                                                          dist_cexp),
                                                          case.nexp = list(input$cnexp_parms,
                                                                           dist_cnexp),
                                                          ncase.exp = list(input$ncexp_parms,
                                                                           dist_ncexp),
                                                          ncase.nexp = list(input$ncnexp_parms,
                                                                            dist_ncnexp),
                                                          alpha = input$alpha)
                               } else if (input$prob_type == "probsens_conf") {
                                   episensr::probsens.conf(mat,
                                                           reps = input$reps_conf,
                                                           prev.exp = list(input$prevexp_parms,
                                                                           dist_prevexp),
                                                           prev.nexp = list(input$prevnexp_parms,
                                                                            dist_prevnexp),
                                                           risk = list(input$risk_parms,
                                                                       dist_risk),
                                                           corr.p = input$corr_conf,
                                                           discard = throw_away_conf,
                                                           alpha = input$alpha)
                               }
                           })

    plotout = reactive({
                           if (input$prob_type == "probsens") {
                               plot(episensrout(), input$plot_probsens)
                           } else if (input$prob_type == "probsens_sel") {
                               plot(episensrout(), input$plot_probsens_sel)
                           } else if (input$prob_type == "probsens_conf") {
                               plot(episensrout(), input$plot_probsens_conf)
                           }
                       })

    ## Output
    output$summary_prob = renderPrint({
                                          episensrout()
                                      })
    output$warnings_prob = renderText({
                                          invisible(episensrout()$warnings)
                                      })
    output$message_prob = renderText({
                                          invisible(episensrout()$message)
                                     })

    output$plot_res <- renderPlot({
                                      plotout()
                                  })

    shinyjs::runjs("document.getElementById('help_probsens').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/probsens.html', '_blank');
         };"
         )

    shinyjs::runjs("document.getElementById('help_probsens_sel').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/probsens.sel.html', '_blank');
         };"
         )

    shinyjs::runjs("document.getElementById('help_probsens_conf').onclick = function() {
           window.open('https://dhaine.github.io/episensr/reference/probsens.conf.html', '_blank');
         };"
         )
}

## To be copied in the UI
# mod_prob_ui("tab_prob")

## To be copied in the server
# callModule(mod_prob_server, "tab_prob")
