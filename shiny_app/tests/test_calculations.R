# Verification suite for calculate_planner() and build_snapshot_row().
#
# WHY THIS FILE EXISTS
# --------------------
# The calculation logic is the product. These tests encode the non-obvious
# behaviour that makes it correct -- the things a naive reimplementation gets
# wrong. If this planner is ever ported to another language, this file is the
# specification: the new implementation must reproduce these same numbers on
# these same inputs.
#
# Run from anywhere:  Rscript shiny_app/tests/test_calculations.R
# Exits non-zero if any check fails.

# Locate calculations.R relative to this script, so the suite is path-independent.
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
tests_dir <- if (length(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(dirname(tests_dir), "R", "calculations.R"))

# ---- tiny assertion helper -------------------------------------------------
failures <- 0
checks <- 0
TOL <- 1e-9

check <- function(label, actual, expected) {
  checks <<- checks + 1
  ok <- if (is.numeric(actual) && is.numeric(expected)) {
    isTRUE(abs(actual - expected) < TOL)
  } else {
    isTRUE(all.equal(actual, expected))
  }
  if (!ok) {
    failures <<- failures + 1
    cat(sprintf("  FAIL  %s\n          expected: %s\n          actual:   %s\n",
                label, format(expected), format(actual)))
  } else {
    cat(sprintf("  ok    %s\n", label))
  }
}

section <- function(x) cat(sprintf("\n== %s ==\n", x))

# ---- fixtures: the app's current defaults ----------------------------------
tax <- list(
  fed_wh_rate = 0.24, ee_ss_rate = 0.062, er_ss_rate = 0.062, ss_wage_base = 184500,
  ee_medicare_rate = 0.0145, er_medicare_rate = 0.0145, add_medicare_rate = 0.009,
  add_medicare_threshold = 200000,
  state_income_tax_rate = 0.0307, local_tax_rate = 0.0165,   # Pennsylvania
  ee_sui_rate = 0.0007, er_sui_rate = 0.065, sui_wage_base = 10000,
  ee_leave_rate = 0, er_leave_rate = 0, other_state_er_rate = 0,
  futa_rate = 0.006, futa_wage_base = 7000,
  sep_annual_limit = 72000,
  solo401k_deferral_limit = 23500, solo401k_catchup_limit = 7500,
  solo401k_combined_limit = 70000,
  simple_deferral_limit = 16000, simple_catchup_limit = 3500,
  simple_match_rate = 0.03, simple_nonelective_rate = 0.02
)

base <- list(
  planning_month = as.Date("2026-07-01"), scenario_name = "test",
  billable_hours = 157, billing_rate = 100, wage_rate = 50,
  additional_receipts = 0, beginning_cash = 0, other_opex = 0, payroll_fees = 0,
  min_cash_reserve = 0, ytd_wages = 0, additional_fed_withholding = 0,
  retirement_plan_type = "None", sep_rate = 0, ytd_sep = 0,
  solo401k_employer_rate = 0, solo401k_deferral_election = 0,
  solo401k_catchup_eligible = FALSE, ytd_solo401k_deferral = 0, ytd_solo401k_employer = 0,
  simple_employer_formula = "Match", simple_deferral_election = 0,
  simple_catchup_eligible = FALSE, ytd_simple_deferral = 0
)
with_inputs <- function(...) modifyList(base, list(...))

GROSS <- 7850  # 157 hrs x $50

# ---------------------------------------------------------------------------
section("Baseline: no retirement plan")
r <- calculate_planner(base, tax)
check("gross wages = hours x wage rate", r$gross_wages, GROSS)
check("net paycheck", r$net_paycheck, 4989.46)
check("total payroll cash requirement", r$total_payroll_cash_requirement, 9002.775)
check("available cash", r$available_cash, 6697.225)
check("state income tax uses PA 3.07%", r$state_income_tax, GROSS * 0.0307)
check("local tax uses PA 1.65%", r$local_tax, GROSS * 0.0165)

# ---------------------------------------------------------------------------
section("Wage-base caps engage from YTD wages, not from this payroll alone")
# FUTA base is $7,000, so a single payroll of $7,850 already exceeds it.
check("FUTA is capped at the wage base in month 1", r$futa, 7000 * 0.006)
r_ytd <- calculate_planner(with_inputs(ytd_wages = 7850), tax)
check("FUTA is zero once YTD wages exceed the base", r_ytd$futa, 0)
# Social Security: below the base everything is taxable; above it, nothing more.
r_ss <- calculate_planner(with_inputs(ytd_wages = 184500), tax)
check("employee SS stops at the annual wage base", r_ss$ee_ss, 0)
check("employer SS stops at the annual wage base", r_ss$er_ss, 0)
check("Medicare keeps applying above the SS base", r_ss$ee_medicare, GROSS * 0.0145)

# ---------------------------------------------------------------------------
section("Voluntary additional federal withholding")
r_afw <- calculate_planner(with_inputs(additional_fed_withholding = 200), tax)
check("adds a flat amount on top of the rate calculation",
      r_afw$fed_withholding, GROSS * 0.24 + 200)
check("does NOT reduce taxable wages (FICA unchanged)", r_afw$ee_ss, r$ee_ss)
check("reduces the paycheck by exactly the amount", r_afw$net_paycheck, r$net_paycheck - 200)
check("cash-neutral: shifts paycheck -> deposit, total unchanged",
      r_afw$total_payroll_cash_requirement, r$total_payroll_cash_requirement)

# ---------------------------------------------------------------------------
section("SEP-IRA is employer-only")
r_sep <- calculate_planner(with_inputs(retirement_plan_type = "SEP-IRA", sep_rate = 0.15), tax)
check("contribution = rate x gross", r_sep$sep_contribution, GROSS * 0.15)
check("does not touch the employee paycheck", r_sep$net_paycheck, r$net_paycheck)
check("does not touch federal withholding", r_sep$fed_withholding, r$fed_withholding)
check("adds its full cost to the cash requirement",
      r_sep$total_payroll_cash_requirement - r$total_payroll_cash_requirement, GROSS * 0.15)
r_sep_cap <- calculate_planner(
  with_inputs(retirement_plan_type = "SEP-IRA", sep_rate = 0.25, ytd_sep = 71000), tax)
check("capped by remaining room under the annual limit", r_sep_cap$sep_contribution, 1000)

# ---------------------------------------------------------------------------
section("Solo 401(k): deferral is pre-tax for income tax but NOT for FICA")
s401 <- with_inputs(retirement_plan_type = "Solo 401(k)",
                    solo401k_deferral_election = 1000, solo401k_employer_rate = 0.10)
r401 <- calculate_planner(s401, tax)
check("employee deferral taken as elected", r401$solo401k_employee_deferral, 1000)
check("federal withholding computed on REDUCED wages",
      r401$fed_withholding, (GROSS - 1000) * 0.24)
check("state tax computed on REDUCED wages",
      r401$state_income_tax, (GROSS - 1000) * 0.0307)
check("Social Security still on FULL gross wages", r401$ee_ss, r$ee_ss)
check("Medicare still on FULL gross wages", r401$ee_medicare, r$ee_medicare)
check("employer contribution = rate x gross", r401$solo401k_employer_contribution, GROSS * 0.10)
check("only the employer side adds net cost",
      r401$total_payroll_cash_requirement - r$total_payroll_cash_requirement, GROSS * 0.10)

section("Solo 401(k): limits and catch-up")
r_over <- calculate_planner(with_inputs(retirement_plan_type = "Solo 401(k)",
                                        solo401k_deferral_election = 50000), tax)
check("deferral capped at the annual limit", r_over$solo401k_employee_deferral, 23500)
r_catch <- calculate_planner(with_inputs(retirement_plan_type = "Solo 401(k)",
                                         solo401k_deferral_election = 50000,
                                         solo401k_catchup_eligible = TRUE), tax)
check("catch-up raises the deferral limit", r_catch$solo401k_employee_deferral, 23500 + 7500)
# Combined limit: employer contribution is squeezed by whatever the employee deferred.
r_comb <- calculate_planner(with_inputs(retirement_plan_type = "Solo 401(k)",
                                        billable_hours = 5000,          # gross 250,000
                                        solo401k_deferral_election = 23500,
                                        solo401k_employer_rate = 0.25), tax)
check("employer contribution capped by remaining combined room",
      r_comb$solo401k_employer_contribution, 70000 - 23500)
r_comb_catch <- calculate_planner(with_inputs(retirement_plan_type = "Solo 401(k)",
                                              billable_hours = 5000,
                                              solo401k_deferral_election = 23500,
                                              solo401k_employer_rate = 0.25,
                                              solo401k_catchup_eligible = TRUE), tax)
check("catch-up also raises the COMBINED limit",
      r_comb_catch$solo401k_employer_contribution, 70000 + 7500 - 23500)

# ---------------------------------------------------------------------------
section("SIMPLE IRA: Match is dollar-for-dollar UP TO the rate, not a flat rate")
# This is the single easiest thing to get wrong.
r_m_high <- calculate_planner(with_inputs(retirement_plan_type = "SIMPLE IRA",
                                          simple_employer_formula = "Match",
                                          simple_deferral_election = 300), tax)
check("defer above the ceiling -> employer pays the ceiling",
      r_m_high$simple_employer_contribution, GROSS * 0.03)
r_m_low <- calculate_planner(with_inputs(retirement_plan_type = "SIMPLE IRA",
                                         simple_employer_formula = "Match",
                                         simple_deferral_election = 100), tax)
check("defer below the ceiling -> employer matches only what was deferred",
      r_m_low$simple_employer_contribution, 100)
check("  (and that is NOT the flat 3% of gross)",
      r_m_low$simple_employer_contribution != GROSS * 0.03, TRUE)

section("SIMPLE IRA: Nonelective is flat and ignores the deferral")
r_ne <- calculate_planner(with_inputs(retirement_plan_type = "SIMPLE IRA",
                                      simple_employer_formula = "Nonelective",
                                      simple_deferral_election = 0), tax)
check("paid even when the employee defers nothing",
      r_ne$simple_employer_contribution, GROSS * 0.02)

section("SIMPLE IRA: deferral behaves like a pre-tax deferral, and respects limits")
check("federal withholding on reduced wages",
      r_m_high$fed_withholding, (GROSS - 300) * 0.24)
check("FICA unaffected by the deferral", r_m_high$ee_ss, r$ee_ss)
r_s_over <- calculate_planner(with_inputs(retirement_plan_type = "SIMPLE IRA",
                                          simple_deferral_election = 50000), tax)
check("deferral capped at the SIMPLE limit", r_s_over$simple_employee_deferral, 16000)
r_s_catch <- calculate_planner(with_inputs(retirement_plan_type = "SIMPLE IRA",
                                           simple_deferral_election = 50000,
                                           simple_catchup_eligible = TRUE), tax)
check("catch-up raises the SIMPLE limit", r_s_catch$simple_employee_deferral, 16000 + 3500)

# ---------------------------------------------------------------------------
section("Retirement plans are mutually exclusive: no field leakage")
leak <- with_inputs(retirement_plan_type = "SIMPLE IRA", simple_deferral_election = 100,
                    sep_rate = 0.15, solo401k_deferral_election = 5000,
                    solo401k_employer_rate = 0.25)
r_leak <- calculate_planner(leak, tax)
check("SEP inputs ignored when SIMPLE is selected", r_leak$sep_contribution, 0)
check("Solo 401(k) deferral ignored when SIMPLE is selected", r_leak$solo401k_employee_deferral, 0)
check("Solo 401(k) employer ignored when SIMPLE is selected", r_leak$solo401k_employer_contribution, 0)
check("the selected plan still applies", r_leak$simple_employee_deferral, 100)

# ---------------------------------------------------------------------------
section("Cash health margin is monotonic in wage rate (the slider relies on this)")
margins <- sapply(seq(0, 500, by = 10), function(w) {
  calculate_planner(with_inputs(wage_rate = w), tax)$available_cash_margin
})
check("margin never increases as wage rate rises", all(diff(margins) <= TOL), TRUE)
check("status lookup: full margin -> GREAT", lookup_health_status(1.0), "GREAT")
check("status lookup: negative margin -> DEFICIT", lookup_health_status(-0.5), "DEFICIT")
check("status lookup: on a band edge takes the upper band", lookup_health_status(0.15), "SAFE")

# ---------------------------------------------------------------------------
section("Degenerate inputs do not produce NaN/Inf")
r_zero <- calculate_planner(with_inputs(billable_hours = 0), tax)
check("zero gross wages -> cash-requirement ratio is 0, not NaN",
      r_zero$cash_requirement_to_gross_ratio, 0)
check("zero gross wages -> net-pay ratio is 0, not NaN", r_zero$net_pay_to_gross_ratio, 0)
r_norev <- calculate_planner(with_inputs(billing_rate = 0, additional_receipts = 0), tax)
check("zero receipts -> margin is -1, not divide-by-zero", r_norev$available_cash_margin, -1)

# ---------------------------------------------------------------------------
section("Snapshot row")
snap <- calculate_planner(s401, tax)
row <- build_snapshot_row(s401, tax, snap)
check("is a single row", nrow(row), 1L)
check("Scenario Name leads the row", names(row)[1], "Scenario Name")
check("carries the selected plan", row[["Retirement Plan"]], "Solo 401(k)")
check("carries the assumed federal rate", row[["Federal Withholding Rate"]], 0.24)
check("no missing values", any(is.na(row)), FALSE)
row_blank <- build_snapshot_row(with_inputs(planning_month = character(0)), tax, r)
check("a blank planning month yields an empty Month, not an error",
      row_blank[["Month"]], "")

# ---------------------------------------------------------------------------
section("Glossary integrity")
check("all three columns are the same length", nrow(glossary), length(glossary$Term))
check("no missing entries", any(is.na(glossary)), FALSE)
check("no duplicated terms", any(duplicated(glossary$Term)), FALSE)
check("health-status bands are ascending", all(diff(health_status_table$threshold) > 0), TRUE)

# ---------------------------------------------------------------------------
cat(sprintf("\n%d checks, %d failure(s)\n", checks, failures))
if (failures > 0) quit(status = 1)
