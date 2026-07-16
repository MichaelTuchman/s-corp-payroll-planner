# Payroll and cash-planning calculations, ported 1:1 from the Planner
# worksheet's Section 3-5 formulas (workbook/Solo_S_Corp_Payroll_Cash_Planner.xlsx).

# Intentionally hardcoded, not exposed as a UI input: these are the
# product's own safety-margin policy, not a per-scenario assumption like
# a tax rate. A read-only reference table may be added later.
health_status_table <- data.frame(
  threshold = c(-100, 0, 0.05, 0.10, 0.15, 1),
  status = c("DEFICIT", "DIRE WARNING", "TOO CLOSE", "OK", "SAFE", "GREAT"),
  stringsAsFactors = FALSE
)

# Excel VLOOKUP(value, table, 2, TRUE): largest threshold <= value.
lookup_health_status <- function(margin) {
  idx <- findInterval(margin, health_status_table$threshold)
  if (idx < 1) return(NA_character_)
  health_status_table$status[idx]
}

calculate_planner <- function(inputs, tax) {
  gross_wages <- inputs$billable_hours * inputs$wage_rate
  expected_billed_revenue <- inputs$billable_hours * inputs$billing_rate
  client_receipts <- expected_billed_revenue + inputs$additional_receipts

  ss_taxable <- min(gross_wages, max(0, tax$ss_wage_base - inputs$ytd_wages))
  sui_taxable <- min(gross_wages, max(0, tax$sui_wage_base - inputs$ytd_wages))
  futa_taxable <- min(gross_wages, max(0, tax$futa_wage_base - inputs$ytd_wages))

  fed_withholding <- gross_wages * tax$fed_wh_rate
  ee_ss <- ss_taxable * tax$ee_ss_rate
  ee_medicare <- gross_wages * tax$ee_medicare_rate
  add_medicare <- max(0, inputs$ytd_wages + gross_wages - tax$add_medicare_threshold) * tax$add_medicare_rate -
    max(0, inputs$ytd_wages - tax$add_medicare_threshold) * tax$add_medicare_rate
  state_income_tax <- gross_wages * tax$state_income_tax_rate
  local_tax <- gross_wages * tax$local_tax_rate
  ee_sui <- sui_taxable * tax$ee_sui_rate
  ee_leave <- gross_wages * tax$ee_leave_rate

  total_ee_withholding <- fed_withholding + ee_ss + ee_medicare + add_medicare +
    state_income_tax + local_tax + ee_sui + ee_leave
  net_paycheck <- gross_wages - total_ee_withholding

  er_ss <- ss_taxable * tax$er_ss_rate
  er_medicare <- gross_wages * tax$er_medicare_rate
  er_sui <- sui_taxable * tax$er_sui_rate
  er_leave <- gross_wages * tax$er_leave_rate
  other_state_er <- gross_wages * tax$other_state_er_rate
  futa <- futa_taxable * tax$futa_rate
  sep_contribution <- min(gross_wages * inputs$sep_rate, max(0, tax$sep_annual_limit - inputs$ytd_sep))

  federal_deposit <- fed_withholding + ee_ss + ee_medicare + add_medicare + er_ss + er_medicare
  state_wh_deposit <- state_income_tax
  local_deposit <- local_tax
  sui_deposit <- ee_sui + er_sui
  leave_deposit <- ee_leave + er_leave
  other_state_deposit <- other_state_er
  futa_reserve <- futa
  sep_reserve <- sep_contribution

  total_payroll_cash_requirement <- net_paycheck + federal_deposit + state_wh_deposit +
    local_deposit + sui_deposit + leave_deposit + other_state_deposit + futa_reserve + sep_reserve

  cash_after_obligations <- inputs$beginning_cash + client_receipts -
    total_payroll_cash_requirement - inputs$other_opex - inputs$payroll_fees
  available_cash <- cash_after_obligations - inputs$min_cash_reserve
  available_cash_margin <- if (client_receipts == 0) -1 else available_cash / client_receipts
  health_status <- lookup_health_status(available_cash_margin)

  list(
    expected_billed_revenue = expected_billed_revenue,
    client_receipts = client_receipts,
    gross_wages = gross_wages,
    fed_withholding = fed_withholding,
    ee_ss = ee_ss,
    ee_medicare = ee_medicare,
    add_medicare = add_medicare,
    state_income_tax = state_income_tax,
    local_tax = local_tax,
    ee_sui = ee_sui,
    ee_leave = ee_leave,
    total_ee_withholding = total_ee_withholding,
    net_paycheck = net_paycheck,
    er_ss = er_ss,
    er_medicare = er_medicare,
    er_sui = er_sui,
    er_leave = er_leave,
    other_state_er = other_state_er,
    futa = futa,
    sep_contribution = sep_contribution,
    federal_deposit = federal_deposit,
    state_wh_deposit = state_wh_deposit,
    local_deposit = local_deposit,
    sui_deposit = sui_deposit,
    leave_deposit = leave_deposit,
    other_state_deposit = other_state_deposit,
    futa_reserve = futa_reserve,
    sep_reserve = sep_reserve,
    total_payroll_cash_requirement = total_payroll_cash_requirement,
    cash_after_obligations = cash_after_obligations,
    available_cash = available_cash,
    available_cash_margin = available_cash_margin,
    health_status = health_status
  )
}

# Reference definitions for the Glossary modal. Columns mirror the workbook's
# own structure: "Why it matters" / "Purpose" becomes Explanation, and
# "Source / note" becomes Source Info. Calculated (Section 3-5) fields have
# no external source, so Source Info reads "Calculated" and Explanation is
# the workbook's "Formula basis" text.
glossary <- data.frame(
  Term = c(
    "Planning month", "Planned billable hours", "Billing rate", "Wage rate",
    "Additional receipts", "Expected client receipts", "Beginning LLC cash", "Other operating expenses",
    "Payroll service fees", "Minimum operating cash reserve", "SEP contribution rate",
    "YTD wages before this payroll", "YTD SEP contributions before this payroll", "Scenario name",
    "Federal withholding planning rate", "Employee Social Security rate", "Employer Social Security rate",
    "Social Security wage base", "Employee Medicare rate", "Employer Medicare rate",
    "Additional Medicare rate", "Additional Medicare threshold", "State income-tax rate",
    "Local income / occupational tax rate", "Employee state unemployment rate", "Employer state unemployment rate",
    "State unemployment wage base", "Employee leave / disability rate", "Employer leave / disability rate",
    "Other state payroll-tax rate", "FUTA rate", "FUTA wage base",
    "SEP annual contribution limit", "Expected billed revenue",
    "Gross W-2 wages", "Federal income tax withheld", "Employee Social Security", "Employee Medicare",
    "Additional Medicare", "State income tax", "Local income / occupational tax",
    "Employee state unemployment", "Employee leave / disability", "Total employee withholding",
    "Net employee paycheck", "Employer Social Security", "Employer Medicare",
    "Employer state unemployment", "Employer leave / disability", "Other state payroll tax",
    "FUTA", "SEP contribution", "Total payroll cash requirement", "Cash after all obligations",
    "Available cash", "Available cash margin", "Cash Health Status"
  ),
  Explanation = c(
    "Labels the scenario and future register row.",
    "Drives billed revenue and gross wages.",
    "Converts hours into expected billed revenue.",
    "Converts hours into gross payroll.",
    "Cash beyond rate x hours — e.g. a prior-month collection, retainer, or advance.",
    "Rate x hours, plus any additional receipts. Cash planning uses receipts, not only billed revenue.",
    "Determines cash available before payroll.",
    "Reduces available cash.",
    "Reduces available cash.",
    "Protects the business from running too close.",
    "Creates a retirement reserve.",
    "Applies annual wage bases and thresholds.",
    "Applies the annual SEP contribution limit.",
    "Identifies this scenario at a glance — shown first in the snapshot table, a better label than the month alone.",
    "Estimates federal income-tax withholding.",
    "Employee OASDI withholding.",
    "Employer OASDI contribution.",
    "Caps employee and employer Social Security.",
    "Employee Medicare withholding.",
    "Employer Medicare contribution.",
    "Additional Medicare withholding above threshold.",
    "Employer withholding threshold.",
    "Generic user-entered state withholding rate.",
    "Generic local payroll-tax rate.",
    "Employee UI contribution where applicable.",
    "Employer UI contribution.",
    "Caps state unemployment estimate.",
    "State paid-leave or disability deduction.",
    "Employer paid-leave or disability contribution.",
    "Other state payroll assessment.",
    "Federal unemployment estimate.",
    "Caps FUTA estimate.",
    "Caps employer SEP contribution.",
    "Billable hours × billing rate.",
    "Billable hours × wage rate.",
    "Gross wages × federal planning rate.",
    "Subject to annual wage base.",
    "Gross wages × Medicare rate.",
    "Applies above annual threshold.",
    "Gross wages × state rate.",
    "Gross wages × local rate.",
    "Subject to state wage base.",
    "Gross wages × employee rate.",
    "Sum of employee withholding.",
    "Gross wages − employee withholding.",
    "Subject to annual wage base.",
    "Gross wages × Medicare rate.",
    "Subject to state wage base.",
    "Gross wages × employer rate.",
    "Gross wages × other state rate.",
    "Subject to FUTA wage base.",
    "Gross wages × SEP rate, capped annually.",
    "Net paycheck + all deposits + SEP.",
    "Beginning cash + receipts − obligations − expenses.",
    "Cash after obligations − minimum reserve.",
    "Available cash ÷ client receipts.",
    "Approximate threshold lookup (see the thresholds table above)."
  ),
  "Source Info" = c(
    "User input", "User input", "Client contract", "Owner compensation policy",
    "User input", "Calculated", "Bank balance", "User input", "User input", "Owner policy",
    "Owner policy", "Payroll records", "Retirement records", "User input",
    "Replace with accountant's actual withholding when available.",
    "IRS Publication 15", "IRS Publication 15", "SSA annual wage base",
    "IRS Publication 15", "IRS Publication 15", "IRS Topic 560", "IRS Topic 560",
    "Enter local current rate.", "Enter zero if none applies.",
    "Enter zero if none applies.", "Use assigned employer rate.",
    "Enter current state wage base.", "Enter zero if none applies.",
    "Enter zero if none applies.", "Enter zero if none applies.",
    "Assumes full state credit.", "IRS Topic 759", "IRS annual limit",
    "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Builds the horizontal snapshot row, extended from row 87 of the workbook
# with the Federal/State/Local tax rates in effect for the scenario (stored
# as raw decimals, matching Available Cash Margin's convention) since those
# are user-editable and worth keeping as a record even though they change
# infrequently.
build_snapshot_row <- function(inputs, tax, results) {
  data.frame(
    "Scenario Name" = inputs$scenario_name,
    "Month" = format(inputs$planning_month, "%Y-%m"),
    "Planned Billable Hours" = inputs$billable_hours,
    "Billing Rate ($/hr)" = inputs$billing_rate,
    "Wage Rate ($/hr)" = inputs$wage_rate,
    "Expected Billed Revenue ($)" = results$expected_billed_revenue,
    "Additional Receipts ($)" = inputs$additional_receipts,
    "Expected Client Receipts ($)" = results$client_receipts,
    "Beginning LLC Cash ($)" = inputs$beginning_cash,
    "Gross Wages ($)" = results$gross_wages,
    "Federal Withholding Rate" = tax$fed_wh_rate,
    "Federal Withholding ($)" = results$fed_withholding,
    "Employee Social Security ($)" = results$ee_ss,
    "Employee Medicare ($)" = results$ee_medicare,
    "Additional Medicare ($)" = results$add_medicare,
    "State Income Tax Rate" = tax$state_income_tax_rate,
    "State Income Tax ($)" = results$state_income_tax,
    "Local Tax Rate" = tax$local_tax_rate,
    "Local Tax ($)" = results$local_tax,
    "Employee State UI ($)" = results$ee_sui,
    "Employee Leave / Disability ($)" = results$ee_leave,
    "Total Employee Withholding ($)" = results$total_ee_withholding,
    "Net Paycheck ($)" = results$net_paycheck,
    "Employer Social Security ($)" = results$er_ss,
    "Employer Medicare ($)" = results$er_medicare,
    "Employer State UI ($)" = results$er_sui,
    "Employer Leave / Disability ($)" = results$er_leave,
    "Other State Payroll Tax ($)" = results$other_state_er,
    "FUTA ($)" = results$futa,
    "SEP Contribution ($)" = results$sep_contribution,
    "Federal Payroll Tax Deposit ($)" = results$federal_deposit,
    "State Withholding Deposit ($)" = results$state_wh_deposit,
    "Local Tax Deposit ($)" = results$local_deposit,
    "State UI Deposit ($)" = results$sui_deposit,
    "State Leave / Disability Deposit ($)" = results$leave_deposit,
    "Other State Payroll Deposit ($)" = results$other_state_deposit,
    "FUTA Reserve ($)" = results$futa_reserve,
    "SEP Reserve ($)" = results$sep_reserve,
    "Total Payroll Cash Requirement ($)" = results$total_payroll_cash_requirement,
    "Other Operating Expenses ($)" = inputs$other_opex,
    "Payroll Service Fees ($)" = inputs$payroll_fees,
    "Minimum Cash Reserve ($)" = inputs$min_cash_reserve,
    "Cash After Obligations ($)" = results$cash_after_obligations,
    "Available Cash ($)" = results$available_cash,
    "Available Cash Margin" = results$available_cash_margin,
    "Cash Health Status" = results$health_status,
    check.names = FALSE
  )
}
