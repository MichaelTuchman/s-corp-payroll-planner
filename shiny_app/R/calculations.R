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
  receipts_timing_diff <- inputs$client_receipts - expected_billed_revenue

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

  cash_after_obligations <- inputs$beginning_cash + inputs$client_receipts -
    total_payroll_cash_requirement - inputs$other_opex - inputs$payroll_fees
  available_cash <- cash_after_obligations - inputs$min_cash_reserve
  available_cash_margin <- if (inputs$client_receipts == 0) -1 else available_cash / inputs$client_receipts
  health_status <- lookup_health_status(available_cash_margin)

  list(
    expected_billed_revenue = expected_billed_revenue,
    receipts_timing_diff = receipts_timing_diff,
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

# Reference definitions for the Glossary modal, condensed from the
# workbook's own "What it means" / "Formula basis" / "Interpretation" columns.
glossary <- data.frame(
  Term = c(
    "Planning month", "Planned billable hours", "Billing rate", "Wage rate",
    "Expected client receipts", "Beginning LLC cash", "Other operating expenses",
    "Payroll service fees", "Minimum operating cash reserve", "SEP contribution rate",
    "YTD wages before this payroll", "YTD SEP contributions before this payroll",
    "Federal withholding planning rate", "Employee/Employer Social Security rate",
    "Social Security wage base", "Employee/Employer Medicare rate",
    "Additional Medicare rate", "Additional Medicare threshold", "State income-tax rate",
    "Local income / occupational tax rate", "Employee/Employer state unemployment rate",
    "State unemployment wage base", "Employee/Employer leave / disability rate",
    "Other state payroll-tax rate", "FUTA rate", "FUTA wage base",
    "SEP annual contribution limit", "Expected billed revenue", "Receipts timing difference",
    "Gross W-2 wages", "Total employee withholding", "Net employee paycheck",
    "SEP contribution", "Total payroll cash requirement", "Cash after all obligations",
    "Available cash", "Available cash margin", "Cash Health Status"
  ),
  Definition = c(
    "The month this scenario models.",
    "Client hours expected this month; drives revenue and gross wages.",
    "Amount charged per billable hour.",
    "Hourly W-2 wage paid to the owner-employee.",
    "Cash expected from the client this month — may differ from billed revenue.",
    "Business cash on hand before this month's activity.",
    "Insurance, software, accounting, travel, and similar costs.",
    "Accountant or payroll-provider fee.",
    "Cash intentionally kept in reserve after all obligations are met.",
    "Employer-only retirement contribution, as a % of wages.",
    "Wages already paid earlier this year, for applying annual wage caps.",
    "SEP contributions already made this year, for the annual SEP limit.",
    "Estimated % of wages withheld for federal income tax.",
    "FICA OASDI rate — normally 6.2% on both the employee and employer side.",
    "Annual wage level above which Social Security tax stops applying.",
    "Medicare payroll tax rate — normally 1.45% on both sides.",
    "Extra employee Medicare withholding (0.9%) on wages above the threshold.",
    "Annual wage level (IRS default $200,000) above which Additional Medicare applies.",
    "Your state's income-tax withholding rate on wages.",
    "Local payroll tax rate, if your area has one.",
    "State unemployment insurance (SUI) contribution rate.",
    "Annual wage cap for state unemployment tax.",
    "State paid-family-leave or disability-insurance rate.",
    "Any other state payroll assessment not covered above.",
    "Federal Unemployment Tax Act rate — federal unemployment insurance.",
    "Annual wage cap for FUTA.",
    "IRS cap on total employer SEP contributions per year.",
    "Planned billable hours × billing rate.",
    "Expected client receipts minus expected billed revenue.",
    "Planned billable hours × wage rate.",
    "Sum of everything withheld from the paycheck (taxes, etc.).",
    "Gross wages minus total employee withholding.",
    "Simplified Employee Pension — an employer-funded retirement contribution.",
    "Net paycheck plus all tax deposits and reserves for this payroll.",
    "Beginning cash + receipts − payroll requirement − other expenses.",
    "Cash after obligations, minus the minimum reserve you want to keep.",
    "Available cash ÷ expected client receipts.",
    "A DEFICIT-to-GREAT rating based on the available cash margin (see the thresholds table in the Glossary)."
  ),
  stringsAsFactors = FALSE
)

# Builds the 41-column horizontal snapshot row (mirrors row 87 of the workbook).
build_snapshot_row <- function(inputs, tax, results) {
  data.frame(
    "Month" = format(inputs$planning_month, "%Y-%m"),
    "Planned Billable Hours" = inputs$billable_hours,
    "Billing Rate ($/hr)" = inputs$billing_rate,
    "Wage Rate ($/hr)" = inputs$wage_rate,
    "Expected Billed Revenue ($)" = results$expected_billed_revenue,
    "Expected Client Receipts ($)" = inputs$client_receipts,
    "Beginning LLC Cash ($)" = inputs$beginning_cash,
    "Gross Wages ($)" = results$gross_wages,
    "Federal Withholding ($)" = results$fed_withholding,
    "Employee Social Security ($)" = results$ee_ss,
    "Employee Medicare ($)" = results$ee_medicare,
    "Additional Medicare ($)" = results$add_medicare,
    "State Income Tax ($)" = results$state_income_tax,
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
    "Notes" = inputs$notes,
    check.names = FALSE
  )
}
