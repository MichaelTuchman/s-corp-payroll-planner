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

  # Retirement plan: SEP-IRA, Solo 401(k), and SIMPLE IRA are mutually
  # exclusive per scenario (matches real law -- an employer generally can't
  # run a SIMPLE IRA alongside another qualified plan in the same year
  # either). SEP is employer-only. Solo 401(k) and SIMPLE IRA both have an
  # employee elective deferral (pre-tax: reduces federal/state/local taxable
  # wages below, but not FICA wages, which stay on full gross_wages).
  #
  # Solo 401(k)'s employer side is a freely-chosen profit-share rate capped
  # by an overall combined limit (raised by age-50+ catch-up). SIMPLE IRA's
  # employer side is NOT a free rate -- the law only allows one of two
  # formulas per year: a dollar-for-dollar match up to a fixed rate (capped
  # by what the employee actually deferred, not a free percentage), or a
  # flat nonelective rate paid regardless of the employee's own deferral.
  # SIMPLE has no equivalent combined-limit ceiling the way 401(k) does --
  # the employer side is inherently small by formula.
  is_sep <- identical(inputs$retirement_plan_type, "SEP-IRA")
  is_solo401k <- identical(inputs$retirement_plan_type, "Solo 401(k)")
  is_simple <- identical(inputs$retirement_plan_type, "SIMPLE IRA")

  sep_contribution <- if (is_sep) {
    min(gross_wages * inputs$sep_rate, max(0, tax$sep_annual_limit - inputs$ytd_sep))
  } else {
    0
  }

  # Room remaining is computed regardless of which plan is active, so the UI
  # can show/react to it live even as the user is dialing in a scenario.
  catchup <- if (isTRUE(inputs$solo401k_catchup_eligible)) tax$solo401k_catchup_limit else 0
  solo401k_deferral_room <- max(0, tax$solo401k_deferral_limit + catchup - inputs$ytd_solo401k_deferral)

  if (is_solo401k) {
    solo401k_employee_deferral <- min(inputs$solo401k_deferral_election, solo401k_deferral_room)
  } else {
    solo401k_employee_deferral <- 0
  }

  solo401k_employer_room <- max(0, tax$solo401k_combined_limit + catchup -
    inputs$ytd_solo401k_deferral - inputs$ytd_solo401k_employer - solo401k_employee_deferral)

  if (is_solo401k) {
    solo401k_employer_contribution <- min(gross_wages * inputs$solo401k_employer_rate, solo401k_employer_room)
  } else {
    solo401k_employer_contribution <- 0
  }

  simple_catchup <- if (isTRUE(inputs$simple_catchup_eligible)) tax$simple_catchup_limit else 0
  simple_deferral_room <- max(0, tax$simple_deferral_limit + simple_catchup - inputs$ytd_simple_deferral)

  if (is_simple) {
    simple_employee_deferral <- min(inputs$simple_deferral_election, simple_deferral_room)
    simple_employer_contribution <- if (identical(inputs$simple_employer_formula, "Match")) {
      min(simple_employee_deferral, gross_wages * tax$simple_match_rate)
    } else {
      gross_wages * tax$simple_nonelective_rate
    }
  } else {
    simple_employee_deferral <- 0
    simple_employer_contribution <- 0
  }

  taxable_wages <- gross_wages - solo401k_employee_deferral - simple_employee_deferral

  # Voluntary additional federal withholding (Form W-4 Step 4(c)) is a flat
  # amount added on top of the standard calculation -- unlike the Solo
  # 401(k) deferral, it doesn't reduce taxable wages, since it's withholding
  # itself, not a pre-tax election.
  fed_withholding <- taxable_wages * tax$fed_wh_rate + inputs$additional_fed_withholding
  ee_ss <- ss_taxable * tax$ee_ss_rate
  ee_medicare <- gross_wages * tax$ee_medicare_rate
  add_medicare <- max(0, inputs$ytd_wages + gross_wages - tax$add_medicare_threshold) * tax$add_medicare_rate -
    max(0, inputs$ytd_wages - tax$add_medicare_threshold) * tax$add_medicare_rate
  # Retirement deferrals reduce ONLY federal taxable wages. Pennsylvania does
  # not recognize elective deferrals (401(k), SIMPLE, etc.) as pre-tax for
  # state or local income tax, so those are computed on full gross wages --
  # confirmed against a CPA-prepared PA paystub (gross 12,040, deferral 600:
  # SITW 369.63 = 3.07% x 12,040, LITW 198.66 = 1.65% x 12,040). FICA is
  # likewise on full gross. Only fed_withholding above uses taxable_wages.
  # NOTE: this is the PA treatment. Most states follow the federal pre-tax
  # rule, so a nationwide version would need this to be configurable.
  state_income_tax <- gross_wages * tax$state_income_tax_rate
  local_tax <- gross_wages * tax$local_tax_rate
  # Employee unemployment: some states (notably PA) tax the employee portion on
  # ALL wages with no wage base cap, while the employer portion stays capped at
  # the wage base. The toggle controls the employee side only -- confirmed
  # against the PA paystub (PASUI $8.43 = 0.07% x full $12,040, uncapped;
  # employer PA UC still capped at the $10,000 base).
  ee_sui_taxable <- if (isTRUE(tax$ee_sui_uncapped)) gross_wages else sui_taxable
  ee_sui <- ee_sui_taxable * tax$ee_sui_rate
  ee_leave <- gross_wages * tax$ee_leave_rate

  # Local Services Tax (LST): a flat per-pay dollar amount some PA
  # municipalities levy on people who work there (commonly up to $52/year,
  # spread across pay periods). It is NOT a percentage of wages and does not
  # reduce taxable wages -- it is simply withheld from the paycheck and
  # remitted to the local taxing authority, so it is cash-neutral to the total
  # payroll cost (net pay drops by it, its deposit rises by it). Confirmed
  # against the CPA paystub, where it is the "Other" line ($4.00 = the LST
  # column on the detail report). Defaults to 0 for the general (non-PA) case.
  lst <- if (is.null(tax$lst) || is.na(tax$lst)) 0 else tax$lst

  total_ee_withholding <- fed_withholding + ee_ss + ee_medicare + add_medicare +
    state_income_tax + local_tax + ee_sui + ee_leave + lst
  net_paycheck <- gross_wages - total_ee_withholding - solo401k_employee_deferral - simple_employee_deferral

  er_ss <- ss_taxable * tax$er_ss_rate
  er_medicare <- gross_wages * tax$er_medicare_rate
  er_sui <- sui_taxable * tax$er_sui_rate
  er_leave <- gross_wages * tax$er_leave_rate
  other_state_er <- gross_wages * tax$other_state_er_rate
  futa <- futa_taxable * tax$futa_rate

  federal_deposit <- fed_withholding + ee_ss + ee_medicare + add_medicare + er_ss + er_medicare
  state_wh_deposit <- state_income_tax
  local_deposit <- local_tax
  lst_deposit <- lst
  sui_deposit <- ee_sui + er_sui
  leave_deposit <- ee_leave + er_leave
  other_state_deposit <- other_state_er
  futa_reserve <- futa
  sep_reserve <- sep_contribution
  solo401k_reserve <- solo401k_employee_deferral + solo401k_employer_contribution
  simple_reserve <- simple_employee_deferral + simple_employer_contribution

  # Plan-agnostic totals. Since the plans are mutually exclusive, at most one
  # term in each sum is non-zero, so these report the active plan's figures
  # without the caller needing to know which plan is selected. The snapshot
  # uses these instead of one column per plan, which left two always-zero
  # columns on every row.
  retirement_employee_deferral <- solo401k_employee_deferral + simple_employee_deferral
  retirement_employer_contribution <- sep_contribution + solo401k_employer_contribution +
    simple_employer_contribution
  retirement_reserve <- sep_reserve + solo401k_reserve + simple_reserve

  total_payroll_cash_requirement <- net_paycheck + federal_deposit + state_wh_deposit +
    local_deposit + lst_deposit + sui_deposit + leave_deposit + other_state_deposit + futa_reserve +
    retirement_reserve

  cash_after_obligations <- inputs$beginning_cash + client_receipts -
    total_payroll_cash_requirement - inputs$other_opex - inputs$payroll_fees
  available_cash <- cash_after_obligations - inputs$min_cash_reserve
  available_cash_margin <- if (client_receipts == 0) -1 else available_cash / client_receipts
  health_status <- lookup_health_status(available_cash_margin)

  # Personal-interest ratios, not used elsewhere in the app's own logic.
  cash_requirement_to_gross_ratio <- if (gross_wages == 0) 0 else total_payroll_cash_requirement / gross_wages
  net_pay_to_gross_ratio <- if (gross_wages == 0) 0 else net_paycheck / gross_wages

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
    lst = lst,
    total_ee_withholding = total_ee_withholding,
    net_paycheck = net_paycheck,
    er_ss = er_ss,
    er_medicare = er_medicare,
    er_sui = er_sui,
    er_leave = er_leave,
    other_state_er = other_state_er,
    futa = futa,
    sep_contribution = sep_contribution,
    solo401k_employee_deferral = solo401k_employee_deferral,
    solo401k_employer_contribution = solo401k_employer_contribution,
    solo401k_deferral_room = solo401k_deferral_room,
    solo401k_employer_room = solo401k_employer_room,
    simple_employee_deferral = simple_employee_deferral,
    simple_employer_contribution = simple_employer_contribution,
    simple_deferral_room = simple_deferral_room,
    simple_reserve = simple_reserve,
    retirement_employee_deferral = retirement_employee_deferral,
    retirement_employer_contribution = retirement_employer_contribution,
    retirement_reserve = retirement_reserve,
    federal_deposit = federal_deposit,
    state_wh_deposit = state_wh_deposit,
    local_deposit = local_deposit,
    lst_deposit = lst_deposit,
    sui_deposit = sui_deposit,
    leave_deposit = leave_deposit,
    other_state_deposit = other_state_deposit,
    futa_reserve = futa_reserve,
    sep_reserve = sep_reserve,
    solo401k_reserve = solo401k_reserve,
    total_payroll_cash_requirement = total_payroll_cash_requirement,
    cash_after_obligations = cash_after_obligations,
    available_cash = available_cash,
    available_cash_margin = available_cash_margin,
    health_status = health_status,
    cash_requirement_to_gross_ratio = cash_requirement_to_gross_ratio,
    net_pay_to_gross_ratio = net_pay_to_gross_ratio
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
    "Payroll service fees", "Minimum operating cash reserve", "YTD wages before this payroll",
    "Voluntary additional federal withholding",
    "Retirement plan", "SEP contribution rate", "YTD SEP contributions before this payroll",
    "Solo 401(k) employer profit-sharing rate", "Employee elective deferral this payroll (Solo 401(k))",
    "Age 50+ catch-up eligible (Solo 401(k))", "YTD Solo 401(k) employee deferrals", "YTD Solo 401(k) employer contributions",
    "SIMPLE IRA employer contribution formula", "Employee elective deferral this payroll (SIMPLE IRA)",
    "Age 50+ catch-up eligible (SIMPLE IRA)", "YTD SIMPLE IRA employee deferrals",
    "Scenario name",
    "Federal withholding planning rate", "Employee Social Security rate", "Employer Social Security rate",
    "Social Security wage base", "Employee Medicare rate", "Employer Medicare rate",
    "Additional Medicare rate", "Additional Medicare threshold", "State income-tax rate",
    "Local income / occupational tax rate", "Employee state unemployment rate", "Employer state unemployment rate",
    "State unemployment wage base", "Employee leave / disability rate", "Employer leave / disability rate",
    "Other state payroll-tax rate", "FUTA rate", "FUTA wage base",
    "SEP annual contribution limit", "Solo 401(k) employee deferral limit", "Solo 401(k) catch-up limit",
    "Solo 401(k) combined contribution limit", "SIMPLE IRA employee deferral limit", "SIMPLE IRA catch-up limit",
    "SIMPLE IRA employer match rate", "SIMPLE IRA employer nonelective rate",
    "Expected billed revenue",
    "Gross W-2 wages", "Federal income tax withheld", "Employee Social Security", "Employee Medicare",
    "Additional Medicare", "State income tax", "Local income / occupational tax",
    "Employee state unemployment", "Employee leave / disability", "Local Services Tax (LST)",
    "Total employee withholding",
    "Solo 401(k) employee elective deferral", "SIMPLE IRA employee elective deferral",
    "Net employee paycheck", "Employer Social Security", "Employer Medicare",
    "Employer state unemployment", "Employer leave / disability", "Other state payroll tax",
    "FUTA", "SEP contribution", "Solo 401(k) employer contribution", "SIMPLE IRA employer contribution",
    "Total payroll cash requirement",
    "Cash after all obligations", "Available cash", "Available cash margin", "Cash Health Status",
    "Total payroll cash requirement / gross wages", "Net pay / gross pay"
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
    "Applies annual wage bases and thresholds.",
    "A flat amount withheld beyond the standard rate calculation, at the employee's election (Form W-4 Step 4(c)). Added directly to federal withholding, unlike a pre-tax deferral.",
    "Choose SEP-IRA, Solo 401(k), or SIMPLE IRA — mutually exclusive; only one plan's inputs apply per scenario (matches real law: an employer generally can't run a SIMPLE IRA alongside another qualified plan the same year either).",
    "Creates a retirement reserve.",
    "Applies the annual SEP contribution limit.",
    "Employer-only contribution, same mechanic as SEP's rate.",
    "Comes out of the paycheck itself; pre-tax for federal income tax only. In PA it does not reduce state or local income tax, and never reduces FICA wages.",
    "Raises both the employee deferral limit and the combined limit by the catch-up amount.",
    "Applies the annual employee deferral limit (plus catch-up if eligible).",
    "Applies the combined employee + employer limit.",
    "The law only allows one of two employer formulas per year: a dollar-for-dollar match (capped by the match rate), or a flat nonelective rate paid regardless of what the employee defers.",
    "Comes out of the paycheck itself; pre-tax for federal income tax only. In PA it does not reduce state or local income tax, and never reduces FICA wages. Subject to SIMPLE IRA's own (lower) deferral limit, not the Solo 401(k) limit.",
    "Raises the SIMPLE IRA employee deferral limit by the catch-up amount.",
    "Applies the SIMPLE IRA annual employee deferral limit (plus catch-up if eligible).",
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
    "Caps the employee elective-deferral bucket, before any catch-up.",
    "Additional amount allowed for the employee deferral (and the combined limit) if age 50+.",
    "Caps employee deferral + employer contribution together (Section 415(c)).",
    "Caps the SIMPLE IRA employee elective-deferral bucket, before any catch-up. Lower than the Solo 401(k) limit.",
    "Additional amount allowed for the SIMPLE IRA employee deferral if age 50+.",
    "Employer match rate under the Match formula — dollar-for-dollar up to this rate, capped by what the employee actually deferred. Commonly 3%, but may be reduced to as low as 1% in up to 2 of any 5 years.",
    "Flat employer contribution rate under the Nonelective formula, paid to the employee regardless of their own deferral. Commonly 2%.",
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
    "A flat dollar tax — never a percentage of wages — that some PA municipalities levy on those who work there. It carries a statutory yearly cap (currently $52 total per employer per year), collected in even per-pay installments rather than all at once. Does not reduce taxable wages; withheld from the paycheck and remitted to the local taxing authority. Enter the per-pay amount, or 0 if none applies.",
    "Sum of employee withholding.",
    "The elected deferral, capped by the deferral limit (plus catch-up if eligible) and by remaining room under the combined limit.",
    "The elected deferral, capped by the SIMPLE IRA deferral limit (plus catch-up if eligible).",
    "Gross wages − employee withholding − any Solo 401(k) or SIMPLE IRA employee deferral.",
    "Subject to annual wage base.",
    "Gross wages × Medicare rate.",
    "Subject to state wage base.",
    "Gross wages × employer rate.",
    "Gross wages × other state rate.",
    "Subject to FUTA wage base.",
    "Gross wages × SEP rate, capped annually.",
    "Gross wages × employer profit-sharing rate, capped by remaining room under the combined limit.",
    "Match formula: dollar-for-dollar match of the employee deferral, up to the match rate. Nonelective formula: flat rate × gross wages, regardless of the employee's own deferral.",
    "Net paycheck + all deposits + SEP + Solo 401(k) + SIMPLE IRA contributions.",
    "Beginning cash + receipts − obligations − expenses.",
    "Cash after obligations − minimum reserve.",
    "Available cash ÷ client receipts.",
    "Approximate threshold lookup (see the thresholds table above).",
    "Total payroll cash requirement ÷ gross wages — a personal-interest ratio, not used elsewhere in the app.",
    "Net paycheck ÷ gross wages — a personal-interest ratio, not used elsewhere in the app."
  ),
  "Source Info" = c(
    "User input", "User input", "Client contract", "Owner compensation policy",
    "User input", "Calculated", "Bank balance", "User input", "User input", "Owner policy",
    "Payroll records", "User input", "User input", "Owner policy", "Retirement records",
    "Owner policy", "User input", "User input", "Retirement records", "Retirement records",
    "User input", "User input", "User input", "Retirement records",
    "User input",
    "Replace with accountant's actual withholding when available.",
    "IRS Publication 15", "IRS Publication 15", "SSA annual wage base",
    "IRS Publication 15", "IRS Publication 15", "IRS Topic 560", "IRS Topic 560",
    "Enter local current rate.", "Enter zero if none applies.",
    "Enter zero if none applies.", "Use assigned employer rate.",
    "Enter current state wage base.", "Enter zero if none applies.",
    "Enter zero if none applies.", "Enter zero if none applies.",
    "Assumes full state credit.", "IRS Topic 759", "IRS annual limit",
    "IRS annual limit", "IRS annual limit", "IRS annual limit",
    "IRS annual limit", "IRS annual limit", "IRS Publication 15", "IRS Publication 15",
    "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Municipal LST — enter your local amount (up to ~$52/yr).", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated", "Calculated", "Calculated", "Calculated", "Calculated",
    "Calculated"
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
    "Retirement Plan" = inputs$retirement_plan_type,
    "Month" = if (length(inputs$planning_month) == 0 || is.na(inputs$planning_month)) "" else format(inputs$planning_month, "%Y-%m"),
    "Planned Billable Hours" = inputs$billable_hours,
    "Billing Rate ($/hr)" = inputs$billing_rate,
    "Wage Rate ($/hr)" = inputs$wage_rate,
    "Expected Billed Revenue ($)" = results$expected_billed_revenue,
    "Additional Receipts ($)" = inputs$additional_receipts,
    "Expected Client Receipts ($)" = results$client_receipts,
    "Beginning LLC Cash ($)" = inputs$beginning_cash,
    "Gross Wages ($)" = results$gross_wages,
    "Federal Withholding Rate" = tax$fed_wh_rate,
    "Voluntary Additional Federal Withholding ($)" = inputs$additional_fed_withholding,
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
    "Local Services Tax (LST) ($)" = results$lst,
    "Total Employee Withholding ($)" = results$total_ee_withholding,
    "Retirement Employee Deferral ($)" = results$retirement_employee_deferral,
    "Net Paycheck ($)" = results$net_paycheck,
    "Employer Social Security ($)" = results$er_ss,
    "Employer Medicare ($)" = results$er_medicare,
    "Employer State UI ($)" = results$er_sui,
    "Employer Leave / Disability ($)" = results$er_leave,
    "Other State Payroll Tax ($)" = results$other_state_er,
    "FUTA ($)" = results$futa,
    "Retirement Employer Formula" = if (identical(inputs$retirement_plan_type, "SIMPLE IRA")) inputs$simple_employer_formula else "",
    "Retirement Employer Contribution ($)" = results$retirement_employer_contribution,
    "Federal Payroll Tax Deposit ($)" = results$federal_deposit,
    "State Withholding Deposit ($)" = results$state_wh_deposit,
    "Local Tax Deposit ($)" = results$local_deposit,
    "Local Services Tax Deposit ($)" = results$lst_deposit,
    "State UI Deposit ($)" = results$sui_deposit,
    "State Leave / Disability Deposit ($)" = results$leave_deposit,
    "Other State Payroll Deposit ($)" = results$other_state_deposit,
    "FUTA Reserve ($)" = results$futa_reserve,
    "Retirement Reserve ($)" = results$retirement_reserve,
    "Total Payroll Cash Requirement ($)" = results$total_payroll_cash_requirement,
    "Other Operating Expenses ($)" = inputs$other_opex,
    "Payroll Service Fees ($)" = inputs$payroll_fees,
    "Minimum Cash Reserve ($)" = inputs$min_cash_reserve,
    "Cash After Obligations ($)" = results$cash_after_obligations,
    "Available Cash ($)" = results$available_cash,
    "Available Cash Margin" = results$available_cash_margin,
    "Cash Health Status" = results$health_status,
    "Total Payroll Cash Requirement / Gross Wages" = results$cash_requirement_to_gross_ratio,
    "Net Pay / Gross Pay" = results$net_pay_to_gross_ratio,
    check.names = FALSE
  )
}
