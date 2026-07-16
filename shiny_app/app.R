library(shiny)
library(bslib)
library(bsicons)
library(scales)

source("R/calculations.R")

app_version <- trimws(readLines("VERSION", warn = FALSE)[1])

status_theme <- function(status) {
  switch(status,
    "DEFICIT" = "danger",
    "DIRE WARNING" = "danger",
    "TOO CLOSE" = "warning",
    "OK" = "info",
    "SAFE" = "success",
    "GREAT" = "success",
    "secondary"
  )
}

money <- function(x) dollar(x, accuracy = 0.01)
pct <- function(x) percent(x, accuracy = 0.01)

# First day of the month the app is opened — the most reasonable default
# planning month, used both for the initial value and the Reset button.
first_of_current_month <- function() {
  as.Date(format(Sys.Date(), "%Y-%m-01"))
}

# Every numeric input feeds the calculation chain; a blank (NA) value in any
# of them propagates to an NA index in the health-status lookup, where
# if (idx < 1) throws "missing value where TRUE/FALSE needed" and Shiny
# shows its generic red error box. Validating here shows a clear message
# instead, for whichever field the user blanked.
numeric_field_labels <- c(
  billable_hours = "Planned Billable Hours",
  billing_rate = "Billing Rate",
  additional_receipts = "Additional Receipts",
  beginning_cash = "Beginning LLC Cash",
  other_opex = "Other Operating Expenses",
  payroll_fees = "Payroll Service Fees",
  min_cash_reserve = "Minimum Operating Cash Reserve",
  ytd_wages = "YTD Wages Before This Payroll",
  additional_fed_withholding = "Voluntary Additional Federal Withholding",
  fed_wh_rate = "Federal Withholding Planning Rate",
  ee_ss_rate = "Employee Social Security Rate",
  er_ss_rate = "Employer Social Security Rate",
  ss_wage_base = "Social Security Wage Base",
  ee_medicare_rate = "Employee Medicare Rate",
  er_medicare_rate = "Employer Medicare Rate",
  add_medicare_rate = "Additional Medicare Rate",
  add_medicare_threshold = "Additional Medicare Threshold",
  state_income_tax_rate = "State Income-Tax Rate",
  local_tax_rate = "Local Income / Occupational Tax Rate",
  ee_sui_rate = "Employee State Unemployment Rate",
  er_sui_rate = "Employer State Unemployment Rate",
  sui_wage_base = "State Unemployment Wage Base",
  ee_leave_rate = "Employee Leave / Disability Rate",
  er_leave_rate = "Employer Leave / Disability Rate",
  other_state_er_rate = "Other State Payroll-Tax Rate",
  futa_rate = "FUTA Rate",
  futa_wage_base = "FUTA Wage Base"
)

# Validated only when their retirement plan is actually selected — a field
# hidden by conditionalPanel() still exists in `input` with whatever value
# it last had, so validating it unconditionally would block computation
# over a blank the user can no longer even see.
sep_field_labels <- c(
  ytd_sep = "YTD SEP Contributions Before This Payroll",
  sep_annual_limit = "SEP Annual Contribution Limit"
)
solo401k_field_labels <- c(
  solo401k_deferral_election = "Solo 401(k) Employee Elective Deferral",
  ytd_solo401k_deferral = "YTD Solo 401(k) Employee Deferrals",
  ytd_solo401k_employer = "YTD Solo 401(k) Employer Contributions",
  solo401k_deferral_limit = "Solo 401(k) Employee Deferral Limit",
  solo401k_catchup_limit = "Solo 401(k) Catch-Up Limit",
  solo401k_combined_limit = "Solo 401(k) Combined Contribution Limit"
)
simple_field_labels <- c(
  simple_deferral_election = "SIMPLE IRA Employee Elective Deferral",
  ytd_simple_deferral = "YTD SIMPLE IRA Employee Deferrals",
  simple_deferral_limit = "SIMPLE IRA Employee Deferral Limit",
  simple_catchup_limit = "SIMPLE IRA Catch-Up Limit",
  simple_match_rate = "SIMPLE IRA Employer Match Rate",
  simple_nonelective_rate = "SIMPLE IRA Employer Nonelective Rate"
)

ui <- page_sidebar(
  title = paste("Solo S-Corp Payroll & Cash Planner — Version", app_version),
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  tags$style(HTML("
    .sidebar .accordion-button, .sidebar .accordion-button:not(.collapsed), .sidebar .accordion-body {
      background-color: #e3f2fd;
    }
  ")),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('copyToClipboard', function(msg) {
      var btn = document.getElementById('copy_snapshot');
      if (msg.error) { alert(msg.error); return; }
      navigator.clipboard.writeText(msg.text).then(function() {
        if (!btn) return;
        var original = btn.innerHTML;
        btn.innerHTML = 'Copied!';
        setTimeout(function() { btn.innerHTML = original; }, 1500);
      }).catch(function(err) {
        alert('Could not copy to clipboard: ' + err);
      });
    });
  ")),

  sidebar = sidebar(
    width = 380,
    accordion(
      open = "Inputs",
      accordion_panel(
        "Inputs", icon = bsicons::bs_icon("pencil-square"),
        div(
          actionButton("reset_inputs", "Reset inputs", icon = bsicons::bs_icon("arrow-counterclockwise"), class = "btn-outline-secondary btn-sm"),
          style = "margin-bottom: 12px;"
        ),
        textInput("scenario_name", "Scenario name", value = "July planning scenario"),
        dateInput("planning_month", "Planning month", value = first_of_current_month()),
        numericInput("billable_hours", "Planned billable hours", value = 157, min = 0),
        numericInput("billing_rate", "Billing rate ($/hour)", value = 100, min = 0),
        sliderInput("wage_rate", "Wage rate ($/hour) — cannot exceed billing rate; slide to see the Cash Health Status change", value = 50, min = 0, max = 100, step = 1),
        numericInput("additional_receipts", "Additional receipts ($) (beyond rate × hours — e.g. prior-month collections, retainers, advances; can be negative)", value = 0),
        div(style = "margin-top: -10px; margin-bottom: 15px; color: #495057;", "Expected client receipts: ", strong(textOutput("expected_receipts_preview", inline = TRUE))),
        selectInput("retirement_plan_type", "Retirement plan", choices = c("None", "SEP-IRA", "Solo 401(k)", "SIMPLE IRA"), selected = "None"),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'SEP-IRA'",
          sliderInput("sep_rate", "SEP contribution rate (%) (retirement) — slide to see the Cash Health Status change", value = 0, min = 0, max = 25, step = 0.5),
          numericInput("ytd_sep", "YTD SEP contributions before this payroll ($)", value = 0, min = 0)
        ),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'Solo 401(k)'",
          checkboxInput("solo401k_catchup_eligible", "Age 50+ (catch-up eligible)", value = FALSE),
          numericInput("ytd_solo401k_deferral", "YTD employee elective deferrals before this payroll ($)", value = 0, min = 0),
          numericInput("ytd_solo401k_employer", "YTD Solo 401(k) employer contributions before this payroll ($)", value = 0, min = 0),
          numericInput("solo401k_deferral_election", "Employee pre-tax deferral this payroll ($)", value = 0, min = 0),
          uiOutput("solo401k_deferral_room_ui"),
          sliderInput("solo401k_employer_rate", "Solo 401(k) employer profit-sharing rate (%) — slide to see the Cash Health Status change", value = 0, min = 0, max = 25, step = 0.5),
          uiOutput("solo401k_employer_room_ui")
        ),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'SIMPLE IRA'",
          selectInput("simple_employer_formula", "Employer contribution formula (choose one — the law only allows these two)", choices = c("3% Match", "2% Nonelective"), selected = "3% Match"),
          checkboxInput("simple_catchup_eligible", "Age 50+ (catch-up eligible)", value = FALSE),
          numericInput("ytd_simple_deferral", "YTD employee elective deferrals before this payroll ($)", value = 0, min = 0),
          numericInput("simple_deferral_election", "Employee pre-tax deferral this payroll ($)", value = 0, min = 0),
          uiOutput("simple_deferral_room_ui")
        )
      ),
      accordion_panel(
        "Cash, Expenses & History", icon = bsicons::bs_icon("wallet2"),
        numericInput("beginning_cash", "Beginning LLC cash ($)", value = 0),
        numericInput("other_opex", "Other operating expenses ($)", value = 0, min = 0),
        numericInput("payroll_fees", "Payroll service fees ($)", value = 0, min = 0),
        numericInput("min_cash_reserve", "Minimum operating cash reserve ($)", value = 0, min = 0),
        numericInput("ytd_wages", "YTD wages before this payroll ($)", value = 0, min = 0),
        numericInput("additional_fed_withholding", "Voluntary additional federal withholding ($) (flat amount, beyond the standard rate calculation — Form W-4 Step 4(c))", value = 0, min = 0)
      ),
      accordion_panel(
        "Default Tax Rates and Limits", icon = bsicons::bs_icon("sliders"),
        numericInput("fed_wh_rate", "Federal withholding planning rate (%)", value = 24, min = 0, max = 100, step = 0.1),
        numericInput("ee_ss_rate", "Employee Social Security rate (%)", value = 6.2, min = 0, max = 100, step = 0.01),
        numericInput("er_ss_rate", "Employer Social Security rate (%)", value = 6.2, min = 0, max = 100, step = 0.01),
        numericInput("ss_wage_base", "Social Security wage base ($ annual)", value = 184500, min = 0),
        numericInput("ee_medicare_rate", "Employee Medicare rate (%)", value = 1.45, min = 0, max = 100, step = 0.01),
        numericInput("er_medicare_rate", "Employer Medicare rate (%)", value = 1.45, min = 0, max = 100, step = 0.01),
        numericInput("add_medicare_rate", "Additional Medicare rate (%) (surtax above threshold)", value = 0.9, min = 0, max = 100, step = 0.01),
        numericInput("add_medicare_threshold", "Additional Medicare threshold ($ annual)", value = 200000, min = 0),
        numericInput("state_income_tax_rate", "State income-tax rate (%)", value = 3.07, min = 0, max = 100, step = 0.01),
        numericInput("local_tax_rate", "Local income / occupational tax rate (%)", value = 1.65, min = 0, max = 100, step = 0.01),
        numericInput("ee_sui_rate", "Employee state unemployment rate (%)", value = 0.07, min = 0, max = 100, step = 0.01),
        numericInput("er_sui_rate", "Employer state unemployment rate (%)", value = 6.5, min = 0, max = 100, step = 0.01),
        numericInput("sui_wage_base", "State unemployment wage base ($ annual)", value = 10000, min = 0),
        numericInput("ee_leave_rate", "Employee leave / disability rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("er_leave_rate", "Employer leave / disability rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("other_state_er_rate", "Other state payroll-tax rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("futa_rate", "FUTA rate (%) (federal unemployment tax)", value = 0.6, min = 0, max = 100, step = 0.01),
        numericInput("futa_wage_base", "FUTA wage base ($ annual)", value = 7000, min = 0),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'SEP-IRA'",
          numericInput("sep_annual_limit", "SEP annual contribution limit ($) (retirement)", value = 72000, min = 0)
        ),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'Solo 401(k)'",
          numericInput("solo401k_deferral_limit", "Solo 401(k) employee deferral limit ($ annual)", value = 23500, min = 0),
          numericInput("solo401k_catchup_limit", "Solo 401(k) catch-up limit ($ annual, age 50+)", value = 7500, min = 0),
          numericInput("solo401k_combined_limit", "Solo 401(k) combined employee + employer limit ($ annual)", value = 70000, min = 0)
        ),
        conditionalPanel(
          condition = "input.retirement_plan_type == 'SIMPLE IRA'",
          numericInput("simple_deferral_limit", "SIMPLE IRA employee deferral limit ($ annual) (placeholder — verify current figure)", value = 16000, min = 0),
          numericInput("simple_catchup_limit", "SIMPLE IRA catch-up limit ($ annual, age 50+) (placeholder — verify current figure)", value = 3500, min = 0),
          numericInput("simple_match_rate", "SIMPLE IRA employer match rate (%) (\"3% Match\" formula)", value = 3, min = 0, max = 100, step = 0.5),
          numericInput("simple_nonelective_rate", "SIMPLE IRA employer nonelective rate (%) (\"2% Nonelective\" formula)", value = 2, min = 0, max = 100, step = 0.5)
        )
      )
    )
  ),

  div(
    style = "text-align: right;",
    actionButton("show_glossary", "Glossary", icon = bsicons::bs_icon("book"), class = "btn-outline-secondary btn-sm")
  ),

  div(
    class = "alert alert-light border",
    div(style = "color: firebrick;", strong("DISCLAIMER: "), "Use for planning purposes only. Tax and payroll filing should be performed only by a professional."),
    div(style = "color: #212529;", "One S corporation. One W-2 employee, who is also the owner. One payroll scenario at a time. Not payroll processing or tax-return preparation.")
  ),

  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Available cash",
      value = textOutput("available_cash_out", inline = TRUE),
      showcase = bsicons::bs_icon("cash-coin")
    ),
    value_box(
      title = "Available cash margin",
      value = textOutput("margin_out", inline = TRUE),
      showcase = bsicons::bs_icon("percent")
    ),
    uiOutput("status_box")
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Section 3 — Employee Payroll Results"),
      tableOutput("employee_results")
    ),
    card(
      card_header("Section 4 — Employer Obligations and Cash Planning"),
      tableOutput("employer_results")
    )
  ),

  card(
    card_header("Copy-Ready Payroll Snapshot"),
    p("Dial in a scenario above, then add it to the table below. Repeat for as many scenarios as you like, then download them all at once."),
    div(
      actionButton("add_snapshot", "Add this scenario to the table", icon = bsicons::bs_icon("plus-square"), class = "btn-primary"),
      actionButton("clear_snapshots", "Clear table", icon = bsicons::bs_icon("trash"), class = "btn-outline-secondary"),
      style = "margin-bottom: 10px;"
    ),
    textOutput("snapshot_count"),
    div(style = "overflow-x: auto;", tableOutput("snapshot_table")),
    p(
      em("Rate columns are rounded to 2 decimal places on screen for readability (e.g. 3.07% shows as 0.03) — the downloaded CSV and clipboard copy both retain full precision (0.0307)."),
      style = "font-size: 0.85em; color: #495057;"
    ),
    div(
      downloadButton("download_snapshot", "Download snapshot table (CSV)"),
      actionButton("copy_snapshot", "Copy to clipboard", icon = bsicons::bs_icon("clipboard"), class = "btn-outline-secondary"),
      style = "margin-top: 10px;"
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$show_glossary, {
    showModal(modalDialog(
      title = "Glossary",
      div(
        style = "max-height: 60vh; overflow-y: auto;",
        strong("Cash Health Status thresholds"),
        p("Fixed by the app, not user-editable — the available cash margin is looked up against these bands:"),
        tableOutput("thresholds_table"),
        hr(),
        strong("Term definitions"),
        tableOutput("glossary_table")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  })

  output$glossary_table <- renderTable(glossary, striped = TRUE, bordered = TRUE, colnames = TRUE)

  output$thresholds_table <- renderTable({
    t <- health_status_table
    n <- nrow(t)
    ranges <- character(n)
    ranges[1] <- paste("Below", pct(t$threshold[2]))
    if (n > 2) {
      for (i in 2:(n - 1)) {
        ranges[i] <- paste(pct(t$threshold[i]), "to under", pct(t$threshold[i + 1]))
      }
    }
    ranges[n] <- paste(pct(t$threshold[n]), "or more")
    data.frame("Available Cash Margin" = ranges, "Status" = t$status, check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  # Wage rate must never exceed billing rate, so the slider's max is tied
  # directly to it (wage rate == billing rate is allowed).
  observeEvent(input$billing_rate, {
    req(!is.na(input$billing_rate))
    new_max <- max(0, round(input$billing_rate, 2))
    new_value <- if (is.na(input$wage_rate) || input$wage_rate > new_max) new_max else input$wage_rate
    updateSliderInput(session, "wage_rate", max = new_max, value = new_value)
  })

  # "Reset inputs" only touches the Inputs panel, never Default Tax Rates and
  # Limits. Billing rate is deliberately left alone (last time is a good
  # point of departure); wage rate resets relative to whatever billing rate
  # currently is, not a fixed number.
  observeEvent(input$reset_inputs, {
    updateTextInput(session, "scenario_name", value = "")
    updateDateInput(session, "planning_month", value = first_of_current_month())
    updateNumericInput(session, "billable_hours", value = 160)
    billing_rate <- if (is.na(input$billing_rate)) 0 else input$billing_rate
    updateSliderInput(session, "wage_rate", value = round(billing_rate * 0.6, 2))
    updateNumericInput(session, "additional_receipts", value = 0)
    updateNumericInput(session, "beginning_cash", value = 0)
    updateNumericInput(session, "other_opex", value = 0)
    updateNumericInput(session, "payroll_fees", value = 0)
    updateNumericInput(session, "min_cash_reserve", value = 0)
    updateNumericInput(session, "ytd_wages", value = 0)
    updateNumericInput(session, "additional_fed_withholding", value = 0)
    updateSelectInput(session, "retirement_plan_type", selected = "None")
    updateSliderInput(session, "sep_rate", value = 0)
    updateNumericInput(session, "ytd_sep", value = 0)
    updateSliderInput(session, "solo401k_employer_rate", value = 0)
    updateNumericInput(session, "solo401k_deferral_election", value = 0)
    updateCheckboxInput(session, "solo401k_catchup_eligible", value = FALSE)
    updateNumericInput(session, "ytd_solo401k_deferral", value = 0)
    updateNumericInput(session, "ytd_solo401k_employer", value = 0)
    updateSelectInput(session, "simple_employer_formula", selected = "3% Match")
    updateNumericInput(session, "simple_deferral_election", value = 0)
    updateCheckboxInput(session, "simple_catchup_eligible", value = FALSE)
    updateNumericInput(session, "ytd_simple_deferral", value = 0)
  })

  inputs <- reactive({
    list(
      planning_month = input$planning_month,
      billable_hours = input$billable_hours,
      billing_rate = input$billing_rate,
      wage_rate = input$wage_rate,
      additional_receipts = input$additional_receipts,
      beginning_cash = input$beginning_cash,
      other_opex = input$other_opex,
      payroll_fees = input$payroll_fees,
      min_cash_reserve = input$min_cash_reserve,
      ytd_wages = input$ytd_wages,
      additional_fed_withholding = input$additional_fed_withholding,
      retirement_plan_type = input$retirement_plan_type,
      sep_rate = input$sep_rate / 100,
      ytd_sep = input$ytd_sep,
      solo401k_employer_rate = input$solo401k_employer_rate / 100,
      solo401k_deferral_election = input$solo401k_deferral_election,
      solo401k_catchup_eligible = input$solo401k_catchup_eligible,
      ytd_solo401k_deferral = input$ytd_solo401k_deferral,
      ytd_solo401k_employer = input$ytd_solo401k_employer,
      simple_employer_formula = input$simple_employer_formula,
      simple_deferral_election = input$simple_deferral_election,
      simple_catchup_eligible = input$simple_catchup_eligible,
      ytd_simple_deferral = input$ytd_simple_deferral,
      scenario_name = input$scenario_name
    )
  })

  tax <- reactive({
    list(
      fed_wh_rate = input$fed_wh_rate / 100,
      ee_ss_rate = input$ee_ss_rate / 100,
      er_ss_rate = input$er_ss_rate / 100,
      ss_wage_base = input$ss_wage_base,
      ee_medicare_rate = input$ee_medicare_rate / 100,
      er_medicare_rate = input$er_medicare_rate / 100,
      add_medicare_rate = input$add_medicare_rate / 100,
      add_medicare_threshold = input$add_medicare_threshold,
      state_income_tax_rate = input$state_income_tax_rate / 100,
      local_tax_rate = input$local_tax_rate / 100,
      ee_sui_rate = input$ee_sui_rate / 100,
      er_sui_rate = input$er_sui_rate / 100,
      sui_wage_base = input$sui_wage_base,
      ee_leave_rate = input$ee_leave_rate / 100,
      er_leave_rate = input$er_leave_rate / 100,
      other_state_er_rate = input$other_state_er_rate / 100,
      futa_rate = input$futa_rate / 100,
      futa_wage_base = input$futa_wage_base,
      sep_annual_limit = input$sep_annual_limit,
      solo401k_deferral_limit = input$solo401k_deferral_limit,
      solo401k_catchup_limit = input$solo401k_catchup_limit,
      solo401k_combined_limit = input$solo401k_combined_limit,
      simple_deferral_limit = input$simple_deferral_limit,
      simple_catchup_limit = input$simple_catchup_limit,
      simple_match_rate = input$simple_match_rate / 100,
      simple_nonelective_rate = input$simple_nonelective_rate / 100
    )
  })

  results <- reactive({
    active_field_labels <- c(
      numeric_field_labels,
      if (identical(input$retirement_plan_type, "SEP-IRA")) sep_field_labels,
      if (identical(input$retirement_plan_type, "Solo 401(k)")) solo401k_field_labels,
      if (identical(input$retirement_plan_type, "SIMPLE IRA")) simple_field_labels
    )
    for (field_id in names(active_field_labels)) {
      validate(need(!is.na(input[[field_id]]), paste(active_field_labels[[field_id]], "should not be blank")))
    }
    calculate_planner(inputs(), tax())
  })

  output$available_cash_out <- renderText(money(results()$available_cash))
  output$margin_out <- renderText(pct(results()$available_cash_margin))
  output$expected_receipts_preview <- renderText(money(results()$client_receipts))

  room_note <- function(room, label, maxed_out_text) {
    if (room <= 0) {
      div(class = "text-danger", style = "font-size: 0.85em; margin-top: -10px; margin-bottom: 15px;", maxed_out_text)
    } else {
      div(style = "font-size: 0.85em; color: #495057; margin-top: -10px; margin-bottom: 15px;", label, strong(money(room)))
    }
  }

  output$solo401k_deferral_room_ui <- renderUI({
    room_note(
      results()$solo401k_deferral_room,
      "Deferral room remaining this year: ",
      "You've used your full employee deferral room for the year."
    )
  })

  output$simple_deferral_room_ui <- renderUI({
    room_note(
      results()$simple_deferral_room,
      "Deferral room remaining this year: ",
      "You've used your full employee deferral room for the year."
    )
  })

  output$solo401k_employer_room_ui <- renderUI({
    room_note(
      results()$solo401k_employer_room,
      "Combined room remaining for the employer contribution: ",
      "You've used your full Solo 401(k) combined room for the year — no more employer contribution is possible."
    )
  })

  # Keeps the employer-rate slider's usable range tied to the actual dollar
  # room left under the combined limit (as a % of gross wages), so toggling
  # catch-up or entering YTD amounts visibly changes what's draggable,
  # instead of only showing up in a results-table number.
  observe({
    r <- results()
    gross <- inputs()$billable_hours * inputs()$wage_rate
    max_pct <- if (is.na(gross) || gross <= 0) 0 else min(25, round(r$solo401k_employer_room / gross * 100, 2))
    current <- input$solo401k_employer_rate
    new_value <- if (is.na(current) || current > max_pct) max_pct else current
    updateSliderInput(session, "solo401k_employer_rate", max = max_pct, value = new_value)
  })

  output$status_box <- renderUI({
    status <- results()$health_status
    value_box(
      title = "Cash Health Status",
      value = status,
      showcase = bsicons::bs_icon("heart-pulse"),
      theme = status_theme(status)
    )
  })

  output$employee_results <- renderTable({
    r <- results()
    data.frame(
      Item = c(
        "Gross W-2 wages", "Federal income tax withheld", "Employee Social Security",
        "Employee Medicare", "Additional Medicare (surtax above threshold)", "State income tax",
        "Local income / occupational tax", "Employee state unemployment",
        "Employee leave / disability", "Total employee withholding",
        "Solo 401(k) employee elective deferral", "SIMPLE IRA employee elective deferral", "Net employee paycheck"
      ),
      Amount = c(
        money(r$gross_wages), money(r$fed_withholding), money(r$ee_ss),
        money(r$ee_medicare), money(r$add_medicare), money(r$state_income_tax),
        money(r$local_tax), money(r$ee_sui), money(r$ee_leave),
        money(r$total_ee_withholding), money(r$solo401k_employee_deferral),
        money(r$simple_employee_deferral), money(r$net_paycheck)
      )
    )
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  output$employer_results <- renderTable({
    r <- results()
    data.frame(
      Item = c(
        "Employer Social Security", "Employer Medicare", "Employer state unemployment",
        "Employer leave / disability", "Other state payroll tax (catch-all)",
        "FUTA (federal unemployment tax)", "SEP contribution (retirement)",
        "Solo 401(k) employer contribution (retirement)", "SIMPLE IRA employer contribution (retirement)",
        "Total payroll cash requirement", "Cash after all obligations", "Available cash"
      ),
      Amount = c(
        money(r$er_ss), money(r$er_medicare), money(r$er_sui),
        money(r$er_leave), money(r$other_state_er), money(r$futa), money(r$sep_contribution),
        money(r$solo401k_employer_contribution), money(r$simple_employer_contribution),
        money(r$total_payroll_cash_requirement), money(r$cash_after_obligations), money(r$available_cash)
      )
    )
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  snapshot <- reactive({
    build_snapshot_row(inputs(), tax(), results())
  })

  # Captured scenarios accumulate in-session only: one row per click of
  # "Add this scenario", in click order, with no dedup or upsert-by-month.
  # This lives in browser-session memory, not on disk — download before
  # the tab closes or the session times out.
  captured_snapshots <- reactiveVal(NULL)

  observeEvent(input$add_snapshot, {
    existing <- captured_snapshots()
    captured_snapshots(if (is.null(existing)) snapshot() else rbind(existing, snapshot()))
  })

  observeEvent(input$clear_snapshots, {
    captured_snapshots(NULL)
  })

  output$snapshot_count <- renderText({
    n <- nrow(captured_snapshots())
    if (is.null(n) || n == 0) "No scenarios captured yet." else paste(n, "scenario(s) captured.")
  })

  output$snapshot_table <- renderTable({
    validate(need(!is.null(captured_snapshots()), "Add a scenario to see it here."))
    captured_snapshots()
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  output$download_snapshot <- downloadHandler(
    filename = function() "payroll_snapshots.csv",
    content = function(file) {
      write.csv(captured_snapshots(), file, row.names = FALSE)
    }
  )

  observeEvent(input$copy_snapshot, {
    data <- captured_snapshots()
    if (is.null(data)) {
      session$sendCustomMessage("copyToClipboard", list(error = "Add a scenario to the table before copying."))
      return()
    }
    tsv <- paste(capture.output(write.table(data, sep = "\t", row.names = FALSE, quote = FALSE)), collapse = "\n")
    session$sendCustomMessage("copyToClipboard", list(text = tsv))
  })
}

shinyApp(ui, server)
