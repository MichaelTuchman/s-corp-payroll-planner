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

ui <- page_sidebar(
  title = paste("Solo S-Corp Payroll & Cash Planner —", app_version),
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 380,
    accordion(
      open = "Inputs",
      accordion_panel(
        "Inputs", icon = bsicons::bs_icon("pencil-square"),
        dateInput("planning_month", "Planning month", value = "2026-07-01"),
        numericInput("billable_hours", "Planned billable hours", value = 157, min = 0),
        numericInput("billing_rate", "Billing rate ($/hour)", value = 100, min = 0),
        numericInput("wage_rate", "Wage rate ($/hour)", value = 50, min = 0),
        numericInput("client_receipts", "Expected client receipts ($)", value = 15700, min = 0),
        numericInput("beginning_cash", "Beginning LLC cash ($)", value = 0),
        numericInput("other_opex", "Other operating expenses ($)", value = 0, min = 0),
        numericInput("payroll_fees", "Payroll service fees ($)", value = 0, min = 0),
        numericInput("min_cash_reserve", "Minimum operating cash reserve ($)", value = 0, min = 0),
        numericInput("sep_rate", "SEP contribution rate (%)", value = 0, min = 0, max = 100, step = 0.5),
        numericInput("ytd_wages", "YTD wages before this payroll ($)", value = 0, min = 0),
        numericInput("ytd_sep", "YTD SEP contributions before this payroll ($)", value = 0, min = 0),
        textInput("notes", "Notes", value = "July planning scenario")
      ),
      accordion_panel(
        "Default Tax Rates and Limits", icon = bsicons::bs_icon("sliders"),
        numericInput("fed_wh_rate", "Federal withholding planning rate (%)", value = 24, min = 0, max = 100, step = 0.1),
        numericInput("ee_ss_rate", "Employee Social Security rate (%)", value = 6.2, min = 0, max = 100, step = 0.01),
        numericInput("er_ss_rate", "Employer Social Security rate (%)", value = 6.2, min = 0, max = 100, step = 0.01),
        numericInput("ss_wage_base", "Social Security wage base ($ annual)", value = 184500, min = 0),
        numericInput("ee_medicare_rate", "Employee Medicare rate (%)", value = 1.45, min = 0, max = 100, step = 0.01),
        numericInput("er_medicare_rate", "Employer Medicare rate (%)", value = 1.45, min = 0, max = 100, step = 0.01),
        numericInput("add_medicare_rate", "Additional Medicare rate (%)", value = 0.9, min = 0, max = 100, step = 0.01),
        numericInput("add_medicare_threshold", "Additional Medicare threshold ($ annual)", value = 200000, min = 0),
        numericInput("state_income_tax_rate", "State income-tax rate (%)", value = 6, min = 0, max = 100, step = 0.01),
        numericInput("local_tax_rate", "Local income / occupational tax rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("ee_sui_rate", "Employee state unemployment rate (%)", value = 0.07, min = 0, max = 100, step = 0.01),
        numericInput("er_sui_rate", "Employer state unemployment rate (%)", value = 6.5, min = 0, max = 100, step = 0.01),
        numericInput("sui_wage_base", "State unemployment wage base ($ annual)", value = 10000, min = 0),
        numericInput("ee_leave_rate", "Employee leave / disability rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("er_leave_rate", "Employer leave / disability rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("other_state_er_rate", "Other state payroll-tax rate (%)", value = 0, min = 0, max = 100, step = 0.01),
        numericInput("futa_rate", "FUTA rate (%)", value = 0.6, min = 0, max = 100, step = 0.01),
        numericInput("futa_wage_base", "FUTA wage base ($ annual)", value = 7000, min = 0),
        numericInput("sep_annual_limit", "SEP annual contribution limit ($)", value = 72000, min = 0)
      )
    )
  ),

  div(
    class = "alert alert-warning",
    strong("DISCLAIMER: "), "Use for planning purposes only. Tax and payroll filing should be performed only by a professional.",
    br(),
    "One S corporation. One W-2 employee, who is also the owner. One payroll scenario at a time. Not payroll processing or tax-return preparation."
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
    div(style = "overflow-x: auto;", tableOutput("snapshot_table")),
    downloadButton("download_snapshot", "Download snapshot (CSV)")
  )
)

server <- function(input, output, session) {

  inputs <- reactive({
    list(
      planning_month = input$planning_month,
      billable_hours = input$billable_hours,
      billing_rate = input$billing_rate,
      wage_rate = input$wage_rate,
      client_receipts = input$client_receipts,
      beginning_cash = input$beginning_cash,
      other_opex = input$other_opex,
      payroll_fees = input$payroll_fees,
      min_cash_reserve = input$min_cash_reserve,
      sep_rate = input$sep_rate / 100,
      ytd_wages = input$ytd_wages,
      ytd_sep = input$ytd_sep,
      notes = input$notes
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
      sep_annual_limit = input$sep_annual_limit
    )
  })

  results <- reactive({
    calculate_planner(inputs(), tax())
  })

  output$available_cash_out <- renderText(money(results()$available_cash))
  output$margin_out <- renderText(pct(results()$available_cash_margin))

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
        "Employee Medicare", "Additional Medicare", "State income tax",
        "Local income / occupational tax", "Employee state unemployment",
        "Employee leave / disability", "Total employee withholding", "Net employee paycheck"
      ),
      Amount = c(
        money(r$gross_wages), money(r$fed_withholding), money(r$ee_ss),
        money(r$ee_medicare), money(r$add_medicare), money(r$state_income_tax),
        money(r$local_tax), money(r$ee_sui), money(r$ee_leave),
        money(r$total_ee_withholding), money(r$net_paycheck)
      )
    )
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  output$employer_results <- renderTable({
    r <- results()
    data.frame(
      Item = c(
        "Employer Social Security", "Employer Medicare", "Employer state unemployment",
        "Employer leave / disability", "Other state payroll tax", "FUTA", "SEP contribution",
        "Total payroll cash requirement", "Cash after all obligations", "Available cash"
      ),
      Amount = c(
        money(r$er_ss), money(r$er_medicare), money(r$er_sui),
        money(r$er_leave), money(r$other_state_er), money(r$futa), money(r$sep_contribution),
        money(r$total_payroll_cash_requirement), money(r$cash_after_obligations), money(r$available_cash)
      )
    )
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  snapshot <- reactive({
    build_snapshot_row(inputs(), tax(), results())
  })

  output$snapshot_table <- renderTable({
    snapshot()
  }, striped = TRUE, bordered = TRUE, colnames = TRUE)

  output$download_snapshot <- downloadHandler(
    filename = function() {
      paste0("payroll_snapshot_", format(input$planning_month, "%Y-%m"), ".csv")
    },
    content = function(file) {
      write.csv(snapshot(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
