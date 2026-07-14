# User Guide

## Purpose

The planner answers one operational question:

> After paying the owner-employee and funding every payroll-related obligation, how much business cash is actually available?

## Section 1 — Inputs

### Planned billable hours

The number of hours expected to be billed for the planning month.

### Billing rate

The amount charged to the client per billable hour.

### Wage rate

The W-2 hourly compensation paid to the owner-employee. The planner does not determine whether compensation is reasonable under tax law; that decision belongs with the owner and tax adviser.

### Expected client receipts

Cash expected to be received during the month. This can differ from billed revenue when invoicing and collection occur in different periods.

### Beginning LLC cash

Cash available before the current month's receipts and payroll obligations.

### Minimum operating cash reserve

Cash the owner intends to preserve after all payroll, tax, retirement, and operating obligations.

### SEP contribution rate

For a traditional SEP-IRA, the modeled contribution is an employer contribution. Employee elective salary deferrals are not part of a traditional SEP-IRA.

## Section 2 — Default Tax Rates and Limits

These assumptions are user-maintained. The workbook intentionally does not select rates by state or locality.

Users should confirm:

- federal withholding assumptions;
- Social Security and Medicare rates and annual limits;
- state income-tax withholding;
- local wage, income, or occupational taxes;
- employee and employer unemployment rates;
- paid-leave or disability contributions;
- other state payroll assessments;
- FUTA rate and wage base; and
- SEP annual contribution limit.

## Section 3 — Employee Payroll Results

This section calculates employee withholding and ends with **Net Employee Paycheck**, making the employee result visually distinct.

## Section 4 — Employer Obligations and Cash Planning

This section calculates employer payroll costs, tax deposits, retirement funding, total payroll cash requirement, and remaining cash.

## Cash Health Status

The status is based on:

```text
Available Cash ÷ Expected Client Receipts
```

The visible lookup table defines the thresholds. Users may change the thresholds to reflect their own cash-risk tolerance.

## Copy-Ready Payroll Snapshot

The horizontal snapshot contains model assumptions and results in a stable column order. It is intended to become the source for a future value-only payroll register. Historical rows should contain numbers, not formulas, so later model changes do not rewrite history.
