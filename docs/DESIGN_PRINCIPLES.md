# Design Principles

## One owner, one employee, one scenario

The product is deliberately narrow: a single-owner S corporation with the owner as the only W-2 employee.

## Planning first

The Planner is the product's primary interface. Historical recording is secondary.

## Inputs are explicit

Blue cells are entered by the user. Calculations remain visible and traceable.

## Actual payroll is authoritative

The workbook estimates payroll for planning. The accountant or payroll provider's output governs the actual payroll and deposits.

## History should not recalculate

A future payroll register will store copied values, not formulas. Model improvements must not rewrite prior payroll history.

## Cash on hand is not available cash

The planner distinguishes business cash from amounts already committed to payroll, taxes, retirement, expenses, and operating reserves.

## No premature generalization

Users enter their own state and local assumptions. The product does not attempt to maintain a nationwide payroll-tax engine.
