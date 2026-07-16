# Attorney Briefing: Solo S-Corp Payroll & Cash Planner

This document summarizes what the product currently does, how it handles data, and what's already in place, to make an initial conversation with counsel more efficient. It is a factual product summary, not a legal analysis — the open questions at the end are what we're hoping counsel can help answer.

## What the product is

A web-based calculator that helps a one-owner S corporation (the owner is also the sole W-2 employee) model a single payroll scenario: expected revenue and payroll costs, estimated withholding and employer tax obligations, and how much business cash remains available afterward.

## Intended market

Solo S-corp owners directly (freelancers/consultants operating through a one-person S-corp), not accountants or bookkeepers — they already have better-suited professional tools. The owner may show it to their own accountant, but the accountant is not the target customer.

## Your mission :

Provide language and outline how I should position to hold me harmless from liability suits.

## Product boundary (by design)

The product is deliberately narrow. It is built for:

- one S corporation;
- one W-2 employee, who is also the owner;
- one payroll scenario at a time; and
- payroll and cash *planning*, not payroll processing or tax-return preparation.

Explicitly out of scope: multiple employees, multi-company support, actual payroll processing or tax filing, automated state/local tax-rate selection, general bookkeeping, and bank feeds.

## How it works

- **User inputs**: hours worked, billing rate, wage rate, cash on hand, operating expenses, and about 19 tax rates/wage-base assumptions (Social Security, Medicare, state/local rates, FUTA, SEP limits, etc.) that default to commonly-cited figures but are editable by the user.
- **Calculated outputs**: gross wages, itemized employee/employer withholding, total payroll cash requirement, and an "Available Cash Margin" that's compared against a fixed set of bands (DEFICIT / DIRE WARNING / TOO CLOSE / OK / SAFE / GREAT) to produce a "Cash Health Status."
- The status thresholds are hardcoded by the app, not user-editable — they're a product policy, not a per-scenario assumption.
- Every input and formula has a plain-language explanation available in an in-app Glossary, reusing the original workbook's own "why it matters" and "source" documentation.

## Current in-product disclaimer

This text appears at the top of the app on every load, in red for the first line:

> **DISCLAIMER:** Use for planning purposes only. Tax and payroll filing should be performed only by a professional.
> One S corporation. One W-2 employee, who is also the owner. One payroll scenario at a time. Not payroll processing or tax-return preparation.

This is currently just in-app text — there is no clickwrap acceptance, Terms of Service, or limitation-of-liability agreement.

## Data handling and privacy

- No user accounts, no login, no authentication of any kind.
- No data is persisted to any database or file on a server. All calculations happen in the user's browser session.
- The one exception is a "snapshot table": clicking "Add this scenario" appends a row to an in-memory table for that browser session only. It is not written to disk anywhere. It disappears if the tab is closed, the page is refreshed, or the hosting session times out — the user must explicitly download it as a CSV to keep it, and that file lands on their own device.
- Currently hosted on Posit Connect Cloud's free tier at a shareable link, for early/informal feedback from one person. It is not yet distributed or sold to anyone.
- Under consideration for a later stage: compiling the app to run entirely client-side (via a technology called Shinylive), so that user data never reaches any server at all — the tool would run as a static page or local file. This is a future option, not the current architecture.

## Known limitations (worth naming explicitly)

- Tax rates/wage bases are user-entered defaults, not automatically kept current with law changes — the app does not verify or update them.
- No support for multiple employees, multiple companies, or multiple concurrent scenarios (each session models one scenario at a time, with a manual "capture several scenarios" workflow layered on top).
- The Cash Health Status thresholds are the product's own judgment call about what counts as a safe cash margin, not a regulatory or accounting standard.
- The tool does not file anything, submit anything, or connect to any payroll provider, bank, or tax authority.

## Questions for counsel

1. Is the current in-app disclaimer sufficient for informal preview sharing (one link, one early tester), or is something more formal needed even at that stage?
2. Before any commercial distribution (sale, wider sharing, or public link), what agreement/notice is needed — Terms of Service, EULA, a clickwrap acceptance gate, something else — and does that differ depending on whether it's distributed as a hosted link vs. a downloadable/offline file?
3. Given the subject matter (payroll/tax estimates), is there recommended limitation-of-liability or "no warranty" language beyond what's shown above?
4. Should we be looking at tech E&O (errors & omissions) insurance before any wider distribution, and if so, at what point (preview vs. paid product)?
5. Does anything change if the product moves to a fully client-side architecture where we never receive or store the user's data?
