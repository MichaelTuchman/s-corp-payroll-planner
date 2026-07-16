# Contributing / Branch Workflow

This repo uses two long-lived branches.

## `dev` — the workbench (default)

All active development happens here. It is the default branch on GitHub,
so new work and pull requests target `dev`. Day-to-day commits land on
`dev`; nothing here is live until it is promoted to `main`.

## `main` — what's deployed

`main` is the source the live application deploys from. **Posit Connect
Cloud is configured to pull from `main`**, so the hosted app only changes
when `main` changes.

## Promoting work to the live app

When changes on `dev` are ready to go live:

```sh
git checkout main
git merge dev
git push origin main      # Connect Cloud redeploys from main
git checkout dev          # back to the workbench
```

This keeps a clean separation: experiment freely on `dev` without ever
touching the deployed app, and update the live version deliberately by
merging into `main`.
