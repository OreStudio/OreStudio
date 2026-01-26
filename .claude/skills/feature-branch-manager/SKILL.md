---
name: feature-branch-manager
description: Manage feature branches across multi-phase implementations with clean git history.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When working on multi-phase implementations that require multiple PRs. Use this skill to transition between phases while maintaining a clean git history.

Typical scenarios:

-   A PR for the current phase has been merged to main.
-   You need to start work on the next phase of an implementation.
-   You want to clean up old feature branches after merge.


# How to use this skill

1.  Confirm the current PR has been merged to main.
2.  Follow the phase transition procedure to create a fresh branch.
3.  Continue with the next phase of work.


# Detailed instructions


## Branch naming conventions

Feature branches should follow this naming pattern:

```
feature/<entity>_<phase_description>
```

Examples:

-   `feature/currency_domain_type` - Phase 1: Domain class and I/O
-   `feature/currency_generator` - Phase 2: Test data generator
-   `feature/currency_repository` - Phase 3: Repository support
-   `feature/currency_tests` - Phase 4: Tests and documentation

For Qt entity implementations:

-   `feature/session_model_window` - Phase 1: Model and list window
-   `feature/session_detail_dialog` - Phase 2: Detail dialog
-   `feature/session_history_dialog` - Phase 3: History dialog
-   `feature/session_controller` - Phase 4: Controller and integration


## Phase transition procedure

Execute the following steps to transition between phases:


### Step 1: Note the current branch name

```sh
git branch --show-current
```

Store this for cleanup in later steps.


### Step 2: Check if current branch PR has been merged

Use the GitHub CLI to check if the PR for the current branch has been merged:

```sh
gh pr view --json state,mergedAt
```

If the PR is merged, you'll see `state: MERGED` and a `mergedAt` timestamp.

Alternative using git (if gh is not available):

```sh
git fetch origin main
git branch --contains $(git rev-parse HEAD) -r | grep origin/main
```

If the current branch's HEAD commit is contained in `origin/main`, the PR has been merged.

**Important**: Only proceed if the PR has been merged. If not merged, inform the user and stop.


### Step 3: Stash uncommitted changes (if any)

Check for uncommitted changes and stash them:

```sh
git status --short
```

If there are changes:

```sh
git stash push -m "WIP: Transitioning to next phase"
```


### Step 4: Fetch latest main

If you used the `gh` command in Step 2, you must now fetch the latest changes from main. If you used the git alternative (which includes a fetch), this step ensures you have the most recent state.

```sh
git fetch origin main
```


### Step 5: Create new feature branch

Create a new branch from the updated main for the next phase:

```sh
git checkout -b feature/<entity>_<next_phase> origin/main
```


### Step 6: Restore stashed changes (if any)

If changes were stashed in Step 3:

```sh
git stash pop
```

**Note**: If `git stash pop` results in merge conflicts, you must resolve them before proceeding. Use `git status` to see conflicted files and resolve them manually.


### Step 7: Delete old local branch

```sh
git branch -d <old_branch_name>
```

Use `-D` (force delete) only if the branch wasn't fully merged (rare).


### Step 8: Delete old remote branch

```sh
git push origin --delete <old_branch_name>
```

This keeps the remote repository clean.


## Complete example

Transitioning from Phase 1 (domain type) to Phase 2 (generator) for currency:

```sh
# Step 1: Note current branch
git branch --show-current
# Output: feature/currency_domain_type

# Step 2: Check if PR has been merged
gh pr view --json state,mergedAt
# Output: {"mergedAt":"2025-01-10T14:30:00Z","state":"MERGED"}

# Step 3: Stash any uncommitted changes
git status --short
# Output: M doc/agile/v0/sprint_backlog_08.org
git stash push -m "WIP: Transitioning to next phase"

# Step 4: Fetch latest main (includes the merged PR)
git fetch origin main

# Step 5: Create new branch for Phase 2
git checkout -b feature/currency_generator origin/main

# Step 6: Restore stashed changes
git stash pop

# Step 7: Delete old local branch
git branch -d feature/currency_domain_type

# Step 8: Delete old remote branch
git push origin --delete feature/currency_domain_type
```


## Important notes

-   **Never rebase and force push** - This creates confusing history and can cause issues for reviewers.
-   **Always start from fresh main** - Each phase gets a clean branch from the latest main, which includes all previously merged work.
-   **Delete branches after merge** - Keeps both local and remote repositories clean.
-   **Use descriptive branch names** - Makes it easy to identify what phase/work the branch represents.
-   **Stash handling is automatic** - Uncommitted changes are preserved across phase transitions.
