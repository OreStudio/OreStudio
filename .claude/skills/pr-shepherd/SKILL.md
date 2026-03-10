---
name: pr-shepherd
description: Autonomously monitor a PR after creation, addressing review comments and CI failures until the build is green.
license: Complete terms in LICENSE.txt
---

This skill manages the post-creation phase of a Pull Request autonomously. After a PR is raised it enters a watch-and-fix loop: wait for initial feedback, address any review comments, monitor CI, and fix any failures — repeating until CI passes green.


# When to Use This Skill

Use this skill immediately after a PR has been created and pushed, when you want to:

-   Automatically pick up and address reviewer comments without manual prompting.
-   Monitor CI continuously and fix build or test failures as they appear.
-   Drive a PR to a green state with minimal user intervention.


# How to Use This Skill

1.  Ensure the PR has been pushed and created (e.g. via `gh pr create`).
2.  Run this skill, passing the PR number if not on the current branch.
3.  The skill will execute the watch-and-fix loop until CI is green.

See [Detailed Instructions](#org9e1aeda) for the full loop behaviour.


# Detailed Instructions


## Overview

Before starting the loop, ask the user for the polling interval:

> How long should I wait between checks? (default: 10 minutes)

Accept any value in minutes. Use 10 minutes if the user does not specify. Convert to seconds for `sleep` (e.g. 5 minutes → `sleep 300`).

The skill then runs a loop at that interval:

1.  Wait the chosen interval for CI and reviewers to produce initial feedback.
2.  Fetch and address all open review comments.
3.  Check CI status.
    -   **Green**: loop exits — PR is ready.
    -   **Still running**: wait another interval and repeat from step 2.
    -   **Red**: investigate failures, apply fixes, push, then wait and repeat.


## Step 1: Wait for Initial Feedback

After the PR is created, pause for the chosen interval before checking anything. This gives CI time to start and reviewers time to post early comments.

```sh
sleep <interval_in_seconds>
```


## Step 2: Fetch and Address Review Comments

Retrieve all unresolved review comments on the PR:

```sh
gh pr view --comments
gh api repos/{owner}/{repo}/pulls/{pr_number}/comments
```

For each open comment:

1.  Read the comment carefully and understand the requested change.
2.  Apply the fix in the relevant source file(s).
3.  Stage and commit the change:

```sh
git add <modified-files>
git commit -m "[component] Address review comment: <brief description>

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
```

1.  Push all accumulated commits once all comments in the current batch are addressed:

```sh
git push
```

1.  Reply to each review comment on GitHub, referencing the fix commit:

```sh
gh api repos/{owner}/{repo}/pulls/{pr_number}/comments/{comment_id}/replies \
  -X POST -f body="Fixed in commit <sha> — <brief description of what was changed>."
```

1.  Resolve each review thread on GitHub using the GraphQL API. First, get the thread IDs:

```sh
gh api graphql -f query='
{
  repository(owner: "{owner}", name: "{repo}") {
    pullRequest(number: {pr_number}) {
      reviewThreads(first: 50) {
        nodes {
          id
          isResolved
          comments(first: 1) { nodes { databaseId body } }
        }
      }
    }
  }
}'
```

Then resolve each unresolved thread:

```sh
gh api graphql -f query="mutation {
  resolveReviewThread(input: {threadId: \"<thread_id>\"}) {
    thread { isResolved }
  }
}"
```

**Important**: never amend existing commits — always create new commits for fixes. This preserves the review history.


## Step 3: Check CI Status

Query the current state of all CI checks on the PR:

```sh
gh pr checks
```

Interpret the result:

| Outcome          | Action                                           |
|---------------- |------------------------------------------------ |
| All green (pass) | Loop exits. PR is ready to merge.                |
| Pending/queued   | Wait the chosen interval, then return to Step 2. |
| Any failure      | Investigate and fix (see Step 4), then repeat.   |


## Step 4: Fix CI Failures

When one or more checks are red:

1.  Identify the failing check:

```sh
gh run list --branch <branch-name>
gh run view <run-id> --log-failed
```

1.  Read the failure output carefully. Common categories:
    -   **Compilation errors**: fix the offending source files.
    -   **Test failures**: fix or update the failing tests.
    -   **Linting / formatting**: apply the required formatting fixes.
    -   **Schema validation**: re-run `validate_schemas.sh` locally and fix warnings.

2.  Apply the fixes, commit, and push:

```sh
git add <modified-files>
git commit -m "[component] Fix CI failure: <brief description>

Co-Authored-By: Claude Sonnet 4.6 <noreply@anthropic.com>"
git push
```

1.  Return to Step 1 (wait the chosen interval) and repeat the loop.


## Loop Termination

The loop terminates when `gh pr checks` reports all checks as passed. At that point inform the user:

> All CI checks are green. The PR is ready to review and merge.
