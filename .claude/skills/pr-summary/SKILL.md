---
name: pr-summary-generator
description: Generate a pull request title and overview using only committed work on the current branch.
license: Complete terms in LICENSE.txt
---

This skill inspects the Git commit history of the current branch, from the branching point up to the latest commit **without pushing changes** and **ignoring uncommitted files**. It then generates a clean, descriptive Pull Request (PR) title and overview.


# When to Use This Skill

Use this skill whenever you want to generate a clear and concise Pull Request summary based solely on committed work in the current branch. Note that only committed changes must be considered.


# How to Use This Skill

1.  Ensure that your local branch contains the commits you wish to summarise.
2.  Run the skill from within the repository root.
3.  Provide the base branch if different from the default (`main`).
4.  The skill will automatically:
    -   Determine the branch point,
    -   Extract the relevant commit messages,
    -   Produce a clean PR title and overview.


# Detailed Instructions

Perform the following steps:

1.  Identify the branching point from the base branch using:

```sh
git merge-base HEAD main
```

1.  Collect only the committed work:

```shell
git log --pretty=format:"%s" <merge-base>..HEAD
```

1.  Process the commit messages to infer topic, scope, and intent.

2.  Generate:
    -   A short PR title.
    -   A structured, multi‑paragraph overview.


# Example Workflow

```sh

# Identify branch point
merge_base=$(git merge-base HEAD main)

# Collect commit messages
git log --pretty=format:"%s" ${merge_base}..HEAD

# The skill then processes the output to synthesise a PR title and overview.
```
