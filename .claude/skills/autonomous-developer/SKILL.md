---
name: autonomous-developer
description: Autonomously pick up stories from sprint backlog, implement them incrementally with tests and commits, then submit for review.
license: Complete terms in LICENSE.txt
---


# When to Use This Skill

Use this skill when you want to autonomously work through stories in the sprint backlog with minimal human intervention. This skill combines agile story management with incremental development, comprehensive testing, and proper git workflow.

This is ideal for:

-   Working through well-defined stories from the sprint backlog
-   Implementing features with proper testing at each step
-   Maintaining clean git history with incremental commits
-   Creating professional pull requests for review
-   Following established development workflow automatically


# How to Use This Skill

Simply invoke this skill and it will:

1.  Pick up the next story from the current sprint backlog
2.  Create a feature branch
3.  Plan and implement the story incrementally
4.  Test and commit at each step
5.  Mark the story as BLOCKED (awaiting review)
6.  Push the branch and create a pull request


# Autonomous Developer Mode


## IMPORTANT: Explicit Mode Activation Required

This skill operates in two distinct modes, and the user MUST explicitly enable or disable autonomous developer mode:


### When Autonomous Developer Mode is ENABLED

When the user explicitly enables this mode (e.g., "Enable autonomous developer mode" or "Use the autonomous developer skill"), you should:

-   **Work autonomously** on stories from the sprint backlog
-   **Create feature branches** automatically with proper naming
-   **Commit changes** incrementally after each successful test
-   **Push branches** to remote repository
-   **Create pull requests** without asking
-   **Make assumptions** when clarification is needed (documenting them)
-   **Proceed continuously** until the story is complete


### When Autonomous Developer Mode is DISABLED (Default)

When this mode is NOT explicitly enabled (which is the default state), you should:

-   **Wait for explicit user guidance** at each step
-   **Never create feature branches** unless explicitly asked
-   **Never commit changes** unless explicitly asked
-   **Never push to remote** unless explicitly asked
-   **Never create pull requests** unless explicitly asked
-   **Ask for clarification** when uncertain rather than making assumptions
-   **Work interactively** with the user driving the development process


### How to Enable/Disable

The user must use explicit commands such as:

-   To enable: "Enable autonomous developer mode" or "Use the autonomous developer skill"
-   To disable: "Disable autonomous developer mode" or "Exit autonomous mode" or "Switch to interactive mode"


### Default Behavior

**By default, autonomous developer mode is DISABLED.** You should only operate in autonomous mode when the user has explicitly enabled it. When in doubt, ask the user if they want to enable autonomous developer mode.


# Detailed Instructions


## Step 1: Select Story from Sprint Backlog


### Identify Current Sprint Backlog

1.  List files in `doc/agile/v0/` matching pattern `sprint_backlog_*.org`
2.  Find the file with the highest sprint number - this is the current sprint
3.  Read the sprint backlog file to find candidate stories


### Story Selection Criteria

Pick the next story that:

1.  Has status `STARTED` or is unmarked (no status keyword)
2.  Is NOT `COMPLETED`, `CANCELLED`, `POSTPONED`, or `BLOCKED`
3.  Appears next in the file after any currently in-progress stories
4.  Aligns with the sprint mission


### Multiple Stories in Progress

If multiple stories are already `STARTED`:

1.  Look for the one that appears to need the most immediate attention
2.  Check if any story has uncompleted tasks that can be progressed
3.  If unclear, ask the user which story to work on


## Step 2: Create Feature Branch


### Branch Naming Convention

Create a feature branch with the format:

```sh
feature/summary-of-story-title
```

Rules for branch name:

1.  **Lowercase** - all letters should be lowercase
2.  **Hyphen-separated** - use hyphens to separate words
3.  **Concise** - use 3-5 key words from story title
4.  **Descriptive** - should indicate what the feature is about

Examples:

-   Story: "Implement delete account" → `feature/implement-delete-account`
-   Story: "Add protocol version to splash screen" → `feature/add-protocol-version-splash`
-   Story: "Refactor error handling in client" → `feature/refactor-client-errors`


### Git Commands

```sh
# Ensure we're on main and up to date
git checkout main
git pull origin main

# Create and checkout feature branch
git checkout -b feature/summary-of-story-title
```


## Step 3: Manage Story Using Agile Practices

Follow the Agile Product Owner skill practices:


### Mark Story as STARTED

Update the story status in the sprint backlog:

```fundamental
*** STARTED Story title                                              :code:
    :LOGBOOK:
    CLOCK: [2025-11-18 Tue 10:30]
    :END:
```


### Create Implementation Plan

Under the story, add or update a `Tasks` section with a detailed, incremental plan:

```fundamental
***** Tasks

- [ ] Step 1: Initial setup and scaffolding
- [ ] Step 2: Implement core functionality
- [ ] Step 3: Add tests
- [ ] Step 4: Handle edge cases
- [ ] Step 5: Update documentation
- [ ] Step 6: Final testing and cleanup
```


### Task Characteristics

Each task should be:

1.  **Small and incremental** - completable in 15-30 minutes
2.  **Testable** - can be verified independently
3.  **Committable** - produces a meaningful git commit
4.  **Clear** - unambiguous what needs to be done


## Step 4: Implement Incrementally

For each task in your plan:


### Implementation Cycle

1.  **Implement** - Write the code for the current task
2.  **Compile** - Build the project to ensure it compiles
3.  **Test** - Run all relevant tests
4.  **Commit** - Create a git commit if successful
5.  **Update backlog** - Mark task as complete in sprint backlog


### Detailed Steps

-   A. Implement the Task

    Write the code needed for the current task. Keep changes focused and minimal.

-   B. Compile the Code

    ```sh
    cmake --build --preset linux-clang-debug
    ```
    
    If compilation fails:
    
    1.  Fix the errors
    2.  Compile again
    3.  Repeat until successful
    4.  Do NOT proceed to next step until code compiles

-   C. Run Tests

    Run appropriate tests based on what was changed:
    
    ```sh
    # For specific component tests
    cmake --build --preset linux-clang-debug --target ores.COMPONENT.tests
    ./build/output/linux-clang-debug/publish/bin/ores.COMPONENT.tests
    
    # For all tests (if time permits and changes are broad)
    cmake --build --preset linux-clang-debug --target rat
    ```
    
    If tests fail:
    
    1.  Fix the issues
    2.  Recompile if needed
    3.  Run tests again
    4.  Repeat until all tests pass
    5.  Do NOT proceed to next step until tests pass

-   D. Create Git Commit

    Once code compiles and tests pass, create a descriptive commit:
    
    ```sh
    git add <relevant-files>
    git commit -m "[COMPONENT] Brief description of what was done
    
    More detailed explanation if needed, including:
    - What specific functionality was added/changed
    - Why this approach was chosen
    - Any important implementation details
    
    🤖 Generated with [Claude Code](https://claude.com/claude-code)
    
    Authored-By: Claude <noreply@anthropic.com>"
    ```
    
    -   Git Commit Convention
    
        Use the following prefix format:
        
        -   `[component]` - For code changes (e.g., `[comms]`, `[qt]`, `[cli]`)
        -   `[agile]` - For sprint backlog and agile documentation changes
        -   `[infra]` - For build system, CI/CD changes
        -   `[doc]` - For documentation changes
        
        Examples:
        
        ```fundamental
        [comms] Add connection_error exception class
        
        Created custom exception for connection errors to replace
        the last_error_ member variable pattern. This provides
        more idiomatic C++ error handling.
        
        🤖 Generated with [Claude Code](https://claude.com/claude-code)
        
        Authored-By: Claude <noreply@anthropic.com>
        ```
        
        ```fundamental
        [agile] Update sprint backlog with CLI delete implementation
        
        Added CLI implementation tasks and notes to the "Implement
        delete account" story.
        
        🤖 Generated with [Claude Code](https://claude.com/claude-code)
        
        Authored-By: Claude <noreply@anthropic.com>
        ```

-   E. Update Sprint Backlog

    After successful commit, mark the task as complete:
    
    ```fundamental
    ***** Tasks
    
    - [X] Step 1: Initial setup and scaffolding
    - [ ] Step 2: Implement core functionality
    - [ ] Step 3: Add tests
    ```
    
    Add implementation details under a `COMPLETED` subsection:
    
    ```fundamental
    ***** COMPLETED Step 1: Initial setup and scaffolding
    CLOSED: [2025-11-18 Tue 11:00]
    
    Modified files:
    - =path/to/file.cpp= - Added initial class structure
    - =path/to/file.hpp= - Added header declarations
    
    Notes: Used pattern from similar component for consistency.
    ```
    
    Commit the backlog update:
    
    ```sh
    git add doc/agile/v0/sprint_backlog_NN.org
    git commit -m "[agile] Update backlog: completed step 1 of story
    
    🤖 Generated with [Claude Code](https://claude.com/claude-code)
    
    Authored-By: Claude <noreply@anthropic.com>"
    ```


## Step 5: Handle Clarifications

If you encounter ambiguity or need clarification:


### Document Questions in Sprint Backlog

Add a `Questions` section to the story:

```fundamental
***** Questions

1. *Question*: Should we support both UUID and username for deletion?
   *Assumption*: Implementing both for flexibility. Can remove username
   support if not needed.

2. *Question*: What error message format should we use?
   *Assumption*: Following existing pattern from currency deletion.
   Will adjust if different format is preferred.
```


### Make Best Judgment and Proceed

1.  Document your assumption clearly
2.  Implement based on that assumption
3.  Proceed with the work
4.  The user will review and provide feedback during PR review


### Do Not Block on Clarifications

-   Keep moving forward with reasonable assumptions
-   Document all assumptions for review
-   Avoid getting stuck waiting for answers
-   Trust that PR review will catch any issues


## Step 6: Complete All Tasks

Continue the implementation cycle (Step 4) for each task until:

1.  All tasks are marked `[X]` completed
2.  All code compiles successfully
3.  All tests pass
4.  All changes are committed to git


## Step 7: Mark Story as BLOCKED

Once all implementation is complete:


### Update Story Status

Change status from `STARTED` to `BLOCKED`:

```fundamental
*** BLOCKED Story title                                              :code:
CLOSED: [2025-11-18 Tue 15:30]
    :LOGBOOK:
    CLOCK: [2025-11-18 Tue 10:30]--[2025-11-18 Tue 15:30] =>  5:00
    :END:

Description of the story.

***** Tasks

- [X] All tasks completed

***** COMPLETED Implementation Details

All code complete and committed. Feature branch ready for review.

***** Notes

Awaiting user review and testing before merge.
```


### Add Rationale for BLOCKED Status

In the `Notes` section, clarify:

```fundamental
***** Notes

Story is BLOCKED pending:
- User acceptance testing
- Code review feedback
- Pull request approval

All implementation is complete and tests are passing.
```


### Commit Backlog Update

```sh
git add doc/agile/v0/sprint_backlog_NN.org
git commit -m "[agile] Mark story as BLOCKED awaiting review

All implementation completed. Ready for user testing and PR review.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Authored-By: Claude <noreply@anthropic.com>"
```


## Step 8: Push Feature Branch

Push your feature branch to the remote repository:

```sh
git push -u origin feature/summary-of-story-title
```

If push fails (e.g., branch already exists):

1.  Check if someone else is working on this branch
2.  If not, use `git push --force-with-lease` to update
3.  Document in PR that force-push was needed


## Step 9: Create Pull Request

Follow the [PR Manager](../pr-manager/SKILL.md) skill to create and manage the pull request through its complete lifecycle.


## Step 10: Final Updates


### Update Sprint Backlog with PR Link

Add the PR link to the story:

```fundamental
***** Notes

Pull Request: URL of the PR.
Branch: feature/summary-of-story-title

Story is BLOCKED pending PR review and testing.
```


### Commit Final Backlog Update

```sh
git checkout main
git add doc/agile/v0/sprint_backlog_NN.org
git commit -m "[agile] Add PR link to story

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Authored-By: Claude <noreply@anthropic.com>"
git push origin main
```


### Return to Main Branch

```sh
git checkout main
```


## Step 11: Summary Report

Update the story in the sprint backlog with a summary:

```fundamental
✓ Story Implementation Complete

Story: [Story Title]
Branch: feature/summary-of-story-title
Status: BLOCKED (awaiting review)
Pull Request: #123

Implementation Summary:
- [X] All tasks completed
- [X] Code compiles successfully
- [X] All tests passing (N test cases, M assertions)
- [X] [number] commits created
- [X] Sprint backlog updated
- [X] Pull request created

The story is ready for your review and testing.
Please review the PR at: [GitHub PR URL]

Next steps:
1. Review the code changes
2. Test the functionality
3. Provide feedback on the PR
4. Approve and merge when satisfied
```


# Best Practices


## Incremental Commits

-   Commit early and often
-   Each commit should be self-contained and buildable
-   Commit messages should be descriptive and follow conventions
-   Avoid large "kitchen sink" commits


## Testing Discipline

-   Always run tests before committing
-   Fix failing tests immediately
-   Add tests for new functionality
-   Verify tests actually test what they claim to test


## Documentation as You Go

-   Update sprint backlog continuously
-   Document decisions and assumptions
-   Mark tasks complete immediately after committing
-   Keep notes section current with implementation details


## Clean Git History

-   Use meaningful commit messages
-   Follow commit message conventions
-   Avoid fixup commits (get it right the first time)
-   Use atomic commits (one logical change per commit)


## Communication

-   Document all assumptions in sprint backlog
-   Add questions when uncertain
-   Explain "why" not just "what" in commits
-   Make PR descriptions comprehensive


## Time Management

-   Keep tasks small and manageable
-   Don't let perfect be the enemy of good
-   Make progress, document issues, move forward
-   Use BLOCKED status appropriately
