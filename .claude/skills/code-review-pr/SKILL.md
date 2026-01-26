---
name: code-review-pr
description: Review pull request changes for code quality, conventions, and correctness.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to review changes in a pull request or a set of commits before merging. This skill focuses on the delta (changed files) rather than a full component audit.


# How to use this skill

1.  Identify the PR or commit range to review.
2.  Gather the list of changed files.
3.  Follow the detailed instructions to review each category.
4.  Generate findings and add them to the sprint backlog.


# Detailed instructions


## Step 1: Scope the review

Determine what is being reviewed:

-   For a PR: use `git diff main...HEAD` or `gh pr diff` to see changes.
-   For commits: use `git diff COMMIT1..COMMIT2`.

List all changed files and categorise them by component and facet.


## Step 2: Review checklist

For each changed file, apply the following checks. Not all checks apply to all files - use judgement based on what was changed.


### Headers & Licensing

| Check            | Description                                                           |
|---------------- |--------------------------------------------------------------------- |
| Copyright header | Every `.hpp` and `.cpp` file must have the standard copyright header. |
| Mode line        | Files should have the Emacs mode line at the top.                     |

Reference: See any existing file in the codebase for the expected format.


### Code Style & Naming

| Check            | Description                                                                               |
|---------------- |----------------------------------------------------------------------------------------- |
| snake\_case      | All components except `ores.qt` must use snake\_case for classes, methods, and variables. |
| CamelCase for Qt | `ores.qt` must use CamelCase due to Qt conventions.                                       |
| Include order    | Standard library first, then third-party, then project headers.                           |

Reference: [Component Creator](../component-creator/SKILL.md) skill for component conventions.


### Comment Quality

| Check                  | Description                                                                  |
|---------------------- |---------------------------------------------------------------------------- |
| No stale comments      | Remove historical, deprecated, or TODO comments that are no longer relevant. |
| Useful comments only   | Comments should explain "why", not "what".                                   |
| Doxygen for public API | All public classes and methods should have `@brief` documentation.           |


### Platform Isolation

| Check                  | Description                                                                       |
|---------------------- |--------------------------------------------------------------------------------- |
| Platform-specific code | OS-specific code (`_WIN32`, `__linux__`, etc.) should only be in `ores.platform`. |
| No platform leakage    | Other components should not have platform-specific macros or includes.            |


### Code Locality

| Check               | Description                                                       |
|------------------- |----------------------------------------------------------------- |
| Utility placement   | Generic helper code belongs in `ores.utility` or `ores.platform`. |
| No duplication      | Check if similar code already exists elsewhere in the codebase.   |
| Function extraction | Repetitive code should be factored into functions.                |


### C++23 Idioms

| Check            | Description                                                           |
|---------------- |--------------------------------------------------------------------- |
| Modern patterns  | Prefer `std::expected` over exceptions for error handling.            |
| Ranges and views | Use C++23 ranges where appropriate.                                   |
| Concepts         | Use concepts for template constraints.                                |
| `std::format`    | Prefer `std::format` over stream concatenation for string formatting. |


### Domain Types

If adding or modifying domain types, verify completeness per [Domain Type Creator](../domain-type-creator/SKILL.md):

| Check      | Description                                                   |
|---------- |------------------------------------------------------------- |
| JSON I/O   | Domain type has `_json_io.hpp/.cpp` for serialization.        |
| Table I/O  | Domain type has `_table_io.hpp/.cpp` for table output.        |
| Generator  | Domain type has a generator in the `generators` facet.        |
| Repository | If persisted, domain type has entity, mapper, and repository. |


### Protocol & Messaging

If adding or modifying protocol messages, verify per [Binary Protocol Developer](../binary-protocol-developer/SKILL.md):

| Check                 | Description                                                      |
|--------------------- |---------------------------------------------------------------- |
| Message type enum     | New messages added to `message_types.hpp` in correct range.      |
| Request/response pair | Requests end with `_request`, responses with `_response`.        |
| Serialization         | `serialize()` and `deserialize()` methods implemented correctly. |
| Message traits        | `message_traits` specialization added for new request types.     |
| Version bump          | Protocol version updated if breaking change.                     |


### Temporality Patterns

If adding or modifying persisted domain types, verify temporal audit patterns:

| Check              | Description                                                          |
|------------------ |-------------------------------------------------------------------- |
| Domain type fields | Domain types have `recorded_by` and `recorded_at` fields for audit.  |
| Entity fields      | Database entities have corresponding temporal columns.               |
| Mapper correctness | Mappers correctly convert temporal fields between domain and entity. |
| Repository writes  | Repository write operations populate temporal fields.                |
| Time point queries | Repository supports `read_at_timepoint()` for historical queries.    |


### Unit Tests

Verify test coverage per [Unit Test Writer](../unit-test-writer/SKILL.md):

| Check       | Description                                                 |
|----------- |----------------------------------------------------------- |
| Tests exist | Changed code has corresponding test coverage.               |
| Test naming | Tests follow `layer_class_tests.cpp` naming convention.     |
| No SECTIONs | Tests use separate `TEST_CASE` instead of `SECTION` blocks. |
| Logging     | Tests use `BOOST_LOG_SEV` for debugging output.             |


### Error Handling

| Check               | Description                                           |
|------------------- |----------------------------------------------------- |
| `std::expected`     | Use `std::expected` for operations that can fail.     |
| No silent failures  | Errors should be logged or propagated, never ignored. |
| Meaningful messages | Error messages should be descriptive and actionable.  |


### Security

| Check                | Description                                           |
|-------------------- |----------------------------------------------------- |
| No hardcoded secrets | No passwords, keys, or tokens in code.                |
| Input validation     | External input is validated before use.               |
| Authorization        | Server-side authorization checks in message handlers. |


### Skill Consistency

If changing code patterns that are documented in skills, verify skills remain accurate:

| Check                | Description                                                        |
|-------------------- |------------------------------------------------------------------ |
| Skill alignment      | Changes don't contradict instructions in relevant skills.          |
| Skill updates needed | If introducing new patterns, consider if skills should be updated. |
| Cross-reference      | Check skills under `doc/skills/` that relate to changed code.      |

Relevant skills to check based on changes:

-   Domain types: [Domain Type Creator](../domain-type-creator/SKILL.md)
-   Messaging: [Binary Protocol Developer](../binary-protocol-developer/SKILL.md)
-   Components: [Component Creator](../component-creator/SKILL.md)
-   Tests: [Unit Test Writer](../unit-test-writer/SKILL.md)
-   Diagrams: [PlantUML Modeler](../plantuml-class-modeler/SKILL.md)


## Step 3: Generate findings

For each issue found, categorise by severity:


### Critical (Must Fix)

Issues that must be resolved before merge:

-   Bugs that cause incorrect behaviour.
-   Security vulnerabilities.
-   Data loss risks.
-   Broken functionality.


### Important (Should Fix)

Issues that should be addressed but don't block merge:

-   Architecture problems.
-   Missing features from requirements.
-   Poor error handling.
-   Test coverage gaps.
-   Convention violations.


### Minor (Nice to Have)

Suggestions for improvement:

-   Code style preferences.
-   Optimisation opportunities.
-   Documentation improvements.
-   Refactoring suggestions.


## Step 4: Document findings

For each issue, provide:

-   **<line>** - Exact location of the issue.
-   **What's wrong** - Clear description of the problem.
-   **Why it matters** - Impact of not fixing.
-   **How to fix** - Suggested resolution (if not obvious).


## Step 5: Add to sprint backlog

Create stories in the sprint backlog per [Agile Product Owner](../agile-product-owner/SKILL.md) skill:

-   Group related issues into stories by component or theme.
-   Use `:code:` tag for implementation fixes.
-   Use `:doc:` tag for documentation issues.
-   Include <line> references in task descriptions.

Example story format:

```
**** STARTED [component] Address PR review findings :code:

Review findings from PR #XXX.

***** Tasks
- [ ] Fix: missing copyright header in src/domain/foo.cpp
- [ ] Fix: snake_case violation in bar_service.hpp:42
- [ ] Add: unit tests for baz_repository

***** Notes

PR: https://github.com/OreStudio/OreStudio/pull/XXX
```


## Step 6: Provide verdict

Conclude the review with:

-   **Ready to merge** - No critical issues, important issues acceptable.
-   **Ready with fixes** - Critical issues identified that must be fixed first.
-   **Not ready** - Significant rework required.

Include a 1-2 sentence reasoning for the verdict.
