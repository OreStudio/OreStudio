---
name: skill-review
description: Review skills for consistency, conflicts, duplication, and alignment with codebase patterns.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to audit skills for quality and consistency. Use this skill:

-   Periodically to ensure skills remain current with codebase evolution.
-   After significant codebase changes that may invalidate skill instructions.
-   When adding new skills to ensure they don't duplicate existing content.
-   When onboarding to understand skill coverage and gaps.


# How to use this skill

1.  Identify which skills to review (all or specific subset).
2.  Follow the detailed instructions for each audit category.
3.  Generate findings and add remediation tasks to the sprint backlog.


# Detailed instructions


## Step 1: Identify scope

Skills are located under `doc/skills/`. Each skill has:

-   A folder named after the skill (lowercase, hyphen-separated).
-   A `skill.org` file containing the skill definition.

The skill catalog at `doc/skills/claude_code_skills.org` lists all available skills organised by category.


## Step 2: Skill structure audit

For each skill, verify it follows the standard structure per [Skill Manager](../skill-manager/SKILL.md) skill.


### Required Sections

| Check                 | Description                                                      |
|--------------------- |---------------------------------------------------------------- |
| Preamble              | Has `:PROPERTIES:` block with unique `:ID:`.                     |
| Title                 | Has `#+title:` with descriptive name.                            |
| Metadata              | Has markdown export block with `name`, `description`, `license`. |
| When to use           | Has "When to use this skill" section.                            |
| How to use            | Has "How to use this skill" section with numbered steps.         |
| Detailed instructions | Has "Detailed instructions" section if steps are complex.        |
| Licence               | Has "Artefacts" section with licence tangle block.               |


### Formatting

| Check            | Description                                           |
|---------------- |----------------------------------------------------- |
| Org-mode syntax  | Valid org-mode formatting throughout.                 |
| Table formatting | Tables use proper org-mode table syntax.              |
| Links            | Internal links use `[[id:UUID][Title]]` format.       |
| Code blocks      | Code examples use `#+begin_src` blocks with language. |


## Step 3: Duplication audit

This is a critical check. Skills should reference specialized skills rather than duplicating instructions.


### Cross-Skill Duplication

| Check                 | Description                                                                                              |
|--------------------- |-------------------------------------------------------------------------------------------------------- |
| CMake instructions    | Build/test/deploy commands should reference [CMake Runner](../cmake-runner/SKILL.md) skill.              |
| PlantUML instructions | Diagram generation should reference [PlantUML Modeler](../plantuml-class-modeler/SKILL.md) skill.        |
| Unit test patterns    | Test creation should reference [Unit Test Writer](../unit-test-writer/SKILL.md) skill.                   |
| Domain type patterns  | Domain type creation should reference [Domain Type Creator](../domain-type-creator/SKILL.md) skill.      |
| Component structure   | Component creation should reference [Component Creator](../component-creator/SKILL.md) skill.            |
| Messaging patterns    | Protocol work should reference [Binary Protocol Developer](../binary-protocol-developer/SKILL.md) skill. |
| Sprint backlog        | Backlog updates should reference [Agile Product Owner](../agile-product-owner/SKILL.md) skill.           |


### Common Duplication Patterns

Look for these patterns that indicate duplication:

| Pattern                           | Should Reference          |
|--------------------------------- |------------------------- |
| `cmake --preset` commands         | CMake Runner              |
| `cmake --build --target` commands | CMake Runner              |
| PlantUML generation commands      | PlantUML Modeler          |
| Catch2 test structure examples    | Unit Test Writer          |
| Domain type header/cpp templates  | Domain Type Creator       |
| CMakeLists.txt templates          | Component Creator         |
| Message struct templates          | Binary Protocol Developer |
| Sprint backlog story format       | Agile Product Owner       |


### Resolution Pattern

When duplication is found, replace inline instructions with a reference:

**Before (duplicated):**

```
** Step 9: Build and verify

1. Reconfigure CMake:

#+begin_src sh
cmake --preset linux-clang-debug
#+end_src

2. Build the project:

#+begin_src sh
cmake --build --preset linux-clang-debug --target COMPONENT.lib
#+end_src
```

**After (reference):**

```
** Step 9: Build and verify

Using the [[id:654d4a70-e63d-4c18-b5a5-886066e36314][CMake Runner]] skill:

1. Configure the project.
2. Build the component library (=COMPONENT.lib= target).
3. Build and run the component tests (=test_COMPONENT.tests= target).
```


## Step 4: Conflict detection

Check for conflicting instructions between skills.


### Naming Conventions

| Check              | Description                                         |
|------------------ |--------------------------------------------------- |
| Case conventions   | All skills agree on snake\_case vs CamelCase rules. |
| File naming        | All skills agree on file naming patterns.           |
| Namespace patterns | All skills agree on namespace structure.            |


### Structural Patterns

| Check             | Description                                         |
|----------------- |--------------------------------------------------- |
| Folder structure  | All skills agree on component/part/facet structure. |
| Include paths     | All skills agree on include path conventions.       |
| Test organisation | All skills agree on test file placement and naming. |


### Code Patterns

| Check          | Description                                     |
|-------------- |----------------------------------------------- |
| Error handling | All skills agree on `std::expected` usage.      |
| Logging        | All skills agree on `BOOST_LOG_SEV` patterns.   |
| Serialization  | All skills agree on reflect-cpp/JSON patterns.  |
| Database       | All skills agree on sqlgen/repository patterns. |


## Step 5: Codebase alignment audit

Verify skills match actual codebase patterns.


### Sample Verification

For each skill, verify instructions match reality by sampling:

| Check                | Description                                          |
|-------------------- |---------------------------------------------------- |
| Example files exist  | Files referenced as examples actually exist.         |
| Patterns match       | Code patterns described match actual implementation. |
| Dependencies current | Referenced dependencies are still in use.            |
| Paths valid          | File paths mentioned are still valid.                |


### Common Drift Patterns

| Pattern                   | Check                                                |
|------------------------- |---------------------------------------------------- |
| Namespace changes         | Skill uses old namespace, code uses new.             |
| Dependency updates        | Skill references old library, code uses replacement. |
| Structural reorganization | Skill describes old folder structure.                |
| API changes               | Skill shows old method signatures.                   |


## Step 6: Coverage analysis

Identify gaps in skill coverage.


### Missing Skills

| Check                 | Description                                           |
|--------------------- |----------------------------------------------------- |
| Undocumented patterns | Common code patterns with no guiding skill.           |
| New components        | Components added without corresponding skill updates. |
| New workflows         | Development workflows without skill documentation.    |


### Skill Gaps

Consider whether skills are needed for:

-   Event handling and pub/sub patterns.
-   Database migrations.
-   Security patterns (authentication, authorization).
-   Performance optimization.
-   Debugging and troubleshooting.
-   Deployment and operations.


## Step 7: Generate findings

Categorise issues by severity:


### Critical (Must Fix)

-   Skills with factually incorrect instructions.
-   Skills that conflict with each other.
-   Skills that would cause build failures if followed.


### Important (Should Fix)

-   Duplicated instructions that should reference other skills.
-   Skills out of sync with current codebase patterns.
-   Missing links between related skills.
-   Incomplete skill sections.


### Minor (Nice to Have)

-   Formatting inconsistencies.
-   Outdated examples that still work.
-   Missing optional sections.
-   Wording improvements.


## Step 8: Document findings

For each issue provide:

-   **Skill name** - Which skill has the issue.
-   **Section** - Which section of the skill.
-   **What's wrong** - Clear description of the problem.
-   **Related skills** - Other skills involved (for conflicts/duplication).
-   **How to fix** - Suggested resolution.


## Step 9: Add to sprint backlog

Create stories in the sprint backlog per [Agile Product Owner](../agile-product-owner/SKILL.md) skill.

Group issues by type:

-   **Duplication fixes** - Replace inline instructions with skill references.
-   **Conflict resolution** - Align conflicting instructions.
-   **Codebase sync** - Update skills to match current code.
-   **Missing skills** - Create new skills for undocumented patterns.

Example story format:

```
**** STARTED [skills] Remove duplication from domain-type-creator :doc:

The domain-type-creator skill duplicates CMake instructions that exist in
the cmake-runner skill.

***** Tasks
- [ ] Replace Step 9 CMake commands with reference to cmake-runner skill
- [ ] Verify all command references are accurate
- [ ] Test skill by creating a domain type

***** Notes

Skill review performed on YYYY-MM-DD.
Related skills: cmake-runner, domain-type-creator
```
