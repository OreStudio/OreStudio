---
name: domain-type-creator
description: Guide for creating new domain types with full JSON I/O, table I/O, generators, and repository support.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to add a new domain type to the ORE Studio project with complete support for JSON I/O, table I/O, test data generation, and database persistence with CRUD operations.


# How to use this skill

1.  Gather information about the domain type (name, fields, project location).
2.  Follow the detailed instructions to create all required artefacts in order.
3.  Build and test the new domain type.
4.  Raise PRs at designated checkpoints and wait for review before proceeding.


# Related skills

-   [cmake-runner](../cmake-runner/SKILL.md): For building and testing the component.
-   [plantuml-class-modeler](../plantuml-class-modeler/SKILL.md): Update class diagrams for the component.
-   [feature-branch-manager](../feature-branch-manager/SKILL.md): Manage branch transitions between phases.
-   [pr-manager](../pr-manager/SKILL.md): Create a new Pull Request (PR).


# PR strategy

Creating a complete domain type involves significant work across multiple files. To keep PRs reviewable, the implementation is split into phases with designated checkpoints.

| Phase | Steps | Focus                        | PR Title Template                           |
|----- |----- |---------------------------- |------------------------------------------- |
| 1     | 1-3   | Domain class and I/O support | [COMPONENT] Add <Entity> domain type        |
| 2     | 4     | Test data generator          | [COMPONENT] Add <Entity> generator          |
| 3     | 5-6   | Repository and persistence   | [COMPONENT] Add <Entity> repository         |
| 4     | 7-10  | Tests and documentation      | [COMPONENT] Add <Entity> tests and diagrams |


## Phase checkpoints

After completing each phase:

1.  Using [cmake-runner](../cmake-runner/SKILL.md) skill, build and verify there are no errors.
2.  Commit all changes with an appropriate message.
3.  Push the branch and use [pr-manager](../pr-manager/SKILL.md) skill to raise a PR with the suggested title.
4.  Wait for review feedback and address any comments.
5.  Once approved, merge the PR back to main.
6.  Use the [feature-branch-manager](../feature-branch-manager/SKILL.md) skill to transition to the next phase.


# Detailed instructions

The following sections describe the step-by-step process for creating a complete domain type.


## Gather requirements

Before starting, gather the following information from the user:

-   **Domain type name**: the name of the new type (e.g., `currency`, `account`).
-   **Component location**: which component the domain type belongs to (e.g., `ores.refdata`, `ores.accounts`).
-   **Fields**: list of fields with their types and descriptions.


## Phase 1: Domain class and I/O support


### Step 1: Create the domain class definition

Follow the instructions in [Domain Type Class Definition Facet](domain_type_class_definition_facet.md).


### Step 2: Create JSON I/O support

Follow the instructions in [Domain Type JSON I/O Facet](domain_type_json_io_facet.md).


### Step 3: Create table I/O support

Follow the instructions in [Domain Type Table I/O Facet](domain_type_table_io_facet.md).


### Phase 1 checkpoint

At this point you have a complete domain type with JSON and table I/O support.

1.  Build the component library to verify there are no errors.
2.  Commit with message: `[COMPONENT] Add <Entity> domain type`
3.  Push and raise PR: `[COMPONENT] Add <Entity> domain type`
4.  Wait for review and merge before continuing.
5.  Use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 2.


## Phase 2: Test data generator


### Step 4: Create generator support

Follow the instructions in [Domain Type Generator Support Facet](domain_type_generator_facet.md).


### Phase 2 checkpoint

At this point you have a test data generator for the domain type.

1.  Build the component library to verify there are no errors.
2.  Commit with message: `[COMPONENT] Add <Entity> generator`
3.  Push and raise PR: `[COMPONENT] Add <Entity> generator`
4.  Wait for review and merge before continuing.
5.  Use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 3.


## Phase 3: Repository and persistence


### Step 5: Create repository entity and mapper

Follow the instructions in [Domain Type Repository Entity and Mapper Facet](domain_type_repository_entity_and_mapper_facet.md).


### Step 6: Create repository with CRUD operations

Follow the instructions in [Domain Type Repository CRUD Operations Facet](domain_type_repository_crud_operations_facet.md).


### Phase 3 checkpoint

At this point you have full repository support for the domain type.

1.  Build the component library to verify there are no errors.
2.  Commit with message: `[COMPONENT] Add <Entity> repository`
3.  Push and raise PR: `[COMPONENT] Add <Entity> repository`
4.  Wait for review and merge before continuing.
5.  Use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 4.


## Phase 4: Tests and documentation


### Step 7: Update CMakeLists.txt

Follow the instructions in [Domain Type CMakeLists Update Facet](domain_type_cmakelists_facet.md).


### Step 8: Create tests

Follow the instructions in [Domain Type Test Creation Facet](domain_type_test_facet.md).


### Step 9: Update UML diagrams

Update the component's UML diagrams to include the new domain type:

1.  Use the [plantuml-class-modeler](../plantuml-class-modeler/SKILL.md) skill to update class diagrams showing the new domain type and its relationships.
2.  If repository support was added, use the [plantuml-er-modeler](../plantuml-er-modeler/SKILL.md) skill to update the database ER diagram with the new table.
3.  Build the diagrams using the appropriate CMake targets.


### Step 9: Build and verify

Using the [cmake-runner](../cmake-runner/SKILL.md) skill:

1.  Configure the project (reconfigure to pick up new files).
2.  Build the component library (`COMPONENT.lib` target).
3.  Build and run the component tests (`test_COMPONENT.tests` target).


### Step 10: Update UML diagrams

Update the component's UML diagrams to include the new domain type:

1.  Use the [plantuml-class-modeler](../plantuml-class-modeler/SKILL.md) skill to update class diagrams showing the new domain type and its relationships.
2.  Build the diagrams using the appropriate CMake targets.


### Phase 4 checkpoint

This is the final phase. At this point you have complete domain type support with tests and documentation.

1.  Build and run all tests to verify everything works.
2.  Commit with message: `[COMPONENT] Add <Entity> tests and diagrams`
3.  Push and raise PR: `[COMPONENT] Add <Entity> tests and diagrams`
4.  Wait for review and merge.

The domain type implementation is now complete.


# Common patterns and conventions


## Naming conventions

-   Use snake\_case for all C++ identifiers (classes, methods, variables)
-   File names should match the class name exactly
-   Headers use `.hpp` extension
-   Implementation files use `.cpp` extension


## Code organization

-   Group related functionality in logically named files
-   Standard library includes first, then third-party, then project headers
-   Use namespace aliases to reduce verbosity


## Documentation

-   Add doxygen comments for all public APIs
-   Explain the "why" in comments, not the "what"
-   Include `@brief` tags for all documented items


## Service layer patterns

When creating a service class to expose repository operations, follow these patterns:


### Use save\_\* for upsert semantics

The repository `write()` method has **upsert semantics** - it creates a new record or updates an existing one. The service layer should expose this same behavior through `save_*` methods:

```cpp
// CORRECT: Single save_* method with upsert semantics
void currency_service::save_currency(const domain::currency& currency) {
    currency_repo_.write(currency);
}
```

**DO NOT** create separate `create_*` and `update_*` methods at the service level:

```cpp
// WRONG: Separate create/update methods duplicate repository semantics
void currency_service::create_currency(const domain::currency& c);  // Don't do this
void currency_service::update_currency(const domain::currency& c);  // Don't do this
```


### Service method naming convention

| Operation        | Method Name Pattern        | Notes                         |
|---------------- |-------------------------- |----------------------------- |
| Create or update | `save_<entity>`            | Upsert via repository write() |
| Delete           | `remove_<entity>`          | Soft delete via repository    |
| Find by key      | `find_<entity>`            | Returns std::optional         |
| List all         | `list_<entities>`          | Returns vector                |
| List with filter | `list_<entities>_by_<key>` | Filtered list                 |
| List since time  | `list_<entities>_since`    | Incremental loading support   |
| Get history      | `get_<entity>_history`     | All versions of an entity     |


### Reference implementation

See `projects/ores.refdata/src/service/currency_service.cpp` for a correct implementation of the service layer pattern.
