---
name: ores-cli-recipes
description: Update CLI recipes by comparing commands to the recipe documentation.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   The user wants to update the ORE Studio CLI recipes documentation
-   New CLI commands have been added and need to be documented
-   Existing CLI commands have changed and recipes need updating
-   CLI options have been modified and need verification against recipes


# How to use this skill

1.  Compare the parsers in `projects/ores.cli/src/config/entity_parsers` to `doc/recipes/cli_recipes.org`.
2.  Identify any missing commands, operations, or options that are not documented.
3.  Fix any option name mismatches (e.g., `--admin` vs `--is-admin`).
4.  Add recipes for missing commands following the existing pattern.
5.  Use the Detailed Instructions section for the proper recipe format.


# Detailed instructions


## Recipe file location

The CLI recipes are stored in `doc/recipes/cli_recipes.org`. This file demonstrates all functionality available in the ORE Studio CLI.


## Command source locations

Entity parsers define commands and options in:

-   `projects/ores.cli/src/config/entity_parsers/currencies_parser.cpp`: Currency operations
-   `projects/ores.cli/src/config/entity_parsers/accounts_parser.cpp`: Account operations
-   `projects/ores.cli/src/config/entity_parsers/feature_flags_parser.cpp`: Feature flag operations
-   `projects/ores.cli/src/config/entity_parsers/login_info_parser.cpp`: Login info operations
-   `projects/ores.cli/src/config/entity_parsers/roles_parser.cpp`: Role operations
-   `projects/ores.cli/src/config/entity_parsers/permissions_parser.cpp`: Permission operations
-   `projects/ores.cli/src/config/entity_parsers/countries_parser.cpp`: Country operations
-   `projects/ores.cli/src/config/entity_parsers/change_reasons_parser.cpp`: Change reason operations
-   `projects/ores.cli/src/config/entity_parsers/change_reason_categories_parser.cpp`: Change reason category operations

The main parser that routes to entity parsers:

-   `projects/ores.cli/src/config/parser.cpp`: Main command routing


## Entity operations

Each entity supports these operations (check parser for current list):

| Entity                   | Operations                        |
|------------------------ |--------------------------------- |
| currencies               | import, export, list, delete, add |
| accounts                 | list, delete, add                 |
| feature-flags            | list, delete, add                 |
| login-info               | list, delete, add                 |
| roles                    | list, delete, add                 |
| permissions              | list, delete, add                 |
| countries                | list, delete, add                 |
| change-reasons           | list, delete, add                 |
| change-reason-categories | list, delete, add                 |


## Recipe format

Each recipe follows this pattern:

\#+begin\_src fundamental


### Operation Name

Description of what this recipe demonstrates.

```sh
export ORES_CLI_DB_PASSWORD
./ores.cli <entity> <operation> ${db_args} ${log_args} \
  --option1 value1 --option2 value2
```

\#+end\_src


## Recipe sections

Recipes are organized into sections:

-   **General**: Help, version
-   **Currencies**: Import, export, list, delete, add
-   **Accounts**: List, delete, add (regular and admin)
-   **Login Info**: List, delete, add
-   **Feature Flags**: List, delete, add
-   **Roles**: List, delete, add (with permissions)
-   **Permissions**: List, delete, add
-   **Countries**: List, delete, add
-   **Change Reasons**: List, delete, add
-   **Change Reason Categories**: List, delete, add


## Verifying options

When checking options, compare the `options_description` in the parser with the recipe. Common issues include:

-   Flag names (e.g., `--admin` vs `--is-admin`)
-   Required vs optional fields
-   Default values
-   Environment variable names (`ORES_CLI_DB_PASSWORD`)


## Adding a new recipe

1.  Identify which entity section the command belongs to.
2.  Check the parser for exact option names and requirements.
3.  Create a src block with the CLI command.
4.  Add appropriate `:wrap src` directive for output format (json, text, xml, csv).
5.  Include database and logging args (`${db_args} ${log_args}`).
6.  Add a descriptive paragraph explaining what the recipe demonstrates.
