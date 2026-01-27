---
name: ores-shell-recipes
description: Update shell recipes by comparing commands to the recipe documentation.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   The user wants to update the ORE Studio Shell recipes documentation
-   New shell commands have been added and need to be documented
-   Existing shell commands have changed and recipes need updating


# How to use this skill

1.  Compare the commands in `projects/ores.comms.shell/src/app/commands` to `doc/recipes/shell_recipes.org`.
2.  Identify any missing commands that are not documented.
3.  Add recipes for missing commands following the existing pattern.
4.  Use the Detailed Instructions section for the proper recipe format.


# Detailed instructions


## Recipe file location

The shell recipes are stored in `doc/recipes/shell_recipes.org`. This file demonstrates all functionality available in the ORE Studio Comms Shell.


## Command source locations

Commands are implemented in the following files:

-   `projects/ores.comms.shell/src/app/commands/accounts_commands.cpp`: Account management
-   `projects/ores.comms.shell/src/app/commands/rbac_commands.cpp`: Role-based access control
-   `projects/ores.comms.shell/src/app/commands/currencies_commands.cpp`: Currency management
-   `projects/ores.comms.shell/src/app/commands/variability_commands.cpp`: Feature flags
-   `projects/ores.comms.shell/src/app/commands/subscription_commands.cpp`: Event subscriptions
-   `projects/ores.comms.shell/src/app/commands/compression_commands.cpp`: Compression settings
-   `projects/ores.comms.shell/src/app/commands/connection_commands.cpp`: Server connectivity

New entity commands should be added following the [shell-entity-creator](../shell-entity-creator/SKILL.md) skill.


## Recipe format

Each recipe follows this pattern:

\#+begin\_src fundamental


### Recipe Name

Description of what this recipe demonstrates.

```sh
export ORES_SHELL_LOGIN_PASSWORD
./ores.comms.shell ${log_args} ${connect_args} ${login_args} << 'EOF'
command1
command2
exit
EOF
```

\#+end\_src


## Recipe sections

Recipes are organized into sections:

-   **General**: Help, version, and basic REPL commands
-   **Connectivity**: Connect, disconnect, auto-connect
-   **Accounts**: Bootstrap, login, logout, create, list, lock, unlock, roles
-   **Variability**: Feature flag management
-   **Currencies**: Currency CRUD operations
-   **RBAC**: Permissions and roles management
-   **Events**: Event subscriptions and notifications
-   **Compression**: Compression settings


## Adding a new recipe

1.  Identify which section the command belongs to.
2.  Create a named src block with the ores-shell commands (`#+name: recipe-name`).
3.  Create a sh src block that invokes ores.comms.shell with the named block using noweb.
4.  Include appropriate properties (login\_args, connect\_args) based on whether authentication is required.
5.  Add a descriptive paragraph explaining what the recipe demonstrates.


## Recipe naming conventions

-   Use lowercase with dashes: `lock-account`, `list-permissions`
-   Be descriptive: `currencies-add` not just `add`
-   Group related commands: `events-listen`, `events-unlisten`
