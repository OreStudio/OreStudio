---
name: shell-entity-creator
description: Create shell commands for domain entities in ores.comms.shell including list, add, history, and other operations.
license: Complete terms in LICENSE.txt
---


# When to use this skill

When you need to add shell commands for a new entity in `ores.comms.shell`. This skill guides you through creating the command class, registering it with the REPL, and implementing standard operations following established patterns.

Prerequisites:

-   The domain type must already exist (see [domain-type-creator](../domain-type-creator/SKILL.md) skill)
-   The messaging protocol for CRUD operations must exist (see [binary-protocol-developer](../binary-protocol-developer/SKILL.md) skill)
-   The server-side handlers must be implemented
-   Table I/O streaming must be available for list output


# How to use this skill

1.  Gather entity requirements (name, operations needed, parameters).
2.  Follow the detailed instructions to create commands in phases.
3.  Each phase ends with a PR checkpoint - raise PR, wait for review, merge.
4.  Create a fresh branch from main for the next phase (see [feature-branch-manager](../feature-branch-manager/SKILL.md)).
5.  Build and test after each step.


# PR Strategy

This skill is structured into **three phases**, each resulting in a separate PR.

| Phase | Steps     | PR Title Template                            |
|----- |--------- |-------------------------------------------- |
| 1     | Steps 1-3 | `[shell] Add <entity> list and add commands` |
| 2     | Steps 4-5 | `[shell] Add <entity> history command`       |
| 3     | Step 6    | `[doc] Add <entity> shell command recipes`   |

After each PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to the next phase.


# Detailed instructions

The following sections describe the step-by-step process for creating shell commands for an entity.


## Gather Requirements

Before starting, gather the following information:

-   **Entity name**: The name of the entity (e.g., `currency`, `country`, `account`).
-   **Component location**: Which domain the entity belongs to (e.g., `ores.refdata`, `ores.iam`).
-   **Menu name**: The shell menu name (typically plural, e.g., `currencies`, `accounts`).
-   **Operations needed**:
    -   [ ] List/get all entities
    -   [ ] Add/create new entity
    -   [ ] History (version tracking)
    -   [ ] Update/modify entity
    -   [ ] Delete entity
    -   [ ] Custom operations
-   **Authentication**: Which operations require authentication vs public access.
-   **Parameters**: What parameters each command accepts.


# Phase 1: List and Add Commands

This phase creates the command class with basic CRUD operations. After completing Steps 1-3, raise a PR.

**Suggested PR title:** `[shell] Add <entity> list and add commands`


## Step 1: Create Header File

Create the command class header with logger and method declarations.


### File location

`projects/ores.comms.shell/include/ores.comms.shell/app/commands/<entity>_commands.hpp`


### Header structure

```cpp
/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * licensing text...
 */
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_<ENTITY>_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_<ENTITY>_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to <entities>.
 */
class <entity>_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.<entity>_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register <entity>-related commands.
     *
     * Creates the <entities> submenu and adds <entity> operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    /**
     * @brief Process a get <entities> request.
     *
     * Retrieves all <entities> from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_get_<entities>(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add <entity> request.
     *
     * Creates a new <entity> with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param param1 Description of parameter 1
     * @param param2 Description of parameter 2
     */
    static void process_add_<entity>(std::ostream& out,
        comms::net::client_session& session,
        std::string param1, std::string param2);
};

}

#endif
```


### Key patterns

-   All methods are `static`
-   Logger uses inline static `logger_name` with fully qualified path
-   `lg()` function creates logger on first use
-   Methods take `std::ostream&` for output and `client_session&` for server communication
-   String parameters use `std::string` (moved in implementation)


### Commit message

```
[shell] Add <entity>_commands header

Declare command class with register_commands, process_get_<entities>,
and process_add_<entity> methods.
```


## Step 2: Create Implementation File

Implement the command registration and processing functions.


### File location

`projects/ores.comms.shell/src/app/commands/<entity>_commands.cpp`


### Implementation structure

```cpp
/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * licensing text...
 */
#include "ores.comms.shell/app/commands/<entity>_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.<component>/messaging/<entity>_protocol.hpp"
#include "ores.<component>/domain/<entity>_table_io.hpp" // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace logging;
using comms::messaging::message_type;
using comms::net::client_session;

void <entity>_commands::
register_commands(cli::Menu& root_menu, client_session& session) {
    auto <entity>_menu =
        std::make_unique<cli::Menu>("<entities>");

    <entity>_menu->Insert("get", [&session](std::ostream& out) {
        process_get_<entities>(std::ref(out), std::ref(session));
    }, "Retrieve all <entities> from the server");

    <entity>_menu->Insert("add", [&session](std::ostream& out,
            std::string param1, std::string param2) {
        process_add_<entity>(std::ref(out), std::ref(session),
            std::move(param1), std::move(param2));
    }, "Add a <entity> (param1 param2)");

    root_menu.Insert(std::move(<entity>_menu));
}

// ... process function implementations ...

}
```


### Command patterns

-   Pattern A: List/Get (no auth required)

    Use when the operation doesn't require authentication.
    
    ```cpp
    void <entity>_commands::
    process_get_<entities>(std::ostream& out, client_session& session) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating get <entities> request.";
    
        using <component>::messaging::get_<entities>_request;
        auto result = session.process_request(get_<entities>_request{});
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << result-><entities>.size() << " <entities>.";
        out << result-><entities> << std::endl;
    }
    ```

-   Pattern B: List/Get (authenticated)

    Use when the operation requires authentication.
    
    ```cpp
    void <entity>_commands::
    process_list_<entities>(std::ostream& out, client_session& session) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating list <entities> request.";
    
        using <component>::messaging::get_<entities>_request;
        using <component>::messaging::get_<entities>_response;
        auto result = session.process_authenticated_request<get_<entities>_request,
                                                            get_<entities>_response,
                                                            message_type::get_<entities>_request>
            (get_<entities>_request{});
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << result-><entities>.size() << " <entities>.";
        out << result-><entities> << std::endl;
    }
    ```

-   Pattern C: Add/Create with login check

    ```cpp
    void <entity>_commands::
    process_add_<entity>(std::ostream& out, client_session& session,
        std::string param1, std::string param2) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating add <entity> request for: "
                                   << param1;
    
        // Get recorded_by from logged-in user
        const auto& session_info = session.session_info();
        if (!session_info) {
            out << "✗ You must be logged in to add a <entity>." << std::endl;
            return;
        }
        const auto& recorded_by = session_info->username;
    
        using <component>::messaging::save_<entity>_request;
        using <component>::messaging::save_<entity>_response;
        auto result = session.process_authenticated_request<save_<entity>_request,
                                                            save_<entity>_response,
                                                            message_type::save_<entity>_request>
            (save_<entity>_request{
                .<entity> = <component>::domain::<entity>{
                    .field1 = std::move(param1),
                    .field2 = std::move(param2),
                    .recorded_by = recorded_by,
                    .recorded_at = std::chrono::system_clock::now()
                }
            });
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        const auto& response = *result;
        if (response.success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully added <entity>.";
            out << "✓ <Entity> added successfully!" << std::endl;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to add <entity>: "
                                      << response.message;
            out << "✗ Failed to add <entity>: " << response.message << std::endl;
        }
    }
    ```

-   Pattern D: UUID parameter validation

    Use when a command accepts a UUID parameter.
    
    ```cpp
    void <entity>_commands::
    process_get_<entity>(std::ostream& out, client_session& session,
        std::string <entity>_id) {
        boost::uuids::uuid parsed_id;
        try {
            parsed_id = boost::lexical_cast<boost::uuids::uuid>(<entity>_id);
        } catch (const boost::bad_lexical_cast&) {
            BOOST_LOG_SEV(lg(), error) << "Invalid <entity> ID format: " << <entity>_id;
            out << "✗ Invalid <entity> ID format. Expected UUID." << std::endl;
            return;
        }
    
        // Continue with request using parsed_id...
    }
    ```
    
    Required includes for UUID validation:
    
    ```cpp
    #include <boost/lexical_cast.hpp>
    #include <boost/uuid/uuid_io.hpp>
    #include <boost/uuid/uuid_generators.hpp>
    ```

-   Pattern E: Optional parameter with default

    ```cpp
    void <entity>_commands::
    process_add_<entity>(std::ostream& out, client_session& session,
        std::string required_param, std::string optional_param) {
    
        // Parse optional parameter with default
        int value = 100;  // default
        if (!optional_param.empty()) {
            try {
                value = std::stoi(optional_param);
            } catch (const std::invalid_argument&) {
                out << "✗ Invalid value: '" << optional_param
                    << "' is not a valid integer." << std::endl;
                return;
            } catch (const std::out_of_range&) {
                out << "✗ Invalid value: '" << optional_param
                    << "' is out of range." << std::endl;
                return;
            }
        }
    
        // Continue with request...
    }
    ```


### Commit message

```
[shell] Implement <entity>_commands list and add operations

Add process_get_<entities> for listing all <entities> and
process_add_<entity> for creating new <entities>.
```


## Step 3: Register in REPL

Register the command class in the REPL setup.


### File to modify

`projects/ores.comms.shell/src/app/repl.cpp`


### Changes required

1.  Add include at top of file:

```cpp
#include "ores.comms.shell/app/commands/<entity>_commands.hpp"
```

1.  Add registration in `setup_menus()` method:

```cpp
std::unique_ptr<cli::Cli> repl::setup_menus() {
    auto root =
        std::make_unique<cli::Menu>("ores-shell");

    using namespace commands;
    connection_commands::register_commands(*root, session_);
    currencies_commands::register_commands(*root, session_);
    // ... existing registrations ...
    <entity>_commands::register_commands(*root, session_);  // Add this line

    // ... rest of method
}
```


### Commit message

```
[shell] Register <entity>_commands in REPL

Add <entity> commands to shell menu hierarchy.
```


## Phase 1 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Test manually that commands appear in shell help.
3.  Test get and add operations work correctly.
4.  Commit all changes.
5.  Push branch and raise PR.

**PR Title:** `[shell] Add <entity> list and add commands`

**PR Description:**

```
## Summary

- Add <entity>_commands class with list and add operations
- Register commands in REPL under `<entities>` menu
- Support authenticated requests with login check

## Test Plan

- [ ] Build succeeds
- [ ] `<entities> get` lists all <entities>
- [ ] `<entities> add` creates new <entity>
- [ ] Error handling works for invalid input
```

Wait for review feedback and merge before continuing to Phase 2.


# Phase 2: History Command

After Phase 1 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 2.

**Suggested PR title:** `[shell] Add <entity> history command`


## Step 4: Add History Command

Add history retrieval for version tracking.


### Header additions

Add to `<entity>_commands.hpp`:

```cpp
/**
 * @brief Process a <entity> history request.
 *
 * Retrieves version history for a specific <entity>.
 *
 * @param out Output stream for results
 * @param session Client session for connectivity.
 * @param <entity>_id The <entity> identifier to get history for.
 */
static void process_<entity>_history(std::ostream& out,
    comms::net::client_session& session,
    std::string <entity>_id);
```


### Implementation additions

Add to `<entity>_commands.cpp`:

-   Formatting helpers (in anonymous namespace)

    ```cpp
    namespace {
    
    std::string format_time(std::chrono::system_clock::time_point tp) {
        return ores::platform::time::datetime::format_time_point(tp);
    }
    
    }  // anonymous namespace
    ```
    
    Required include:
    
    ```cpp
    #include "ores.platform/time/datetime.hpp"
    ```

-   History command registration

    Add to `register_commands()`:
    
    ```cpp
    <entity>_menu->Insert("history", [&session](std::ostream& out,
            std::string <entity>_id) {
        process_<entity>_history(std::ref(out), std::ref(session),
            std::move(<entity>_id));
    }, "Show history for a <entity> (<entity>_id)");
    ```

-   History implementation

    ```cpp
    void <entity>_commands::
    process_<entity>_history(std::ostream& out, client_session& session,
        std::string <entity>_id) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating <entity> history request for: "
                                   << <entity>_id;
    
        using <component>::messaging::get_<entity>_history_request;
        using <component>::messaging::get_<entity>_history_response;
        auto result = session.process_authenticated_request<get_<entity>_history_request,
                                                            get_<entity>_history_response,
                                                            message_type::get_<entity>_history_request>
            (get_<entity>_history_request{.<entity>_id = <entity>_id});
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        const auto& history = result->history;
        BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                                  << history.size() << " history entries.";
    
        if (history.empty()) {
            out << "No history found for <entity>: " << <entity>_id << std::endl;
            return;
        }
    
        out << "History for <entity> " << <entity>_id << " ("
            << history.size() << " versions):" << std::endl;
        out << std::string(80, '-') << std::endl;
    
        for (const auto& entry : history) {
            out << "  Version " << entry.version << std::endl;
            out << "    Recorded: " << format_time(entry.recorded_at)
                << " by " << entry.recorded_by << std::endl;
            // Add entity-specific fields as needed
            out << std::endl;
        }
    }
    ```


### Commit message

```
[shell] Add <entity> history command

Implement process_<entity>_history for viewing version history with
formatted timestamps and version details.
```


## Step 5: Add Additional Operations (Optional)

Add any additional operations specific to the entity.


### Common additional operations

-   Delete operation

    ```cpp
    void <entity>_commands::
    process_delete_<entity>(std::ostream& out, client_session& session,
        std::string <entity>_id) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating delete <entity> request for: "
                                   << <entity>_id;
    
        const auto& session_info = session.session_info();
        if (!session_info) {
            out << "✗ You must be logged in to delete a <entity>." << std::endl;
            return;
        }
    
        using <component>::messaging::delete_<entity>_request;
        using <component>::messaging::delete_<entity>_response;
        auto result = session.process_authenticated_request<delete_<entity>_request,
                                                            delete_<entity>_response,
                                                            message_type::delete_<entity>_request>
            (delete_<entity>_request{.<entity>_id = <entity>_id});
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        const auto& response = *result;
        if (response.success) {
            BOOST_LOG_SEV(lg(), info) << "Successfully deleted <entity>: "
                                      << <entity>_id;
            out << "✓ <Entity> deleted successfully!" << std::endl;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Failed to delete <entity>: "
                                      << response.message;
            out << "✗ Failed to delete <entity>: " << response.message << std::endl;
        }
    }
    ```

-   Get single entity

    ```cpp
    void <entity>_commands::
    process_get_<entity>(std::ostream& out, client_session& session,
        std::string <entity>_id) {
        BOOST_LOG_SEV(lg(), debug) << "Initiating get <entity> request for: "
                                   << <entity>_id;
    
        using <component>::messaging::get_<entity>_request;
        using <component>::messaging::get_<entity>_response;
        auto result = session.process_authenticated_request<get_<entity>_request,
                                                            get_<entity>_response,
                                                            message_type::get_<entity>_request>
            (get_<entity>_request{.<entity>_id = <entity>_id});
    
        if (!result) {
            out << "✗ " << comms::net::to_string(result.error()) << std::endl;
            return;
        }
    
        if (!result-><entity>) {
            out << "✗ <Entity> not found: " << <entity>_id << std::endl;
            return;
        }
    
        const auto& entity = *result-><entity>;
        out << "<Entity> Details:" << std::endl;
        out << "  ID: " << entity.id << std::endl;
        out << "  Field1: " << entity.field1 << std::endl;
        // ... additional fields
    }
    ```


### Commit message

```
[shell] Add <entity> delete and get commands

Implement delete and single-entity retrieval operations.
```


## Phase 2 Checkpoint: Raise PR

At this point:

1.  Build and verify: `cmake --build --preset linux-clang-debug`
2.  Test history command with existing entity.
3.  Test any additional operations added.
4.  Commit all changes.
5.  Push branch and raise PR.

**PR Title:** `[shell] Add <entity> history command`

**PR Description:**

```
## Summary

- Add history command for viewing <entity> version history
- Add formatted output with timestamps and version details

## Test Plan

- [ ] Build succeeds
- [ ] `<entities> history <id>` shows version history
- [ ] History displays correct timestamps and recorded_by
```

Wait for review feedback and merge before continuing to Phase 3.


# Phase 3: Recipe Documentation

After Phase 2 PR is merged, use [feature-branch-manager](../feature-branch-manager/SKILL.md) to transition to Phase 3.

**Suggested PR title:** `[doc] Add <entity> shell command recipes`


## Step 6: Add Shell Recipes

Add all new commands to shell recipes using the [ores-shell-recipes](../ores-shell-recipes/SKILL.md) skill. This ensures every command is documented with runnable examples that serve as both documentation and tests.


### File to modify

`doc/recipes/shell_recipes.org`


### Recipe requirements

Create recipes that test **every command** added for the entity:

1.  **Help recipe**: Show the entity menu help output
2.  **List recipe**: Demonstrate listing all entities
3.  **Add recipe**: Demonstrate creating a new entity
4.  **History recipe**: Demonstrate viewing entity history
5.  **Any additional commands**: Delete, get single, etc.


### Recipe section structure

Add a new section for the entity following this pattern:

\#+begin\_src fundamental


## <Entities>

This section demonstrates <entity> management commands.


### <Entities> Help

Display all available <entity> commands.

```sh
./ores.comms.shell ${log_args} ${connect_args} << 'EOF'
<entities> help
exit
EOF
```


### List All <Entities>

Retrieve all <entities> from the server.

```sh
./ores.comms.shell ${log_args} ${connect_args} << 'EOF'
<entities> get
exit
EOF
```


### Add <Entity>

Create a new <entity>. Requires authentication.

```sh
export ORES_SHELL_LOGIN_PASSWORD
./ores.comms.shell ${log_args} ${connect_args} ${login_args} << 'EOF'
<entities> add param1 param2
exit
EOF
```


### <Entity> History

View version history for a <entity>. Requires authentication.

```sh
export ORES_SHELL_LOGIN_PASSWORD
./ores.comms.shell ${log_args} ${connect_args} ${login_args} << 'EOF'
<entities> history <entity_id>
exit
EOF
```

\#+end\_src


### Recipe conventions

-   Use `:exports none` on the ores-shell blocks (they are included via noweb)
-   Add `login_args` property for authenticated commands
-   Include `export ORES_SHELL_LOGIN_PASSWORD` for login commands
-   Use `:wrap example` to format output
-   Name blocks with pattern `<entities>-<action>`


### Testing recipes

Before committing, test all recipes by running them in the shell documentation:

```sh
cd doc/recipes
emacs --batch -l ~/.emacs.d/init.el shell_recipes.org \
    --eval '(org-babel-execute-buffer)'
```

Or manually test each command in the shell.


### Commit message

```
[doc] Add <entity> shell command recipes

Document all <entity> shell commands with runnable examples:
- Help menu
- List all <entities>
- Add new <entity>
- View <entity> history
```


## Phase 3 Checkpoint: Raise PR

At this point:

1.  Verify recipes execute correctly.
2.  Check output formatting is correct.
3.  Commit all changes.
4.  Push branch and raise PR.

**PR Title:** `[doc] Add <entity> shell command recipes`

**PR Description:**

```
## Summary

- Add shell recipes section for <entity> commands
- Document help, list, add, and history operations
- Include runnable examples with expected output

## Test Plan

- [ ] All recipes execute without errors
- [ ] Output matches expected format
- [ ] Documentation renders correctly
```


# Key conventions reference


## Output formatting

| Outcome | Format                                  |
|------- |--------------------------------------- |
| Success | `out << "✓ Message" << std::endl;`      |
| Error   | `out << "✗ " << error << std::endl;`    |
| List    | `out << result->items << std::endl;`    |
| Count   | `out << "Retrieved " << n << " items."` |


## Logging levels

| Level | Use Case                              |
|----- |------------------------------------- |
| debug | Operation start, parameter values     |
| info  | Successful completion with counts/IDs |
| warn  | Operation failed but handled          |
| error | Invalid input, unexpected state       |


## Request patterns

| Pattern                         | Use Case                    |
|------------------------------- |--------------------------- |
| `process_request`               | Public operations (no auth) |
| `process_authenticated_request` | Operations requiring login  |


## Parameter handling

| Type     | Pattern                                     |
|-------- |------------------------------------------- |
| String   | `std::string param` then `std::move(param)` |
| Optional | Empty string check with default value       |
| UUID     | `boost::lexical_cast` with try/catch        |
| Integer  | `std::stoi` with try/catch                  |


## Related skills

-   [domain-type-creator](../domain-type-creator/SKILL.md) - For creating the underlying domain type
-   [binary-protocol-developer](../binary-protocol-developer/SKILL.md) - For creating messaging protocol
-   [feature-branch-manager](../feature-branch-manager/SKILL.md) - For transitioning between phases
-   [ores-shell-recipes](../ores-shell-recipes/SKILL.md) - For updating shell documentation
