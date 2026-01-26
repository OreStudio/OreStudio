---
name: ores-comms-analyser-recipes
description: Update comms analyser recipes by comparing commands to the recipe documentation.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   The user wants to update the ORE Studio Comms Analyser recipes documentation
-   New analyser commands have been added and need to be documented
-   Existing analyser functionality has changed and recipes need updating


# How to use this skill

1.  Compare the commands in `projects/ores.comms.analyser/src/config/parser.cpp` to `doc/recipes/comms_analyser_recipes.org`.
2.  Check `projects/ores.comms.analyser/src/app/application.cpp` for available commands and their implementations.
3.  Identify any missing commands that are not documented.
4.  Add recipes for missing commands following the existing pattern.
5.  Use the Detailed Instructions section for the proper recipe format.


# Detailed instructions


## Recipe file location

The comms analyser recipes are stored in `doc/recipes/comms_analyser_recipes.org`. This file demonstrates all functionality available in the ORE Studio Comms Analyser tool.


## Command source locations

The comms analyser is implemented in the following files:

-   `projects/ores.comms.analyser/src/config/parser.cpp`: Command-line argument parsing
-   `projects/ores.comms.analyser/src/app/application.cpp`: Command implementations
-   `projects/ores.comms.analyser/src/domain/session_reader.cpp`: Session file reading


## Available commands

Currently supported commands:

-   `read`: Read and display all frames in a session file (default)
-   `info`: Display session metadata only (file, session ID, timestamp, server, protocol)


## Recipe format

Each recipe follows this pattern:

\#+begin\_src fundamental


### Recipe Name

Description of what this recipe demonstrates.

```sh
./ores.comms.analyser command /path/to/session.ores
```

[Sample output example if helpful] \#+end\_src


## Recipe sections

Recipes are organized into sections:

-   **General**: Help, version, and basic options
-   **Reading Session Files**: Commands for reading and analysing .ores files
-   **Understanding the Output**: Explanation of output columns and fields


## Adding a new recipe

1.  Identify which section the command belongs to.
2.  Create a sh src block that invokes ores.comms.analyser with appropriate arguments.
3.  Add a descriptive paragraph explaining what the recipe demonstrates.
4.  Include sample output if it helps illustrate the functionality.
5.  Document any relevant command-line options.


## Session file format

Session files (.ores) are binary files that contain:

-   Session metadata header (session ID, start time, server address, protocol version)
-   Recorded frames with timestamps and direction information

The analyser reads these files and displays them in a human-readable format similar to packet capture tools like Wireshark.
