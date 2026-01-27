---
name: recipe-author
description: Create new recipe files and recipe-updating skills for ORE Studio topics.
license: Complete terms in LICENSE.txt
---


# When to use this skill

Use this skill when:

-   The user wants to add recipes for a new topic or subsystem
-   A new component needs executable documentation
-   The user wants to create a skill to maintain existing recipes
-   You need to understand the recipe and recipe-skill patterns


# How to use this skill

1.  Identify the topic for the new recipes (e.g., SQL, shell, CLI, git).
2.  Create the recipe file following the established pattern.
3.  Optionally create a recipe-updating skill if the recipes have a source of truth that needs synchronization.
4.  Link the new recipes to the main index and related documentation.
5.  See Detailed instructions for the complete process.


# Detailed instructions


## Recipe file overview

Recipes are executable org-mode notebooks stored in `doc/recipes/`. They demonstrate functionality through runnable code blocks with captured results.


## Existing recipe files

| File                | Topic                   | Source of truth          |
|------------------- |----------------------- |------------------------ |
| `shell_recipes.org` | ORE Studio Shell (REPL) | C++ command files        |
| `cli_recipes.org`   | ORE Studio CLI          | C++ entity parser files  |
| `http_recipes.org`  | HTTP API (verb mode)    | C++ route files          |
| `sql_recipes.org`   | SQL database queries    | SQL schema files         |
| `git_recipes.org`   | Git operations          | None (general reference) |


## Creating a new recipe file


### Step 1: Create the org-mode file

Create `doc/recipes/TOPIC_recipes.org` with this structure:

```fundamental
:PROPERTIES:
:ID: GUID
:END:
#+title: TOPIC Recipes
#+author: Marco Craveiro
#+startup: inlineimages
#+options: <:nil c:nil todo:nil ^:nil d:nil date:nil author:nil toc:nil html-postamble:nil

Brief description of what these recipes demonstrate.

* Recipes
  :PROPERTIES:
  :header-args: :exports both
  :header-args+: :results raw
  :header-args+: :dir PATH-TO-EXECUTABLES
  :END:

** Section Name

*** Recipe Name

Description of what this recipe demonstrates.

#+begin_src LANGUAGE :wrap WRAPPER
command or code here
```

|                                              |
|--------------------------------------------- |
| Previous: [Recipes](../../recipes/recipes.md) |

\#+end\_src

Replace:

-   `GUID`: Generate with `python3 -c "import uuid; print(uuid.uuid4())"`
-   `TOPIC`: The topic name (e.g., SQL, Shell, CLI)
-   `PATH-TO-EXECUTABLES`: Relative path to binaries if needed
-   `LANGUAGE`: Language for code blocks (sh, sql, python, etc.)
-   `WRAPPER`: Output wrapper (example, src json, src xml, etc.)


### Step 2: Organize recipes into sections

Group related recipes under `**` headings:

-   **General**: Help, version, basic operations
-   **Entity Operations**: CRUD for each entity type
-   **Advanced**: Complex queries, administrative tasks


### Step 3: Recipe block patterns

For shell commands:

```fundamental
*** Command Name

Description.

#+name: recipe-name
#+begin_src REPL-MODE :exports none
command1
command2
exit
```

```sh
./executable ${args} << 'EOF'
*** Command Name

Description.

#+name: recipe-name
#+begin_src REPL-MODE :exports none
command1
command2
exit
EOF
```

\#+end\_src

For direct SQL:

```fundamental
*** Query Name

Description.

#+begin_src sql
SELECT * FROM table WHERE condition;
```

\#+end\_src

For CLI tools:

```fundamental
*** Operation Name

Description.

#+begin_src sh :wrap src json
./ores.cli entity operation ${args} --option value
```

\#+end\_src


### Step 4: Update the recipes index

Add a link in `doc/recipes/recipes.org`:

```fundamental
- [[id:YOUR-GUID][TOPIC Recipes]]: Brief description.
```


### Step 5: Cross-link to related documentation

Add navigation links at the bottom of your recipe file pointing to:

-   The main recipes index
-   Related modeling or component documentation


## Creating a recipe-updating skill

If your recipes have a source of truth (e.g., C++ source files, SQL schemas), create a corresponding skill to help maintain synchronization.


### When to create a recipe skill

Create a skill when:

-   Recipes document features implemented elsewhere (code, schemas)
-   The source of truth changes independently of recipes
-   You want automated help detecting out-of-sync documentation

Don't create a skill when:

-   Recipes are standalone reference material (like git tips)
-   There's no authoritative source to compare against


### Recipe skill structure

Create `doc/skills/ores-TOPIC-recipes/skill.org`:

```fundamental
:PROPERTIES:
:ID: GUID
:END:
#+title: ORE TOPIC Recipes Skill
#+author: Marco Craveiro
#+options: <:nil c:nil todo:nil ^:nil d:nil date:nil author:nil toc:nil
#+startup: inlineimages
#+export_exclude_tags: noexport

#+begin_export markdown
---
name: ores-TOPIC-recipes
description: Update TOPIC recipes by comparing SOURCE to recipe documentation.
license: Complete terms in LICENSE.txt
---
#+end_export

* When to use this skill

Use this skill when:
- The user wants to update the ORE Studio TOPIC recipes documentation
- New TOPIC features have been added and need to be documented
- Existing features have changed and recipes need updating

* How to use this skill

1. Compare the SOURCE files to =doc/recipes/TOPIC_recipes.org=.
2. Identify any missing features that are not documented.
3. Add recipes for missing features following the existing pattern.
4. Use the Detailed Instructions section for the proper recipe format.

* Detailed instructions

** Recipe file location

The TOPIC recipes are stored in =doc/recipes/TOPIC_recipes.org=.

** Source locations

SOURCE files are located in:
- =path/to/source1.EXT=: Description
- =path/to/source2.EXT=: Description

** Recipe format

[Document the specific format for this recipe type]

** Recipe sections

[Document the sections in this recipe file]

** Adding a new recipe

[Step-by-step instructions for adding recipes]

* Artefacts                                                        :noexport:

** Licence

#+BEGIN_SRC fundamental :tangle LICENCE.txt
This program is free software; you can redistribute it and/or modify
...
#+END_SRC
```


### Skill naming convention

Use `ores-TOPIC-recipes` for consistency with existing skills:

-   [ores-shell-recipes](../ores-shell-recipes/SKILL.md)
-   [ores-cli-recipes](../ores-cli-recipes/SKILL.md)
-   `ores-sql-recipes` (if created)


## Existing recipe skills

| Skill                                                | Recipes file        | Compares against                                |
|---------------------------------------------------- |------------------- |----------------------------------------------- |
| [ores-shell-recipes](../ores-shell-recipes/SKILL.md) | `shell_recipes.org` | `projects/ores.comms.shell/src/app/commands/*`  |
| [ores-cli-recipes](../ores-cli-recipes/SKILL.md)     | `cli_recipes.org`   | `projects/ores.cli/src/config/entity_parsers/*` |


## Best practices

1.  **Executable examples**: All recipe code blocks should be runnable
2.  **Captured output**: Include `#+RESULTS:` blocks showing expected output
3.  **Clear descriptions**: Explain what each recipe demonstrates
4.  **Logical grouping**: Organize recipes by feature area
5.  **Cross-references**: Link to related documentation and components
6.  **Temporal patterns**: For SQL recipes, always include the `valid_to = '9999-12-31 23:59:59'::timestamptz` filter for current records
