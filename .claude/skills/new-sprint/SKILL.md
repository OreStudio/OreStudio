---
name: new-sprint-skill
description: Open a new ORE Studio sprint by updating all required files.
license: Complete terms in LICENSE.txt
---

This skill updates all files related to version and task management for a new Agile sprint in the ORE Studio project.


# When to use this skill

When the user requests to open a new sprint for the ORE Studio project. This involves updating version numbers across multiple files and creating a new sprint backlog document.


# How to use this skill


## Workflow summary

1.  Ask the user if a new branch is required.
2.  Determine the current and new sprint numbers.
3.  Confirm the new sprint number with the user.
4.  Follow the detailed instructions to update all required files.
5.  Commit and raise a PR.


## Files to update

| File                                       | Change                                 |
|------------------------------------------ |-------------------------------------- |
| `readme.org`                               | Sprint badge, commits-since, version   |
| `CMakeLists.txt`                           | Project VERSION                        |
| `vcpkg.json`                               | version-string                         |
| `.github/workflows/continuous-linux.yml`   | Package filename version               |
| `.github/workflows/continuous-macos.yml`   | Package filename version               |
| `.github/workflows/continuous-windows.yml` | Package filename version               |
| `doc/agile/v0/version_zero.org`            | Add sprint backlog link                |
| `doc/agile/v0/sprint_backlog_NN.org`       | **New file** - sprint backlog document |


# Detailed instructions


## Step 1: Determine branch strategy

Ask the user if a new branch is required:

-   If yes: create `feature/new_sprint` branched off `main`
-   If no: use the current branch

```sh
git fetch origin main && git checkout -b feature/new_sprint origin/main
```


## Step 2: Determine sprint numbers

Look at files in `doc/agile/v0` with names matching `sprint_backlog_NN.org`.

-   The file with the highest `NN` is the **current sprint**
-   The **new sprint** is current + 1

Example: If `sprint_backlog_10.org` exists and is the highest, current sprint is 10 and new sprint is 11.


## Step 3: Confirm with user

Confirm the new sprint number with the user before proceeding.


## Step 4: Update readme.org

Make three changes to `readme.org`:


### 4.1: Update sprint badge

Change the sprint number in the badge URL and text:

```diff
-#+html: <a href="https://orestudio.github.io/OreStudio/doc/agile/v0/sprint_backlog_10.html"><img alt="Agile Sprint" src="https://img.shields.io/badge/Sprint-10-blue.svg"/></a>
+#+html: <a href="https://orestudio.github.io/OreStudio/doc/agile/v0/sprint_backlog_11.html"><img alt="Agile Sprint" src="https://img.shields.io/badge/Sprint-11-blue.svg"/></a>
```


### 4.2: Update commits-since badge

Update to the **previous** sprint version (current sprint, not new sprint):

```diff
-#+html: <a href="https://github.com/OreStudio/OreStudio/commits/main"><img alt="Commits" src= "https://img.shields.io/github/commits-since/OreStudio/OreStudio/v0.0.9.svg"/></a>
+#+html: <a href="https://github.com/OreStudio/OreStudio/commits/main"><img alt="Commits" src= "https://img.shields.io/github/commits-since/OreStudio/OreStudio/v0.0.10.svg"/></a>
```


### 4.3: Update version text

Update the example version in the documentation:

```diff
-Where =${VERSION}= is your ORE Studio version, such as =0.0.10=.
+Where =${VERSION}= is your ORE Studio version, such as =0.0.11=.
```


## Step 5: Update CMakeLists.txt

Update the project VERSION in the top-level `CMakeLists.txt`:

```diff
-project(OreStudio VERSION 0.0.10 LANGUAGES CXX
+project(OreStudio VERSION 0.0.11 LANGUAGES CXX
```


## Step 6: Update vcpkg.json

Update the version-string in `vcpkg.json`:

```diff
-    "version-string": "0.0.10",
+    "version-string": "0.0.11",
```


## Step 7: Update GitHub workflow files

Update the package version in all three workflow files under `.github/workflows/`:


### 7.1: continuous-linux.yml

```diff
-          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/orestudio_0.0.10_amd64.deb
+          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/orestudio_0.0.11_amd64.deb
```


### 7.2: continuous-macos.yml

```diff
-          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/OreStudio-0.0.10-Darwin.dmg
+          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/OreStudio-0.0.11-Darwin.dmg
```


### 7.3: continuous-windows.yml

```diff
-          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/OreStudio-0.0.10-win64.*
+          path: ./build/output/${{matrix.family}}-${{matrix.compiler}}-${{matrix.buildtype}}/packages/OreStudio-0.0.11-win64.*
```


## Step 8: Generate UUID for new sprint backlog

Generate a new UUID using the `uuid` command:

```sh
uuid
```

Save this UUID for use in steps 9 and 10.


## Step 9: Update version\_zero.org

Add the sprint backlog link to the appropriate sprint section in `doc/agile/v0/version_zero.org`:

```diff
 ** Sprint 10
    :PROPERTIES:
    :resource_id: s10
    :END:

+- [[id:A734018F-A4A7-F8A4-8F5B-DE0C74344940][Sprint Backlog 10]] | Data quality and code generation work.
+
 ** Sprint 11
    :PROPERTIES:
    :resource_id: s11
    :END:

+- [[id:NEW-UUID-HERE][Sprint Backlog 11]] | TBD.
+
 ** Sprint 12
```

Use the UUID generated in Step 8 for the new sprint backlog link.


## Step 10: Create sprint backlog file

Create a new file `doc/agile/v0/sprint_backlog_NN.org` using the template below.


### 10.1: Create template and stage

First create the file with the template content and git stage it:

```sh
git add doc/agile/v0/sprint_backlog_NN.org
```


### 10.2: Update template values

Then update the following in the file:

-   Replace `GUID` with the UUID from Step 8 (uppercase)
-   Replace all `05` references with the new sprint number (`NN`)
-   Update the `#+title:` to `Sprint Backlog NN`
-   Update all filename references (`sprint_backlog_05_*` to `sprint_backlog_NN_*`)
-   Update all ggtitle references (`"Sprint 5:"` to `"Sprint NN:"`)
-   Update the clock table dates to today's date
-   Set the sprint mission to `TBD.`


## Step 11: Verify changes

Review all changes with:

```sh
git status
git diff --stat HEAD
```


## Step 12: Commit and raise PR

Stage all changes, commit, and raise a PR:

```sh
git add -A
git commit -m "[agile] Open Sprint NN"
git push -u origin feature/new_sprint
gh pr create --title "[agile] Open Sprint NN" --body "Opens Sprint NN for development."
```
