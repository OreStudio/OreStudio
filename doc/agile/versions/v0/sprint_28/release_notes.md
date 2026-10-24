*October 2026*

Sprint 28 set out to finish the work carried from sprint 27 and to add no new scope. It did not keep that promise or its size. The sprint ran twelve days instead of seven, merged 432 pull requests after the v0.0.27 tag and made about 2,500 commits, so its load was RED against every target. Most of the carried stories shipped in part: the XML strategy, the instrument table rework, the conventions refactor, the market data service, the engine settings and the grid job screens are in. Eight in ten stories were new, and the report page with its six tabs is not built. At the close we split every unfinished story. Each keeps its finished tasks here, and the rest, 117 tasks in 24 stories plus 37 stories that never started, is in the product backlog.

---


# ✅ Highlights

-   **ORE XML strategy settled and built.** Tables generated from the 28 ORE schemas replace xsdcpp. A faithful document layer, an exporter and a round-trip proof are in, and the portfolio read is cheaper.
-   **Instrument tables reworked.** FX, equity and credit follow the instrument table rule. The rates family carries its fields across the ORE boundary, with caps, floors, collars and the inflation swap proved against the document's own values.
-   **Conventions managed as families and bundles.** A convention is declared once in its own entity. The list screens and the tenor resolution screen are built on the new shape.
-   **Market data service with adapters.** ores.marketdata owns the catalogue, the mapping, the ingest path, routing and metering. A bindings screen lists each source, rebinds a party and starts or stops its feed, and feeds restart the same way every time.
-   **Engine settings and 610 ORE parameters.** The ORE parameters now reach every tenant through the data quality pipeline, with documentation, party rules, a history and a refresh that follow the screen standard.
-   **Grid jobs and reports.** A person browses the grid's batches, workunits and results, triggers a report on demand or by schedule, and manages the reports a party runs from one list.
-   **CI cheaper and steadier.** The continuous Linux jobs carry less work, share one compiler cache and save it on every exit. The nightly builds report under their own CDash group.


# 🛠️ Key Improvements

A story marked "Partly delivered" closed with its finished tasks. The count shows how much of its first plan it delivered. The rest is in the product backlog as "the remaining work".


## Hotfixes

-   **Hotfix: the debug builds never finish**: The debug legs finish.
-   **Hotfix: the services die on shutdown**: A service survives stopping.
-   **Hotfix: the site stops on a link it cannot resolve**: The documentation site builds.
-   **Hotfix: gcc rejects member names that match their type in the generated xml headers**: The nightly gcc builds compile the generated xml headers again.
-   **Hotfix: the compiler cache is too small for the builds it serves**: The Linux builds keep a compiler cache that is as big as the build, inside the 10 GB repository allowance.
-   **Hotfix: the Windows debug builds never pass**: The nightly Windows workflow builds release only.
-   **Hotfix: fix the main CI failures**: Four failures on main are fixed: the gcc range-loop error in the XML document tests, the incomplete trade type on Windows, the workflow engine test abort and the broken site link.
-   **Hotfix: fix the Linux sccache save**: The debug builds save their compiler cache without a tar race, and the old cache stays until the new one is saved.


## Financial Features

-   **Rework the FX instruments to the instrument table rule**: Forwards, swaps, vanilla, barrier, digital, Asian, accumulator and variance products follow the instrument table rule, as the bonds do.
-   **Rework the equity instruments to the instrument table rule**: Options, forwards, swaps, variance swaps, barriers, digitals, Asians, accumulators and positions follow the instrument table rule.
-   **Carry the rates family's fields across the ORE boundary**: A rates trade that the document states is the trade the store holds and the document gets back. Every cap and floor of a collar is kept, and the inflation swap keeps its index.
-   **Close the credit loose ends**: The three gaps the credit rework left open are closed.
-   **Prove the credit round trip over the corpus**: Every trade of the credit corpus survives import, export and re-import.
-   **Replace the credit fact variant with typed arrays**: The credit carrier holds one typed array per product type instead of a variant. The consumers read through typed accessors, and a type that is not a credit fact fails at compile time. The rates and equity variants stay with their own stories.
-   **Refactor conventions onto families and bundles** (Partly delivered, 7 of 10 tasks): A convention is declared once, in its own entity, and carries only its own fields.
-   **Atomise every convention onto the central entity** (Partly delivered, 2 of 3 tasks): The tenor basis swap pilots the atomised convention, and its publish path is restored. The composite family get and put is not built.
-   **Fix the tenor basis swap convention model and the document TU** (Partly delivered, 2 of 4 tasks): The investigation of the configuration document protocol is done. The remodel onto field groups and the split of the protocol are not.
-   **Populate the ACME books with a realistic trade population** (Partly delivered, 18 of 24 tasks): A stable, present-relative trade population generated from ores.synthetic fills the ACME books, starting with bonds. The investigation this story commissioned is Investigation: Populating ACME with a trade population.


## Market Data & Curves

-   **Rework the market data feeds into a market data service with adapters** (Partly delivered, 20 of 36 tasks): ores.marketdata owns the catalogue, the mapping, the ingest path, routing and metering. The adapters for public sources are not built.
-   **Build the bindings screen and clear the oresmd snags** (Partly delivered, 15 of 16 tasks): A person lists bindings, says what each source writes and when it last ticked, binds and rebinds a party to a source, enables and disables a binding, and starts or stops the feed behind it. The language control for the site is not built.
-   **Feeds start and restart the same way every time**: The stop and start faults of every feed group are closed, so each group shows the documented behaviour.
-   **Tenant auto-start feeds are not restored after a services restart**: After a services restart every tenant's auto-start feeds run again, so ingest stores ticks and the plots fill.
-   **Cut the feed ingest round trips**: The database round trips that the feed ingest pays per stored tick are fewer, and a measurement proves it.
-   **Find why market data ticks lag the wall clock** (Partly delivered, 10 of 31 tasks): The investigation found where the backlog of ticks builds up. The per-row costs of the ingest are removed, and a quiet loop runs in one thread. The remaining 20 tasks, among them the tick tables, the lanes and the history call, are not closed.


## Security & Tenancy

-   **A feed runs as the tenant that owns it**: A feed acts for its owning tenant with the narrowest authority, whoever or whatever started it.


## Compute & Storage

-   **Trigger a report run on demand or on a schedule**: A person runs a report now, or a schedule runs it later, and both paths share one handler.
-   **Prove the report grid run end to end** (Partly delivered, 4 of 8 tasks): The trades export as the ORE portfolio the engine reads, the compute results come back, an engine error counts as a failed result, and the risk report configurations are seeded. The proof that the run completes and repeats from a recreated database is not done.
-   **Run a report end to end on a synthetic bond population** (Partly delivered, 4 of 5 tasks): The NPV report of an ACME party runs from the trigger to a completed report instance, and each issue met is written down with its cause and its fix or its owner. The grid views of the run are not built.
-   **Gather each trade set and its market data** (Partly delivered, 1 of 8 tasks): Each convention links to its market data series. The trade sets, their archives and the package assembly are not built.


## Web & Journeys

-   **Prototype the management of conventions**: A person sees, from clickable prototypes, how conventions are managed.
-   **Build the conventions list screens**: The list of kinds and the list of documents of a kind are built in English, French and Portuguese, and Define instrument conventions opens from the Workflows menu.
-   **A trader screen that resolves a tenor to a date, before any editing**: A trader asks what date a tenor gives on a horizon date, and the screen answers before any editing.
-   **Test the tenor rule before saving it**: A person authoring a tenor's resolution reads the date it produces against the draft they typed, before they write it.
-   **Preview a convention's term structure on a calendar** (Partly delivered, 2 of 3 tasks): One call resolves a convention's tenors. The page that shows the term structure on a calendar is not built.
-   **Close the tenor journey and the remaining refdata gaps** (Partly delivered, 3 of 10 tasks): The tenor resolution engine, the resolve operation and its screen are built. The party membership versions, the convention family mapping and term rules, and the party reads are not.
-   **See and change everything a report runs** (Partly delivered, 18 of 19 tasks): A person sees and changes the engine settings of a report in the party that owns them. 610 ORE parameters are documented and published through the data quality pipeline. The report page with its six tabs is not built.
-   **Manage reports from the list a party runs** (Partly delivered, 4 of 6 tasks): The list of reports a party runs, its detail and its edit form are built, with the grants, the filter, the latest run and the next fire time. Amending a definition of another party is not done.
-   **Browse and follow the grid's jobs** (Partly delivered, 19 of 24 tasks): A person sees the batches, workunits and results the session may see, as a tree with the details of each record.
-   **Keep the record screens up to date** (Partly delivered, 5 of 16 tasks): The browser holds one change stream for the signed-in session, and the record screens watch it. Naming the changed record in each event is not done.
-   **Build the retire or reset tenant journey** (3 of 4 tasks): The three finished tasks concern the ACME risk report configurations: the generated output is committed, every seeded report is configured, and an invalid scaffold is removed. The retire and reset operations, which give the story its name, are not landed.
-   **Draw every coded value as its badge**: A code is shown the way the rest of the platform shows it.
-   **Trade Blotter** (Partly delivered, 5 of 13 tasks): The prototypes meet the entity screen specification, the operational authorisation kind is seeded and the trade list rows are read. The blotter screen itself is not built.
-   **Give the web one way to handle time and timestamps**: One place writes, reads and converts a timestamp in ores.web.


## Codegen & Tooling

-   **Decide the XML strategy for ORE documents**: Tables generated from the ORE schemas, a faithful document layer and an exporter replace xsdcpp, settled by measurement.
-   **Add entity groups and finish the remaining codegen work** (Partly delivered, 2 of 13 tasks): The umbrella header is a permanent artefact, and the codegen snags the parts guard left are cleared. The model drift fixes and the literate template split are not done.
-   **Bring the service code onto the NATS patterns** (Partly delivered, 1 of 3 tasks): The step commands have one consumer per service. The audit of the call sites against the pattern library is not done.
-   **Fix the failing local gate checks**: The local gate is green for reasons that hold on any machine.
-   **Re-tangle the shell scripts the report trigger work left behind**: The shell script library matches what the recipes tangle to.
-   **Prune the branches that are merged into origin/main**: compass branches prune deletes only branches that are ancestors of origin/main.


## Data, SQL & Quality

-   **Provision the streams services durably consume**: Provisioning creates the JetStream streams, and stale overlapping streams no longer stop a service starting.
-   **Move the lookup artefact tables and settle the remaining DQ artefact models**: The fifteen lookup artefact tables come from domain models, the owner of the DQ-only models is settled, and the geo ip2country staging table is generated from a model.
-   **Carry six-digit microseconds in every timestamp the platform writes**: A time point survives a round trip through the database or the wire to the microsecond.
-   **Audit the FX instrument family against the data-oriented design guidelines**: The review states whether the FX data model meets the guidelines, and the rework story carries the findings.
-   **Audit the rates instrument family against the data-oriented design guidelines**: The review states whether the rates data model meets the guidelines, and the findings are recorded.


## Build, CI & Testing

-   **Investigate the broken Linux and macOS CI**: Continuous Linux and Continuous macOS pass on main again.
-   **Rebalance the CI tiers: Linux clang continuous, macOS and Windows nightly**: Continuous Linux carries only the two linux-clang jobs, and runs as often as their run time allows.
-   **Cut the work the continuous CI jobs carry**: Continuous Linux builds and tests without coverage and without a package.
-   **Cut the continuous build time**: The continuous linux-clang jobs finish inside the six-hour limit on a cold runner.
-   **Keep the compiler cache warm when a continuous job is cancelled**: A cancelled or timed-out run still saves the compiler objects it made.
-   **Give the cache save time to run after the build stops**: A run that overruns the build still has time to save its compiler cache.
-   **Cap the Nightly Build step below the six-hour job limit**: A Nightly Build that overruns stops with enough time left to save its cache.
-   **Give the Nightly Build its own cache keys**: Each workflow restores only a cache built with its own flags, and all six Linux variants fit the cache cap.
-   **Share one compiler cache between the Linux tiers**: One cache entry serves each Linux variant.
-   **Save the compiler cache instead of silently dropping it**: A run that builds enough to be worth saving stores its cache.
-   **Stop deleting the compiler cache before saving it**: A failed or interrupted save never removes the cache that exists.
-   **Prune older cache entries after a successful save**: Each cache variant holds one entry, so every platform has room for a warm cache.
-   **Report the nightly platform builds under the CDash Nightly group**: Every macOS and Windows build appears under the CDash Nightly group.
-   **Run code coverage on the Continuous debug builds**: The Continuous Linux debug build collects coverage and submits it to CDash.
-   **Restore the linux gcc release build**: Every Linux configuration is built somewhere: gcc and clang, debug and release.
-   **Remove the unused CodeQL workflow**: No CodeQL workflow remains, and the CI workflows page describes the workflows that run.
-   **Free disk space on the macOS runner**: The macOS build starts with unused toolchains and simulator runtimes removed.
-   **Silence the GNUInstallDirs uninitialized warnings**: The Linux configure step reports no uninitialized-variable warning from CMake.
-   **Silence the configure warnings on the site and macOS builds**: The site and macOS configure steps report no warning that the repository causes.
-   **Overlay the libarchive port to silence its unused variable**: The macOS vcpkg install reports no unused-variable warning from libarchive.
-   **Replace the deprecated sscanf calls**: No translation unit calls a C function that MSVC's STL deprecates, so the Windows clang build compiles with -Werror.
-   **Fix the deprecated getenv in the build stamp**: The Windows build compiles version.cpp under -Werror.
-   **Unblock the unity build by hoisting the small-kappa threshold**: The analytics quant component compiles with unity builds, and the value has one definition.
-   **Move the version stamp out of the binaries**: A person knows which commit produced a running binary, and a commit changes no compiled artefact.
-   **Fix the marketdata suite timeout**: The marketdata core suite runs serially and inside its own time limit again.
-   **Fix the macOS real parse**: The ORE XML binding and record readers compile on macOS, where libc++ deletes the floating-point from\_chars.
-   **Fix the site build id link**: The site build no longer fails on an uppercase id link to a page that defines its id in lowercase.


## Agile & Documentation

-   **Sprint health review — System 2 analysis**: A System 2 review of sprint 28 at its close. It rates the sprint RED and repairs the agile record.
-   **Open sprint 28**: The sprint, its version manifest and its index exist, the project version is 0.0.28 and the vcpkg submodule is updated.
-   **Populate sprint 28 with the carried stories**: Sprint 27's carried stories are in sprint 28 and wired into its table.


# ⚠️ Known Issues & Postponed


## Delivered in part

24 stories closed with their finished tasks. Each has a story in the product backlog named "<title> — the remaining work". The largest remainders are:

-   **Find why market data ticks lag the wall clock**: 20 tasks remain.
-   **Rework the market data feeds into a market data service with adapters**: 16 tasks remain, among them the adapters for public sources.
-   **Keep the record screens up to date**: 11 tasks remain.
-   **Add entity groups and finish the remaining codegen work**: 11 tasks remain.
-   **Close the tenor journey and the remaining refdata gaps**: 7 tasks remain.
-   **Trade Blotter**: 7 tasks remain, among them the blotter screen.
-   **Gather each trade set and its market data**: 7 tasks remain.
-   **Populate the ACME books with a realistic trade population**: 6 tasks remain.
-   **Browse and follow the grid's jobs**: 5 tasks remain.
-   **Trace a process run and the trade groups it creates**: 4 tasks remain.

The other 14 split stories each have one to four tasks remaining. The report page with its six tabs, the composite family get and put for conventions, and the audit of the NATS call sites are among them.


## Not started

37 stories moved to the product backlog with no finished task. Their live pages are in the inbox, with their identifiers unchanged. They include:

-   **Author the report configurations on guided screens** and **Complete the ACME report inputs and prove they run**. Both have pull requests merged against their tasks, but no task met its acceptance.
-   **Edit a party on a standard entity page** (ten tasks), **End a session when its window closes, not when it is unattended** and **Raise approval requests from the remaining gated write paths**.
-   **Record system events in a central event log** and **Record service lifecycle events and surface them in system health**.
-   **Stop the workflow from losing steps to a transient storm**, **Follow the child run a workflow hands off** and **Subscribe tenants to provider feeds and prove the browser at scale**.
-   **Raise a notification for a scenario waiting for a tester**, **Restore the trading guards the versioned model dropped**, **Trace a process run and the trade groups it creates** and **Find a trade by its identifiers, state, book and date from one list** closed with nothing shipped. Only a scaffold task was done, or the one task went to another story. Their real tasks moved to the backlog.
-   Eight stories that were never broken into tasks, among them **Name the convention sets a report chooses**, **Link every reference to its record** and **Paint every web badge from the catalogue**.


## Known problems

-   The goal of seven finished stories still holds the scaffold placeholder text. This page states their outcomes from their descriptions.
-   51 finished tasks carry no pull request in their record. Most are scaffold tasks.
-   The sprint overran its size by a wide margin. See the sprint health review, which rates it RED.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_28/prs_commits.png)

The sprint ran twelve days of work against a plan of seven, and every day merged pull requests. The peak is 2026-10-17: 85 merges and 403 commits,, the day with the most merged work of the sprint. 2026-10-18 follows with 68 merges. The rate falls after 2026-10-19, which is the planned close, but it does not stop: the last four days still merge 130 pull requests. The high commits-to-merge ratio on most days is the mark of a sprint that grew past its plan.


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_28/line_churn.png)

Three days stand out. 2026-10-14 added 146,657 lines. 2026-10-17 added 151,669 lines and removed 42,255. 2026-10-19 removed 171,047 lines against 39,709 added, the deletion of the xsdcpp-generated domain once the document tables replaced it. A day of deletion larger than the day of addition is the sign of a replacement landing, not of growth.


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_28/pr_cycle.png)

The chart shows the 20 slowest pull requests, from 13 to 48 hours. Most are large changes: the report list screen and endpoint (48.1 hours), the tenor basis swap atomisation (32.3) and the regulatory book scope (25.0). The census of all pull requests is far faster: the median is 0.1 hours and the 90th percentile is 5.3 hours, so the chart is the tail and not the typical case.


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_28/stories_done.png)

The line climbs from 2 to 60 stories between 2026-10-13 and 2026-10-21, a steady slope of about seven stories a day. The step to 125 on 2026-10-24 is the sprint close, not a day of delivery. It counts the 84 stories closed DONE and the 41 closed ABANDONED, of which 37 moved to the product backlog. Read the slope up to 2026-10-21 as the delivery rate.


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 432 after the v0.0.27 tag, 487 counting the PRs merged on 2026-10-13 before the tag (2026-10-13 to 2026-10-24)
-   **Sprint duration**: 2026-10-13 → 2026-10-24 (twelve days, planned seven)

---

*Next sprint: triage the product backlog inbox, which now holds the remainder of 24 split stories and 37 stories that never started, and limit the stories in flight before it starts any.*
