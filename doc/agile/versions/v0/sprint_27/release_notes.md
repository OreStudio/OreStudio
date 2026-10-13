*October 2026*

Sprint 27 ran the UX pass: land the remaining journey groups in ores.web and bring the record screens to the standard. It closed RED. The sprint ran nine days against a seven-day window, carrying 3487 commits against a 1000 ceiling and 569 merged PRs against a 100-band, with 31 stories still in flight at the review. The work still shipped: the operations, refdata, synthetic-marketdata and super-administrator journeys run in ores.web, curves and a joinable series identity landed in ores.marketdata, the rates and credit families moved onto the instrument table rule, and the drift gate and the canonical protocol closed their remaining gaps. At the close every one of the 42 open stories was split: 21 closed at the scope they delivered with a named successor, and 21 moved whole to the product backlog inbox.

---


# ✅ Highlights

-   The UX pass landed its largest journey groups: the operations, refdata, synthetic-marketdata and super-administrator journeys run in ores.web, and the record screens gained their party timeline, audit polish and account split.
-   The market data model matured end to end: curves as derived series with provenance and manual keys, a joinable series identity, and an oresmd URL editor that composes, parses and validates an address in the browser.
-   The instrument reworks held their line: the rates family shipped, and the credit family followed the same bond pattern, so the trading model carries typed families instead of polymorphic payloads.
-   The codegen programme kept paying down structural debt: the drift gate now covers the last components, hand-written IAM and DQ tables gained models, and every subject sits on the canonical entity protocol.
-   The close discipline worked: all 42 open stories were split at the close, 21 closed at the scope they delivered with a named successor, and no open story or task was left behind.
-   A hotfix line answered every red build, clearing the local-check failures on main and keeping the continuous builds green.


# 🛠️ Key Improvements


## Financial Features

-   **Make a series identity joinable in the database**: Make the identity of a market series queryable in the database.
-   **Return marketdata to the clean-standard registry**: marketdata passes every item of the component clean standard, or holds an exception the owner accepted, and is back in `COMPONENTS_UNDER_TEST` so the drift and protocol twin gates cover it again: P01 and H01 close, H03's namespace closer comments leave the templates and the files they generate, and the record is re-measured at one commit.
-   **Rework the rates instruments to the instrument table rule**: Rework swaps, FRAs, caps and floors, swaptions and the other rates trade types to the instrument table rule of the trade shape investigation (section 14), as the bonds were reworked, with the data-oriented review deciding the tables.
-   **Rework the credit instruments to the instrument table rule**: Rework the credit trade types and the risk participation agreement to the instrument table rule: a family header holding exactly its codes, product fact tables keyed by the trade, shared children with role codes, and securities keyed by their reference datum.
-   **Read each tenant's cache partition inside its tenant**: Every tenant's partition of a generated event cache loads inside that tenant with a token for that tenant obtained by Token Exchange, and the parties policy and read are strict again.
-   **Migrate the remaining refdata entities to the generated event registrar**: A change to any refdata entity publishes its event on that entity's own subjects, so the caches, the audit trail and any other consumer learn of it.
-   **Edit an oresmd URL in a dialog**: A person never writes an oresmd URL by hand.
-   **Link an oresmd URL to what it names**: An oresmd URL behaves like every other reference in the app.
-   **Finish the refdata read path**: Every refdata list the record-screen standard names searches and sorts on the server, a tenant has a history, a party event refreshes the cache that names parties, the party test inserts a category, and a parent cycle in the party hierarchy is reported rather than silently dropped.
-   **Implement synthetic data generation against the new market data model**: Make the synthetic feeds first-class producers in the new market data model.
-   **Write the refdata party, book, convention and tenor journeys**: The five refdata journeys of the agreed catalogue that no document covers have one, so that a tenant administrator onboarding a counterparty, keeping a party's details current, shaping the book structure, defining the conventions for an instrument or defining how tenors resolve has a screen design to build against.
-   **Implement the refdata party, book, convention and tenor journeys**: Four of the five refdata journeys &#x2013; counterparty onboarding, party details, the book structure and the instrument conventions &#x2013; run in ores.web on the server changes each one needs.
-   **Carry every trading instrument as a typed batch**: Every trading instrument crosses the export boundary as a typed array, not as an untyped payload.
-   **Hotfix: the oresmd codec does not compile**: Make ores.marketdata.core compile again on main.
-   **Hotfix: the party role names a dropped column**: Make ores.trading.api compile and let the party roles table be created.
-   **Hotfix: the consumers still set the dropped party columns**: Make the trade import and the trading service compile again.
-   **Hotfix: the counterparty fixtures write a dropped column**: Let ores.refdata.api compile and let the counterparty fixtures load.
-   **Hotfix: the series shape writer cannot name its context**: Let ores.marketdata.service compile again.
-   **Hotfix: the versioned catalogues cannot be seeded**: Let compass db recreate finish, so a database can be built from scratch on this branch.
-   **Hotfix: the chrono include never reached the trading instruments**: Bring trading-cpp back under the drift gate, so check\_component\_drift.py reports no drift for it.
-   **Hotfix: the seeded books have no purpose**: Give the book artefact's two new not-null columns, book\_purpose\_type and ledger\_feed\_type, the SQL defaults their own model prose already promises, so the five book seeds insert unchanged and compass db recreate completes.
-   **Publish the currency groups with Acme**: A new tenant, Acme included, holds the desk currency groups and their members after provisioning.
-   **Show the whole history of a party**: A party's history shows every change to the party and to everything attached to it: its identifiers, contacts, business units, countries, currencies and counterparties, each with who made it and why, and says nothing is missing because nothing is.
-   **Write the trade journeys**: The trade journeys exist as use cases under doc/knowledge/journeys/trades/.
-   **Prototype the trade journeys**: Four prototypes under doc/prototypes/trades/ serve all seven trade journeys as entity screens under the UX standards.
-   **Specify the trade screens in the UX standards**: The menu page and the entity screen specification state how the trade screens look and behave, including the standard entity protocol they read and write through.
-   **Filter and search trades through the model**: The trade model declares which columns a list can filter, search and sort by.
-   **Complete the trade list's filters**: A trade page reads each child table by trade id, and the entity protocol states the standard for finding entities with range filter members.
-   **Manage market data bindings**: A journey and a signed-off clickable prototype define how a person manages market data bindings, for supported synthetic data and unsupported real data, and the server reads and BFF record kind the screen needs exist.
-   **Organise feeds by provider and scope**: Make a provider a system-level record, give every feed a system or test scope, seed the provider catalogue by provider and kind, and browse feeds through one tree by scope, provider and feed kind.


## Service Architecture

-   **Complete the ores.inbox approvals and notifications**: A member asks for a role, an administrator answers from the queue, and both are told through in-application notifications: the role request is the first approval kind, and the request and notification screens render it.
-   **Codegen developer experience improvements**: Fix two of the codegen regenerate workflow's friction points: a component run silently skips models whose type the address does not support, and an entity filter restricts a run to the named entities.
-   **Codegen improvements from the clean-up passes**: The codegen layer has no defect left that the component clean-up passes found.
-   **Workspaces: finish the feature**: Finish the workspace feature.
-   **Clean ores.scheduler to the component clean standard**: ores.scheduler meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.scheduler joins `COMPONENTS_UNDER_TEST`.
-   **Add the identity workflow and the commands that drive it**: The identity workflow is a registered definition whose steps are declared by the request, so one fixture covers many workflow configurations that emulate real ones, and `ores.shell` drives it end to end.
-   **Model the handcrafted ores.dq code and generate the artefact tables**: Every handcrafted ores.dq table and function has a model, and the artefact tables generate from the same entities as their base tables, so the component keeps regenerating byte-identically.
-   **Remove the presentation keys no template reads**: No presentation key is written that no template, render path or validator reads; the helpers that existed only to compute them are gone; regenerating a component produces no diff; and every document that named the Qt layer as live is corrected or marked historical.
-   **Complete the super administrator UX journeys**: Signed in as the super administrator, the deployment's own world is real and named: the administrator is greeted by a human name and shown a picture, and the system administration area lists the accounts the installation runs on — the platform's service accounts among them.
-   **Remove workspaces from every component**: No component carries a workspace: no table has a workspace\_id, no request or session carries a workspace context, and ores.workspace is gone.
-   **Bootstrap the system at runtime**: A freshly created database holds the schema and the staged DQ artefacts and nothing else.
-   **Hotfix: Domain generator omits the Boost UUID I/O include**: The CI build fails in `projects/ores.iam/api/src/generators/run_grant_generator.cpp` with `error: 'to_string' is not a member of 'boost::uuids'`.
-   **Attach pictures at runtime through one seam**: Pictures are attached at runtime through one seam: an assets operation ensures a template image exists in a tenant, and an IAM operation reconciles accounts' pictures from a scope.
-   **Keep the nightly include-cleaner out of generated files**: Every include fix from the nightly run lands in the layer that owns the file: hand-crafted sources directly, generated sources through their templates.
-   **Clear the local-check failures on main**: The local check gate stops failing on drift that predates any branch.
-   **Hotfix: The credential model links a model that was withdrawn**: The credential model links nothing that does not exist.
-   **Bring the last components under the drift gate**: Put every component that generates C++ under the drift gate, one component per change, so a template change can no longer pass through unseen.
-   **Bring the generated column references under a gate**: Catch a generated reference to a column that does not exist before it merges.
-   **Hotfix: the generated headers miss the chrono include**: Let every generated domain header that names std::chrono compile on its own.
-   **Hotfix: the tenantless catalogues ask for tenant policies**: Let the database schema load.
-   **Hotfix: the settings trigger is refused by the function it calls**: Let a service write a tenant-wide setting, so bootstrap's last step completes.
-   **Clear the stale block on the literate template task**: Return the literate-template task to the backlog, so a workstream that can proceed is not held by a dependency that finished three months ago.
-   **Hotfix: the audit events miss the sign-ins**: The audit sign-ins screen shows almost nothing, on an installation whose authentication log is not empty.
-   **Hotfix: the er-diagram and pgtap checks fail on main**: The local check gate passes er-diagram and pgtap on main, so a clean run means the branch is clean.
-   **Hotfix: a codegen test remaps the trade target for every later test**: A full codegen test run passes on main, whatever order the tests run in.
-   **Polish the audit sign-ins screen**: The Sign-ins screen reads as the rest of the application does.
-   **Hide what system accounts did in a history**: A history shows a toggle that hides or shows the entries made by service accounts.
-   **Split accounts into open and confidential data**: Every tenant can read the open data of an account, such as the name, picture and type of a service account, through the same pattern that shares reference data.
-   **Model the hand-written IAM tables**: Every IAM table that holds data has a model, so the drift check covers it and no table is written by hand.
-   **Close the IAM isolation gaps and review account visibility**: No IAM table that carries a tenant is readable across the tenant boundary unless a model says why, and the account visibility model has been reviewed against a stated list of threats.
-   **The bootstrap runs as two workflows, one per administrator**: Let each administrator finish the half of a tenant's setup that is theirs.
-   **Bring every DQ artefact table onto the canonical model**: Move DQ artefact tables onto the canonical model: the duplicated ores.dq models and the hand-written artefact tables are generated from the entities they stage, and a guide states the one way to add a DQ artefact.


## New Features

-   **Hotfix: Windows MSVC build treats an unused catch parameter as an error**: Make the MSVC build compile publish\_service.cpp again.
-   **Run compute nodes on the local network hosts**: Three machines on the local network, goedel (192.168.1.199), newton (192.168.1.22) and 192.168.1.11, run compute nodes (ores.compute.wrapper) that join an existing environment's grid over NATS.
-   **Serve the grid's samples as time series**: Serve the grid's and the node samples as bucketed time series over a caller's range, page them through the BFF, and draw the load, fleet, throughput and outcomes panels on one shared time axis.
-   **Keep queued grid jobs runnable for as long as they wait**: A job that waits in the grid queue for any length of time still runs when a node takes it, and the queue is bounded and visible, so a burst of work slows the grid and never fails it.


## Qt UI

-   **Group the prototypes and add the Gemini reviews**: A person looking for a prototype finds it by the journey topic it answers to, not by scanning one flat list of 46 directories.


## Documentation & Tooling

-   **Keep the prototypes in git**: A prototype is the design record its journey cites, and this story keeps it readable after the sprint that built it closes.
-   **Add the writing discipline the Zettelkasten page omits**: Give the Zettelkasten page the half of the method it does not yet carry: the craft of composing and filing a note.
-   **Close the sprint's finished stories**: Let the sprint table tell the truth about what is in flight: no story says STARTED once every one of its tasks is DONE and every one of its PRs merged.
-   **Reconcile the sprint's bookkeeping**: Take the sprint audit from 41 findings to the two that are real open work, and lose no record doing it.


## Other

-   **Open sprint 27**: Sprint 27 exists as a proper agile artefact, with its mission set and wired into the version manifest and the agile index, and the project version carries 0.0.27, with the vcpkg submodule at latest.
-   **Hold money exactly and move the external version on economic change**: Hold every money quantity as an exact decimal, read the ORE boundary without loss, and derive the trade's external version from the economics actually held rather than from the reason a writer supplied.
-   **Add proper curve support to ores.marketdata**: Give ores.marketdata a first-class representation of a curve, so a bootstrapped curve is an entity with identity, provenance and a consumer rather than a derived series nothing reads back.
-   **Seed a relative-time trade population with the market data it needs**: A new ACME system has no time horizon and no deal population, so nothing says which fixings it needs.
-   **Make the ACME data realistic and isolate the ORE samples**: The ACME base bundle holds realistic agreements, netting sets, CSAs and portfolios for each party, and the ore\_samples bundle carries only the labelled reference data the sample import needs and lands its imports in a sandbox portfolio tree.
-   **Publish the ACME reports ready to run**: Publish the static inputs an ACME risk report needs through DQ datasets with declared dependencies, so a clean provision leaves every report definition bound to its shared configuration, with its analytics and parameters, and the export names every ORE file from the file types table.
-   **Implement the operations journeys in ores.web**: The five Operations journeys &#x2013; the running services, the compute grid, the message bus, the telemetry logs, and the versions and database &#x2013; run as screens in ores.web against the real server.
-   **Land the role request journey**: The fourth Access journey runs in `ores.web`: the member asks for a role beyond the one registration gave them, and the tenant administrator answers.
-   **Build the retire or reset tenant journey**: The Tenancy group's second journey: suspend, terminate, deprovision, purge and the party-level reset get their subjects, and the roster's Retire or reset item lights up.
-   **Bring the record screens to the standard**: The classification, tenant and party screens conform to the record screen standard, and the product owner's review of the people screens is closed.
-   **Complete the pattern language**: The pattern language is complete: the communication, coordination, resilience, load and data pattern groups are written, each pattern naming its established name and its sources, and a design names its patterns as a rule rather than by memory.
-   **Record trade activities and trade links**: Add trade\_activity with its type, and trade\_link with its type, so every component version names its cause and every relation between trades is typed and directed.
-   **Model trade structures**: Implement section 11 of the trade shape investigation, so a structure is a group with an identity whose legs reference it, as Trade Structures and Deal Composition requires.
-   **Clean ores.synthetic to the component clean standard**: Bring ores.synthetic to the Component Clean Standard (Entity items): tier 3, domain component.
-   **Run an example bundle through the compute grid from a report definition**: A report definition can select a step chain shorter than the full report execution workflow, and the reporting to compute handoff drives a compute job from it.
-   **Run a report as its tenant and party**: A scheduled report run acts as the tenant and the party of its definition, with no caller present.
-   **Guard every read the security documentation does not allow**: A read checks its read permission, unless the security documentation allow-lists it by subject, with the reason it is open.
-   **Name the installation's own data**: The installation owns a class of data the schema does not name: the facts about the deployment itself.
-   **Move the installation's remaining reads onto its own class**: The roster, the logs, the bus and the versions carry the installation's policy shape and are read whole by a session the permission admits, on the same codegen feature story 58F3FC7C added.
-   **Account for the grid's usage**: The grid's global view: which tenant is using what.
-   **Gather a run's inputs by declared dependency**: A step declares the artifacts it consumes and produces, and a run's chain is held as a persisted dependency graph that the engine dispatches from.
-   **Split the ER diagram by group**: The ER diagram is split into one file per package plus an index, drawn as SVG, and the relationships inside each group come from the SQL rather than a stale map.
-   **Bring the QA validation runner to ores.web**: A tester works through a test scenario in ores.web: open a scenario, read its steps beside its story and task, mark each step PASS or FAIL with a note, and save the result.
-   **Browse the agile tree in ores.web**: Story 2 of the QA runner arc.
-   **Raise a notification for a scenario waiting for a tester**: Story 3 of the QA runner arc.
-   **Hotfix: The user systemd probe ignores the busctl transport**: A sandboxed checkout builds, and starts a Claude session, without a `systemd-run` scope the sandbox refuses.
-   **Hotfix: Site build needs live GNU ELPA**: Site CI turns red whenever `elpa.gnu.org` does not answer.
-   **Hotfix: scheduler accepts six-field cron expressions**: A job or report schedule written with six fields is refused with a message that names the field count, instead of being stored and run on a shifted schedule.
-   **One way to keep a prototype**: Two prototype mechanisms exist.
-   **Hotfix: Continuous Linux red and the site build cancelled on every push**: Restore Continuous Linux to green and stop the site build from firing on every push.
-   **Tidy up the system core and the leftovers**: An installation brings itself up from a bundle that is the whole set of datasets it is read from, and the leftovers the first pass of that fix exposed are closed.
-   **Run every check locally in one report**: One local command replaces the GitHub pull-request wait.
-   **Account avatars are too dark to see**: An account's avatar reads as an identity on the dark theme, whether the account is a person, a service, or an administrator.
-   **Separate the account's credentials from its profile**: A profile write can never change a credential, and the history says which of the two actually changed.
-   **Bring every subject onto the canonical entity protocol**: One gate refuses a subject that is not {component}.v1.{resource}.{verb} with a verb from the closed set of eight, so the rule is enforced rather than remembered.
-   **Report a refused request as a refusal**: Make every client report a refusal as a refusal, naming the reason the server gave, so a permission problem is never mistaken for a malformed payload.
-   **Hotfix: Drop the libstdc++ internal header the include run added**: Both test files include <chrono> and not the libstdc++ internal.
-   **Isolate tenant data in object storage and the compute grid**: Object ownership and the object capability are settled, and a worker node reaches its assignment's objects through a per-assignment capability with no standing storage credential.
-   **Define the trade set model**: Lay the domain knowledge of grouping trades on one foundation.
-   **Trace a process run and the trade groups it creates**: A process that acts on a set of trades must be traceable, so it can be undone or repeated.
-   **Catch the generated artefacts up, and fix a dangling id link**: Four local checks fail on main and none of them is a branch's fault, so every branch inherits a red gate.
-   **Repair the drifted artefacts on main**: The ores.web workspace is prettier-formatted, so the TypeScript workflow passes on main again.
-   **Record the env format change for the workspace removal**: The env format log records version 29 as the removal of ORES\_WORKSPACE\_SERVICE\_DB\_\*, so older checkouts can upgrade.
-   **Mop up after the workspace removal**: The database recreates from the schema and populate scripts, and no knowledge or manual page describes the deleted workspace component.
-   **Quote the offset column in the marketdata identity view**: The commodity identity view loads, so the database recreates from the schema.
-   **Order the structure members after the trades they reference**: The trading schema loads, so the database recreates from the schema.
-   **Hotfix: the sweep schedule cannot see its own name**: Let ores.inbox.service compile again.
-   **Hotfix: the operations screens cannot see the bus or the storage service**: Let the message bus screen show samples, by polling the NATS monitor port the environment actually uses.
-   **Hotfix: the marketdata offset field has no name**: Let ores.marketdata.core.tests compile, so a full build reaches 100% on main.
-   **Hotfix: the party role test asserts a dropped column**: Let ores.trading.api.tests pass, so the trade party role suite reports on the code rather than on a column the model no longer has.
-   **Hotfix: the export joins on a trade id nobody mints**: Let the XML roundtrip walk and the exporter tests join each trade to its own instrument, so the ORE corpus imports stop resolving a trade onto another trade's instrument.
-   **Hotfix: the trading children write outside the session party**: Let the trade booking and trade state eventing tests pass again, so the suite reports on the notification chain rather than on a row its own session cannot see.
-   **Hotfix: the denial test counts on a caller without a role**: Let the denial case of the account-access read exercise the refusal it describes, so the suite tests the authorization boundary rather than a state no account is in.
-   **Implement the audit sign-ins journey**: The administrator can account for every open session: who is signed in, what the session did, who is failing to get in, where the session came from, and they can end a session that cannot be explained.
-   **Guard the curve-snapshot reads**: The curve-snapshot reads answer only a caller that holds the read permission, and the permission is seeded, so a signed-in member cannot read every tenant's curve points.
-   **Validate a curve pillar's tenor codes**: ir\_curve\_bootstrap\_pillar.start\_tenor\_code and end\_tenor\_code are undeclared soft references to refdata\_tenors, and only curve\_role\_code is checked, so a typo saves and then throws at bootstrap time inside curve\_republish\_resolver.
-   **State the test run's prerequisites**: A test run that cannot pass says why before it starts, and the three prerequisites are recorded where the next person looks.
-   **Implement the membership journeys in ores.web**: The two Membership journeys run in ores.web against the real server.
-   **Show who did it in audit**: Every audit row names the account by its username, with the UUID beneath it, and a failed-attempts row means an attempt actually happened.
-   **Restore the trading guards the versioned model dropped**: Making every trading entity versioned removed the immutable profiles, and two guarantees went with them without a replacement: a trade's reported portfolio no longer has to belong to the trade's party, and a raw update rewrites every version of a row instead of opening a new one.
-   **Export the reporting line write from the browser entry**: The web bundle builds from main.
-   **Align the web palette with the reviewed scheme**: The application's dark theme is the GitHub-dark palette the project site uses, and the landing-page review argued a different one: a blue-black surface ramp on slate neutrals, an indigo accent reserved for interaction, and a sky hue for content that is not built yet.
-   **Give the super administrator's landing page its cards**: A system administrator lands on cards for the screens they use, and sees which journeys are still to come.
-   **Open the web as an Acme persona**: Test the web as different Acme users without adding profiles to the developer's own Chrome.
-   **Keep the record screens up to date**: Live updates from the change events, a Refresh button on lists and record pages, and a stated behaviour when the record under edit changed.
-   **Name the record in a change event**: A change event names no record, so a record screen cannot tell whether a change is its own.
-   **Approve a reporting line change by the proposed manager**: A change to a reporting line becomes a four-eyes decision: it is proposed, and the person it would make their manager approves or declines it, like a role request.
-   **End a session when its window closes, not when it is unattended**: A session ends when the browser window that held it is gone, and never because a screen sat unattended, so traders can leave their screens open.
-   **Group the super administrator's home into cards and subcards**: A system administrator lands on Tenants, Accounts, Operations and Security cards, and reaches the operations screens through the Operations card.
-   **Make the services screen tell the truth about the installation**: The Operations services screen shows the web service running, and an overview states how healthy the installation is at a glance.
-   **Show the release with its v**: The newest release figure reads v0.0.27, whichever form the services report.
-   **Review the system administrator's home screen**: The system administrator's home opens on a dashboard of four panels that all state their status the same way, with the active and upcoming modules on their own tabs.
-   **Build the four-eyes framework**: The four-eyes framework: a gated write raises an approval request that names several deciding parts, is held until every part approves, is applied as one transaction, and every kind of request shares one queue, one request page and one review panel.
-   **Review the tenant administrator's home screen**: A tenant administrator lands on a dashboard of panels that say what needs them, with the active and upcoming modules on their own tabs.
-   **Bring the existing screens to the entity screen specification**: Check the currency screens and every other existing screen against the entity screen specification and the record screen standard, and fix each difference in one PR.
-   **Show a curve evolving in ores.web**: A person opens ores.web, names a curve or a surface, and watches it evolve as an aligned matrix of instants by nodes, with each node's change, staleness and provenance.
-   **Pick a curve from the series a party can see**: A person opens the curve evolution screen and chooses the curve or surface from a list of the series their party can see, instead of typing its asset class, scope, instrument type, quote type and fields.
-   **Project the identities of the series a publish seeds**: Every market series a publish creates can be found by the resolver and listed by the picker as soon as the publish completes, without anyone running a backfill by hand.
-   **Adjust a curve point by hand from the web**: An operator who holds the write permission can set a node of a curve or surface by hand from the evolution screen, with a reason, and clear it again.
-   **Order the term axes of a shape by duration**: A person who reads a surface or a curve sees each term axis in order of length: 1W, 1M, 2M, 3M, 6M, 9M, 1Y, 18M, 2Y.
-   **Plot a surface as a heatmap and slices**: A person who reads a surface on the evolution screen sees its shape at once: a heatmap of the first axis against the second, and one line for each value of the first axis across the second.
-   **Paint every web badge from the catalogue**: Replace the hand-picked Tag tones on state and type values in ores.web with one shared Badge component that paints from the badge catalogue.
-   **Session activity: record the size of every NATS message per server**: Count the bytes of every NATS message each server receives and sends, and write the totals to the database at a fixed interval, so the session activity has samples to draw.
-   **Keep counting failed attempts on a locked account**: A locked account still counts its failed sign-in attempts and records the address of each, so a run of attempts against a locked account shows as an attack.
-   **Link every reference to its record**: Replace the per-screen address functions with one system-wide mechanism, so every foreign key a web screen shows is a link to the page of the record it names.
-   **Edit a party on a standard entity page**: Replace the party wizards with a standard entity page for a party: a list, a page with cards and tabs, and dialogs, built from the shared record parts.
-   **Sort every list by when it was updated**: Every list can be ordered by the time of the latest save, so the newest changes come first.
-   **Draw the menu by reads alone**: Who sees an area or a card depends on the reads a person holds and on nothing else, so the menu reads the same way for every person and a new screen needs no change to it.
-   **Name every synthetic feed by its oresmd reference**: Every synthetic feed states the datum or the series it produces as an oresmd URI, the same reference the market data store, the ticks, the feed bindings and the screens use.
-   **Change party from the account menu**: A person changes the party they act for from the account menu, without signing out.
-   **Make the local checks faster**: The local gate stops spending most of its time on work the branch did not touch, so it is cheap enough to run per change.
-   **Run the compute grid on the shared hosts**: Every machine on the local network runs this environment's compute nodes beside the environments already there, and each environment's deploy, stop and logs touch only its own state.
-   **Give every prototype its design note**: Every market data prototype has a design note beside it, and the group page links each note instead of carrying the argument in a table cell.
-   **Write the synthetic market data journeys**: Every synthetic prototype answers to a journey document: the actor, the steps, the wireframe, the entities it composes, a coverage table checked against the synthetic subjects, and the gaps that must close before it is built.
-   **Implement the synthetic market data journeys in ores.web**: A person authors an FX price feed and an IR curve feed in ores.web, on the subjects the journeys name.
-   **Record system events in a central event log**: Telemetry logs bury the key system events among every log line, and entity change events say a record changed, not that something happened.
-   **Sprint health review — System 2 analysis**: A System 2 review of sprint 27 at its close.


# ⚠️ Known Issues & Postponed

-   **Seed a relative-time trade population with the market data it needs** (ABANDONED): deferred.
-   **Build the retire or reset tenant journey** (ABANDONED): deferred.
-   **Workspaces: finish the feature** (ABANDONED): deferred.
-   **Clean ores.synthetic to the component clean standard** (ABANDONED): deferred.
-   **Move the installation's remaining reads onto its own class** (ABANDONED): deferred.
-   **Account for the grid's usage** (ABANDONED): deferred.
-   **Raise a notification for a scenario waiting for a tester** (ABANDONED): deferred.
-   **Trace a process run and the trade groups it creates** (ABANDONED): deferred.
-   **Validate a curve pillar's tenor codes** (ABANDONED): deferred.
-   **Restore the trading guards the versioned model dropped** (ABANDONED): deferred.
-   **Keep the record screens up to date** (ABANDONED): deferred.
-   **Name the record in a change event** (ABANDONED): deferred.
-   **Approve a reporting line change by the proposed manager** (ABANDONED): deferred.
-   **End a session when its window closes, not when it is unattended** (ABANDONED): deferred.
-   **Bring the existing screens to the entity screen specification** (ABANDONED): deferred.
-   **Paint every web badge from the catalogue** (ABANDONED): deferred.
-   **Session activity: record the size of every NATS message per server** (ABANDONED): deferred.
-   **Keep counting failed attempts on a locked account** (ABANDONED): deferred.
-   **Link every reference to its record** (ABANDONED): deferred.
-   **Edit a party on a standard entity page** (ABANDONED): deferred.
-   **Sort every list by when it was updated** (ABANDONED): deferred.
-   **Record system events in a central event log** (ABANDONED): deferred.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_27/prs_commits.png)

The sprint peaked at 560 commits on 2026-10-08 and never fell below 277 on an active day. The PR rate rose with it: 122 merged on 10-08 and 119 on 10-09, the two heaviest days. There is no idle day and no single burst — a sustained overrun, not a spike.


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_27/line_churn.png)

Churn ran high and mixed across the window: the shape of codegen and model rework rather than pure addition, because the rates and credit instrument reworks delete the old layout as they add the new one.


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_27/pr_cycle.png)

The chart carries only the first twenty PRs the collector returned, all from 2026-10-05 to 10-12, so read it as a sample: those twenty merged in a median 11.9 hours and at most 25.8. The collector cap is a known defect, not a property of the sprint.


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_27/stories_done.png)

The curve climbs to 146 DONE stories with its steepest rise in the last three days, as the record-screen, instrument and IAM work closed together.


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 605 (since v0.0.26, 2026-10-05 to 2026-10-13)
-   **Sprint duration**: 2026-10-05 → 2026-10-13

---

*Next sprint: reduce the in-flight story count, finish the record screens and the carried journey remainders, and stop carrying the standing infrastructure programme in the same sprint as the UX pass.*
