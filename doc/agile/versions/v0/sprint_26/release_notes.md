*October 2026*

Release notes for [Sprint 26](https://orestudio.github.io/OreStudio/doc/agile/versions/v0/sprint_26/sprint.html).

Sprint 26 ran the three-pass mission in order: sync codegen and clear the drift, analyse the user journeys, then build the UX. The sprint closed RED: it ran thirteen days against a seven-day plan, and it carried about four times the commit ceiling and five times the PR band. The work still shipped. Twenty-seven components came fully to the Component Clean Standard. The trade was redesigned on data-oriented principles, five journey groups run in ores.web on one shared journey runtime, and the HTTP surface is now generated end to end. Nineteen stories did not finish, and they moved to the product backlog inbox. Eleven stories closed at the scope they delivered, and each names a successor that carries the remainder. Sprint 27 opens from the product backlog.

---


# ✅ Highlights

-   The Component Clean Standard reached 27 components. Five carried to the backlog (ores.diff, ores.eventing, ores.history, ores.scheduler and ores.synthetic), ores.reporting and ores.dq closed at the work they delivered with the remainder split out, and ores.cli was decommissioned.
-   The trade is data-oriented: an immutable anchor with component tables keyed by the trade, generated composite keys, netting agreements and sets, sandboxes and virtual books.
-   Five journey groups — setup, entry, profile, credentials and access — run as ores.web screens on one shared journey runtime.
-   The HTTP surface is generated end to end, and fifteen components have a generated shell surface or a hand-crafted command signed off with a reason.
-   The marketdata grammar is a hand-written, typed codec, checked field by field against ORE's own parser.


# 🛠️ Key Improvements


## Build & Portability

-   **Hotfix: a Boost library landed without its vcpkg port**: Every continuous Linux job is red, in all four configurations, at one translation unit: #+begin\_src text projects/ores.utility/include/ores.utility/decimal/decimal.hpp:24:10: fatal error: 'boost/multiprecision/cpp\_dec\_float.hpp' file not found #+end\_src The decimal amount type, which Decide how an exact decimal is represented in C++ settled and commit `466ea3109f` then added, gives the estate exact base-ten arithmetic with `boost::multiprecision::cpp_dec_float_50`, and `vcpkg.json` was not told.
-   **Hotfix: a visibility macro on an enum breaks the Windows build**: Every Windows clang-cl job is red at one declaration: #+begin\_src text projects/ores.workflow/api/include/ores.workflow.api/service/ workflow\_definition.hpp:149:17: error: '<span class="underline"><span class="underline">dllimport</span></span>' attribute only applies to functions, variables, classes, and Objective-C interfaces [-Werror,-Wignored-attributes] #+end\_src The offending line is the enum, not a struct: #+begin\_src cpp enum class ORES\_WORKFLOW\_API\_EXPORT failure\_policy : std::uint8\_t { #+end\_src `ORES_WORKFLOW_API_EXPORT` expands to `BOOST_SYMBOL_IMPORT` or `BOOST_SYMBOL_EXPORT`, which is `__declspec(dllimport)` or `__declspec(dllexport)` on the Microsoft ABI, and neither attribute applies to an enum.
-   **Hotfix: a library exports a type none of its units compiles**: The Windows clang-cl build fails at the link step of the workflow core DLL: #+begin\_src text lld-link: error: undefined symbol: \_\_declspec(dllimport) public: \_\_cdecl ores::workflow::service::workflow\_registry::workflow\_registry(void) lld-link: error: undefined symbol: \_\_declspec(dllimport) public: \_\_cdecl ores::workflow::service::workflow\_definition::workflow\_definition(void) #+end\_src `ORES_WORKFLOW_API_EXPORT` is `BOOST_SYMBOL_IMPORT` for a consumer and `BOOST_SYMBOL_EXPORT` while the library itself is compiled.


## Financial Features

-   **Move reporting's write path onto the storage protocol**: The last consumer moves onto the bucket and key protocol, and the two consumers that already moved are verified again on the current tree.
-   **Hotfix: macOS cannot parse a floating point with std::from\_chars**: The macOS build stops on the credit simulation mapper: #+begin\_src text projects/ores.ore/core/src/domain/credit\_simulation\_mapper.cpp:63:9: error: call to deleted function 'from\_chars' 63 | std::from\_chars(text.data(), text.data() + text.size(), value); #+end\_src libc++ on Apple platforms does not implement the floating-point overload of `std::from_chars`.
-   **Hotfix: the nightly Valgrind run reports uninitialised members and library leaks**: The nightly memcheck run reports defects in four suites, and the outputs under `tmp/` are the CDash pages for them: #+begin\_src text 52/87 MemCheck: #55: ores.trading.api.tests &#x2026;&#x2026;&#x2026;..
-   **Model the ORE report configuration as entities**: A report definition's configuration is a set of strongly typed entities rather than a flat row of switches: every one of the seventeen ORE configuration document kinds is identified, mapped to an owning component, seeded as a configuration type, and round tripped, import and export, through the entities over the vendored corpus.
-   **Make the oresmd URI grammar explicit and typed**: Every oresmd URI states what it means without the reader knowing a default or a family's coordinate convention: each coordinate dimension is a named query key, every key the grammar defines is present, and every key is declared identity or coordinate so the series identity is a declaration rather than a special case.
-   **Rewrite the oresmd grammar as a hand-written codec checked against ORE**: An ORE market data key read into ORE Studio means what ORE means by it, and it comes back out as the same key.
-   **Hotfix: the marketdata core suite times out in CI**: `ores.marketdata.core.tests` passes inside the 600-second ctest limit on every CI platform again, with the whole-corpus import walk still in the default test set.
-   **Implement the refdata journeys in ores.web**: The written ores.refdata journeys run in ores.web against the real server: a reference data administrator can maintain the classification lists from the generic screen the journey documents describe, instead of the deleted per-entity screens.


## Service Architecture

-   **Make first-generation component scaffolding opt-in**: (Describe the user-visible outcome this story delivers.)
-   **Clean ores.platform to the component clean standard**: ores.platform meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.platform joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.logging to the component clean standard**: ores.logging meets the Component Clean Standard: the items marked All pass, and each item that does not apply is recorded with its reason.
-   **Clean ores.utility to the component clean standard**: ores.utility meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.utility joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.orgmode to the component clean standard**: ores.orgmode meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.orgmode joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.storage to the component clean standard**: ores.storage meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.storage joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.security to the component clean standard**: ores.security meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.security joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.database to the component clean standard**: ores.database meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.database joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.nats to the component clean standard**: ores.nats meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.nats joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.connections to the component clean standard**: ores.connections meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.connections joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.service to the component clean standard**: ores.service meets the Component Clean Standard: the items marked All and Protocol pass, each item that does not apply is recorded with its reason, and ores.service joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.telemetry to the component clean standard**: ores.telemetry meets the Component Clean Standard: the items marked All and Protocol pass, each item that does not apply is recorded with its reason, and ores.telemetry joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.geo to the component clean standard**: ores.geo meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.geo joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.variability to the component clean standard**: ores.variability meets the Component Clean Standard: the items marked All, Entity and Protocol pass, each item that does not apply is recorded with its reason, and ores.variability joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.fpml to the component clean standard**: ores.fpml meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.fpml joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.assets to the component clean standard**: ores.assets meets the Component Clean Standard: the items marked All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.assets joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.http to the component clean standard**: ores.http meets the Component Clean Standard: the items marked All and Protocol pass, each item that does not apply is recorded with its reason, and ores.http joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.workspace to the component clean standard**: ores.workspace meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.workspace joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.workflow to the component clean standard**: ores.workflow meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.workflow joins `COMPONENTS_UNDER_TEST`.
-   **Give the workflow component a fixture it can run against itself**: The workflow component is driven from ores.shell, against the live fleet and with no other component's data: the fixture runs the engine against itself, the engine acts across tenants, and its start, dispatch, success, warning, failure, compensation and recovery paths are each reachable and each asserted.
-   **Clean ores.iam to the component clean standard**: ores.iam meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.iam joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.refdata to the component clean standard**: ores.refdata meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.refdata joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.analytics to the component clean standard**: ores.analytics meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.analytics joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.compute to the component clean standard**: ores.compute meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.compute joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.reporting to the component clean standard**: ores.reporting meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.reporting joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.marketdata to the component clean standard**: ores.marketdata meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.marketdata joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.dq to the component clean standard**: ores.dq meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.dq joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.ore to the component clean standard**: ores.ore meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.ore joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.trading to the component clean standard**: ores.trading meets the Component Clean Standard: every item: All, Protocol and Entity pass, each item that does not apply is recorded with its reason, and ores.trading joins `COMPONENTS_UNDER_TEST`.
-   **Clean ores.testing to the component clean standard**: ores.testing meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and the one item that cannot pass is recorded as an exception with its proof.
-   **Clean ores.shell to the component clean standard**: ores.shell meets the Component Clean Standard: the items marked All pass, each item that does not apply is recorded with its reason, and ores.shell joins `COMPONENTS_UNDER_TEST`.
-   **Roll the generator fixes out to the components outside the test list**: Every component outside `COMPONENTS_UNDER_TEST` carries the four generator fixes the refdata clean-standard pass landed: a put response and a single-version response carry their row in a `std::optional`; a batch removal of an empty key list answers instead of rendering an empty `IN (`; the protocol-dependency gate keeps the `ores.doc.shell-recipe` family alive, so each component's recipes regenerate; and each response shape's TypeScript twin matches its header.
-   **Keep the ores.iam eventing tests idempotent**: `ores.iam.core.tests` passes twice in a row against the same database.
-   **Generate the shell surface of the clean components**: Each component the clean standard has cleared has a shell surface that generation produces, with every hand-crafted command recorded and justified.
-   **Generate the shell surface of the remaining components**: Each of diff, compute, telemetry, history, scheduler, ore, variability and workspace either renders the shell units generation produces or states, model by model, the reason it renders none, and every hand-written command that survives is recorded with its reason.
-   **Add a read-only entity profile**: A named variability profile produces a read-only entity: the generated protocol carries `list`, `get` and `get_many`, and no write verb and no version read.
-   **Generate HTTP routes from the models**: ores.http serves every component through routes generated from that component's models, with one uniform security model, and ores.assets is the first component landed through it.
-   **Generate literate HTTP recipes that tangle to runnable Hurl**: Every HTTP route the models declare has a generated literate recipe, each recipe tangles to a runnable Hurl file, the harness replays a recipe group against the live gateway and reports a verdict for each recipe, and the inventory is gated in CI.
-   **Close the object storage interface leftovers**: Two leftovers of the interface story are closed.
-   **Mop up the storage leftovers the goal surfaced**: Three leftovers the storage work surfaced are closed or accounted for.
-   **Hotfix: Restore every red build on main**: Every continuous build on main is red.
-   **Format the generated TypeScript with prettier**: The TypeScript render emits a tree that no formatter owns, so the format check named in package.json has never passed and never run: there is no prettier configuration in the repository at all.
-   **Hotfix: three reporting tables carry a tenant and no row-level security**: Every build that validates the SQL tree is red: #+begin\_src text WARNING [RLS\_001] create/reporting/reporting\_analytic\_types\_create.sql:0 Table 'ores\_reporting\_analytic\_types\_tbl' has tenant\_id but no ENABLE ROW LEVEL SECURITY found in any \*\_create.sql file WARNING [RLS\_001] create/reporting/reporting\_report\_analytics\_create.sql:0 WARNING [RLS\_001] create/reporting/reporting\_report\_run\_setups\_create.sql:0 Warnings: 3 #+end\_src The three tables arrived with the run document and analytics model, and their models declare a `tenant_id` without the row-level security facet every other tenant-scoped reporting table declares.
-   **Hotfix: the workflow deadline watch uses a thread Apple's libc++ lacks**: Both macOS legs are red at one declaration: #+begin\_src text projects/ores.workflow/core/include/ores.workflow.core/service/workflow\_engine.hpp:383:10: error: no type named 'jthread' in namespace 'std'; did you mean 'thread'?
-   **Hotfix: the workflow deadline pass reads an entry it has freed**: A step that outlives its deadline stops its run on every platform.
-   **Act inside a tenant**: A system administrator can enter one tenant on purpose, see on every screen that they are acting in it, read its data through the same reads its own members use, and leave.
-   **Implement order and filter in the generated list contract**: Finish the list contract the protocol specification already states, in the code generator, so every entity gets it from its model: 1.
-   **Give a person's token the permissions they hold**: Make a person's token carry the permissions they hold, and stop the token check from passing an empty list.
-   **See an account's sign-ins**: An administrator who opens an account reads what kind of account it is, whether it can sign in, and the sessions it has had, newest first.
-   **Build approval requests and notifications**: One component, ores.inbox, holds the approval requests and the notifications as designed: a relational schema generated from codegen models, with the rules the schema can carry — the one-approval-per-person index, the failed-delivery reason check and the four-eyes trigger — and the notification models generated with it.
-   **Fix the self write refusal on a tenant with accounts**: Every write lands on a tenant that holds accounts, including the write whose actor is the account written.


## Qt UI

-   **Make the shell speak to people plainly**: A person sees the work they can do, in their own words.


## Documentation & Tooling

-   **Show the current agile work item inside DSH**: A DSH plugin that shows the sprint as a kanban board inside the DSH web session.
-   **Hotfix: The site build aborts on a read-only HOME**: compass build &#x2013;direct site publishes again on a cold checkout whose HOME is read-only, and no batch Emacs script manages a cache that belongs to the user.
-   **Hotfix: the site build aborts on a POSIX class that reads as a link**: The site build and the CDash site job abort on main while publishing `doc/agile/versions/v0/sprint_26/clean-dq/task_clean_dq.org`: #+begin\_src text Build failed: Unable to resolve link: ":space:" #+end\_src The page records the command that counted the dq NATS subjects.


## Other

-   **Open sprint 26**: Sprint 26 exists as a proper agile artefact, with its mission set and wired into the version manifest and the agile index, and sprint 25 is closed.
-   **Make compass site page the site check**: An agent that checks documentation changes waits seconds, not minutes: the skills and recipes run `compass site page`, and the full site build is only for the first build in a checkout.
-   **Bring every component to the component clean standard**: Every C++ component meets the Component Clean Standard, the checklist written from what `ores.iam` and `ores.refdata` did in sprint 25: every wire type modelled and generated, every model on the current format and bound to a profile, no legacy or hand-written duplicate left, and every codegen gate green with the component under test.
-   **Harden the gates the component clean standard rests on**: A component clean-up passes or fails on what a gate can see.
-   **Design the trade shape on data-oriented principles**: Settle the target shape of the trade and everything joined to it — anchor, components, instruments, links, groups, activities, authorisation, agreement, structures, netting sets — before the instrument families are reworked, and record every decision with its reason.
-   **Teach the code generator the trade shape**: Give the code generator the capabilities the netting-set and trade-split stories need first, so they generate their entities rather than hand-write them.
-   **Model netting agreements, netting sets and CSAs**: Model the legal and credit grouping trades net under, as the Netting sets note analyses it: agreement, netting set and CSA as separate entities in ores.refdata.
-   **Add sandboxes and virtual books**: Let users import and experiment without disturbing official data, as the Sandbox note describes, and within the regulatory boundary it checks.
-   **Split the trade into an anchor and its components**: Implement sections 1 to 3, 6, 9, 10 and 13 of the trade shape investigation: the anchor, the booking, the state, identifiers, party roles and additional fields, with ORE import resolving to entities and export regenerating them.
-   **Decommission ores.cli**: ores.cli and its round-trip dataset are gone.
-   **Write the journey extraction standard and the ores.refdata journeys**: The User Journey Extraction Standard is written, and its first run documents the ores.refdata journeys, with every action its Qt screens offered covered by a journey or recorded as dropped.
-   **Reach systemd from a sandboxed compass session**: Four defects compound so that a compass session inside the sandbox reports a fleet state it cannot actually see or change.
-   **Give object storage a NATS and HTTP interface with authentication**: Object storage is reachable, and authenticated, on both interfaces the Object Storage target state names.
-   **Implement the setup journeys in ores.web**: A browser pointed at an empty installation takes a person from nothing to a working installation, and then to as many tenants and parties as they need.
-   **Implement the entry journeys in ores.web**: The two Entry journeys run in ores.web against the real server.
-   **Implement the profile journeys in ores.web**: The three Profile journeys run in `ores.web` against the real server.
-   **Implement the credentials journeys in ores.web**: The three Credentials journeys run in `ores.web` against the real server: - Protect my account — the member changes their own password and reads where the account is signed in.
-   **Implement the access journeys in ores.web**: The first three Access journeys run in `ores.web` against the real server: - Know what I may do — the member reads the roles they hold and the permissions those roles carry.
-   **Sprint 26 closure: story cleanup and reset for sprint 27**: Own the final steps of sprint 26: the close health review, a cleanup that leaves no half-finished story for sprint 27, the release notes and the retrospective.
-   **Remove the unused packages, extensions and Qt-named keys**: The first passes took the weight the Qt retirement left behind: the setup no longer installs the X11 and GL headers the Qt client needed nor the `pg_cron` and `pgmq` extensions, the Qt-named presentation keys and their helpers are gone, the removal is proved output-neutral — `iam` regenerates byte-identically — and the README, the icon inventory and the manual carry their corrections.
-   **Stop ores.shell menus from silently shadowing each other**: The shell has one owner per menu and per verb.
-   **Design the report pipeline and the run configuration**: One report definition is designed to run end to end from the scheduler through the workflow to the compute grid and back.
-   **Clean up the ores.shell remnants**: The shell holds no hand-written unit that generation supersedes, the three defects the clean-standard pass found are fixed rather than remembered, and every accepted exception that the menu-ownership change invalidated reads the way the code now behaves.
-   **Remove the ores.web fleet workaround**: `ores.web` starts the way every other component starts: the generated fleet, brought up by `compass services`.
-   **Hotfix: two macOS test failures that assert Linux-only facts**: With the macOS build compiling again, its test phase runs and fails in two places, both of them facts about Linux that the tests assert rather than defects in the product: - `filesystem_scoped_temp_file_tests.cpp` compares `sut.path().parent_path()` with `std::filesystem::temp_directory_path()` as strings.
-   **Hotfix: Apple's libc++ has no C++20 stop machinery at all**: The macOS legs are red again, one hour after the portable thread was added: #+begin\_src text stoppable\_thread.hpp:99:24: error: no type named 'stop\_token' in namespace 'std' stoppable\_thread.hpp:113:10: error: no type named 'stop\_source' in namespace 'std' #+end\_src The earlier story assumed the gap was `std::jthread` alone, because the error named only that type and the engine's lambda took a `std::stop_token` without the compiler complaining.
-   **Hotfix: the recovery test fails when the database is busy**: The recovery case fails on CI and passes on a quiet machine: #+begin\_src text workflow\_engine\_tests.cpp:404: FAILED: after.size() `= 2 for: 1 =` 2 #+end\_src `recover_in_progress()` re-dispatches every in-progress instance the database holds.
-   **Hotfix: the ER diagram no longer matches the SQL tree**: The `drift` job passes its first step and fails its second: #+begin\_src text Check generated output matches the models: No drift: regenerated output matches the checked-in tree.
-   **Hotfix: the macOS parser refuses the smallest subnormal**: The macOS test phase fails one assertion: #+begin\_src text projects/ores.platform/tests/numeric\_floating\_point\_tests.cpp:93: FAILED: parsed.has\_value() for: false #+end\_src The case formats each of a set of doubles with @c std::to\_chars and reads each back with `ores::platform::numeric::parse_double`.
-   **Show the environment inside DSH**: A DSH plugin that shows the session's own checkout as a live environment: the environment name, when the database was last restored and how long ago, the schema drift against HEAD, and the state of every service unit.
-   **Log the fleet to the journal**: The fleet logs to the console and systemd captures it into the user journal, so one interface reaches every service and no file is written.
-   **Make the tenant roster a working list**: A system administrator finds a tenant among many and acts on it from the roster: they search by code, name or hostname, filter by type and status, page through the list with its total, open a tenant's detail, and reach the operations a tenant offers from its row.
-   **Hotfix: a service listener falls behind a burst of notifications**: A service's PostgreSQL listener keeps up with a burst of notifications.
-   **Stop the web typecheck racing the web tests**: The two ores.web ctest entries never run at the same time, so the BFF tests never load the wire package's dist while tsc rewrites it.
-   **Refuse to start against a database built from other SQL**: The code knows the schema it was built for and the database records the schema it was built from, as a fingerprint of the SQL scripts.
-   **Send the filter with every web list request**: Every list request the web client builds by hand states the filter, and its schema is held to the generated request type so a member the protocol adds fails the typecheck instead of the live read.
-   **Document the operations journeys**: Five journeys document the operator's view of a running installation: See the running services, Watch the compute grid, Watch the message bus, Read the telemetry logs, and Check the versions and the database.
-   **Hotfix: Linux and Windows CI builds on main**: Bring the Linux and Windows continuous builds on main back to green.
-   **Standardise the record screens**: One standard defines the shape and behaviour of every screen in ores.web that lists and maintains records — lists and paging, the detail page, create, amend and delete, change reasons, history and revert, related records, icons, states, feedback, access and safety, naming, and exceptions — with the same scope the retired Qt entity standard had.
-   **A pattern language for service interaction**: A person who designs a service interaction can say "use pattern X" and point at one page.


# ⚠️ Known Issues & Postponed

-   **Complete the LLM skills catalogue** (ABANDONED): deferred.
-   **Clean ores.diff to the component clean standard** (ABANDONED): deferred.
-   **Clean ores.eventing to the component clean standard** (ABANDONED): deferred.
-   **Clean ores.history to the component clean standard** (ABANDONED): deferred.
-   **Clean ores.scheduler to the component clean standard** (ABANDONED): deferred.
-   **Clean ores.synthetic to the component clean standard** (ABANDONED): deferred.
-   **Add proper curve support to ores.marketdata** (ABANDONED): deferred.
-   **Record trade activities, their participants and trade links** (ABANDONED): deferred.
-   **Model operational authorisation and the client-facing agreement** (ABANDONED): deferred.
-   **Model trade structures** (ABANDONED): deferred.
-   **Clean ores.cli to the component clean standard** (ABANDONED): the component was decommissioned instead.
-   **Implement the user journeys in ores.web** (ABANDONED): deferred.
-   **Observe a run while it works** (ABANDONED): deferred.
-   **Complete the ORE report configuration** (ABANDONED): deferred.
-   **Return marketdata to the clean-standard registry** (ABANDONED): deferred.
-   **Land the role request journey** (ABANDONED): deferred.
-   **Default the shell facet on at the profile level** (ABANDONED): deferred.
-   **Run an example bundle through the compute grid from a report definition** (ABANDONED): deferred.
-   **Remove workspaces from every component** (ABANDONED): deferred.
-   **Implement the operations journeys in ores.web** (ABANDONED): deferred.


# 📈 Sprint Charts


## PRs and Commits per Day

Dual-axis bar chart. PRs (left axis) and commits (right axis) per day. A high commits-to-PR ratio may indicate scope creep.

![prs_commits.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_26/prs_commits.png)


## Daily Line Churn

Lines added (green) and deleted (red) per day. Building work produces mostly additions; refactoring produces a mix.

![line_churn.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_26/line_churn.png)


## PR Cycle Time

Hours from PR open to merge, one bar per PR. Long bars indicate review bottlenecks.

![pr_cycle.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_26/pr_cycle.png)


## Cumulative Stories Done

Line chart tracking stories marked DONE during the sprint. Steady upward slope is healthy; plateauing signals a stall.

![stories_done.png](https://raw.githubusercontent.com/OreStudio/OreStudio/main/doc/agile/versions/v0/sprint_26/stories_done.png)


# 📊 Time Summary

-   **Total effort**: not tracked
-   **PRs merged**: 516 (since v0.0.25, 2026-09-23 to 2026-10-05)
-   **Sprint duration**: 2026-09-23 → 2026-10-05

---

*Next sprint: carry the Component Clean Standard through the five components still in the backlog (ores.diff, ores.eventing, ores.history, ores.scheduler and ores.synthetic), then work the successors the close split out: the ORE report configuration, the report pipeline, the pattern language, the identity workflow, the ores.inbox approvals, the remaining record screens and the remaining user journeys.*
