Thanks — the first finding was a real regression and I would have merged it. Responses in order.

**1. Basket-parsing regression — confirmed, and it was worse than stated. Fixed.**

You were right, and the capability was not merely reduced: after the swap **no shell path could create or edit a basket constituent at all**.

The hand-written `commodity_instrument_commands.cpp` carried a `parse_basket` helper whose `add` took a `[basket]` positional of `CODE:weight,CODE:weight` tokens, built `commodity_basket_constituent` rows and published `put_commodity_basket_constituent_request`. The generated unit has no `basket` reference, because the constituent became a child row in its own model (`ores.trading.commodity_basket_constituent.org`) and is not a field of the instrument — and that model did not enable the shell facet either. So the client-side convenience *and* the menu were both gone.

The same applied to `equity_position_option_underlying`, which you flagged for checking — and there the position is different, so I want to be precise rather than claim a clean sweep. Its deleted unit also carried a bulk parser (`parse_entries`, taking `code:strike:...:weight` rows). That entity *was* already a shell opt-in, so its menu and its per-row `add`/`set` were never missing; what it lost is the comma-separated convenience positional. The basket case was worse: the entity had no shell facet at all, so both the convenience *and* the menu were gone.

So after this fix both entities can create and edit their rows through generated per-row commands, and **neither has the bulk positional back**. Restoring that means modelling the constituent list as a command argument rather than a child table — a design change, not a repair, and one I have recorded rather than taken here. If you want it in this PR, say so and I will take it as its own unit.

The fix is model-driven and one line rather than keeping a hand-written copy, which H02 forbids:

```org
:PROPERTIES:
:ID: 302465BC-152E-4334-907C-E0A83B05CA05
:ores.cpp.shell-command.enabled: true
:END:
```

`commodity_basket_constituent` now owns a menu of its own. Ten recipes and their tests are generated, the source lists and inventory are regenerated, and all ten replay `ANSWERED` against the live fleet — `add` and `set` included. No new permission code was needed: the constituent handler was already registered and its codes already seeded. Commit `8727749250`.

The generalisable lesson is now in the task record: **a hand-written unit can carry behaviour the model does not state, so deleting it needs that behaviour named and its replacement proved, not assumed.** That is the second finding on this branch that no gate could see, the twelve unwired registrars being the first.

**2. Global sentinel `1` for every required decimal — measured, and currently safe.**

You are right that the table is global and that a `< 1`, rate or percentage could reject it. I checked rather than assumed:

- 24 models across the tree both enable the shell facet and declare a required `ores::utility::decimal::decimal` field.
- The only upper bound at or below 1 anywhere in them is `credit_instrument`'s `recovery_rate <= 1` — and that field's `:cpp_type:` is `double`, not a decimal, and `1` satisfies `<= 1` anyway.
- There is **no** lower bound above 1 in any of the 24.

So no model breaks today. I agree the class recurs — it is the same class as capture `A06D2B03` — and the right guard is a test that renders every required decimal's recipe and asserts the token parses, or per-field sentinel overrides. I have not added it here; it is a codegen-wide lever rather than a trading one, and I would rather do it as its own unit than bolt it on. Noted in the reply trail.

**3. `shell_command_surface` reads from a different part / silent opt-out — agreed, not changed here.**

`Path(templates_dir).resolve().parents[2]` does encode the layout depth, and returning `(False, ())` when the overview is missing means a component with opted-in entities and no overview silently generates nothing. The `#+shell_command_aggregator:` value likewise opts out silently on anything but `true`/`t`/`yes`/`1`. Both are real robustness defects in the archetype I added. They are codegen-wide and I have left them for a follow-up rather than widen this PR further; the failure mode is loud in one respect already, in that the component-scoped aggregator simply does not appear and the shell then has no menu for that component.

**4. Lookup depth and `lru_cache` — agreed.**

Nothing enforces that `_shell_command_units` and `discover_models` agree on depth, and a test asserting they discover the same files would be cheap. The cache being process-lifetime is fine for the CLI (`compass codegen`) but can serve stale data to a long-lived pytest session that edits models. Also a follow-up.

**5. `_group_import` second-segment assumption — the cross-component case does work.**

There is a test for it: `ores.trading`'s audit group imports `ores.dq.audit_record`, which is the cross-component path, and the TypeScript typecheck (`ores.web.typecheck`) passes in CI — that is the check that would fail on a wrong relative path. The `::` branch is covered by that case rather than by a dedicated unit test, which I agree is thinner than it should be.

**6. Minor.**

- `trading_commands.cpp` in `application/src` is generated, and `check_component_drift.py --all` reports no drift with it, so the generator and `clang-format` are not fighting.
- `.gitignore` `*.cmapx`: intentional. `git ls-files '*.cmapx'` is exactly one file, the stale `ores_schema` ER diagram, which the repository-wide ER sweep owns; ignoring does not untrack it, and the comment says so.
- The 71-to-0 census: `survey_vacuous_tests.py` detects a weak assertion as a size/length comparison, an emptiness or `has_value` check, a nothrow, or a bare truthiness the test did not itself assign. A reviewer can re-run it as `python3 projects/ores.codegen/scripts/survey_vacuous_tests.py --component trading`.

**Process note on the enum sentinel.** You argue for fixing `trade_types` before merge rather than deferring. I did not, and I want to be straight about why: the fix needs the field's declared `:default_value:` plumbed to `_sentinel_field`, and I wrote that, measured that it changed no generated output because the metadata does not reach the shell projection, and reverted it rather than commit a lookup that quietly finds nothing. Where the derived CRUD messages take their field dicts from is the thing to settle first. It is capture `A06D2B03`, with that finding recorded in it. If you would rather it block the merge, say so and I will take it now.

Also corrected since your review: the H01 audit record contradicted itself on whether the rasters were clipped (they are whole; the *first* render was clipped and reading it is what found the bound), H03 had claimed to cover the whole diff while covering only the generated shell units, the story checklist still recorded most in-scope items as unmet, and two derivation tools the records told a reviewer to rerun were untracked and absent from the PR — both are now committed under `projects/ores.codegen/scripts/`. Commit `d88802a5de`.
