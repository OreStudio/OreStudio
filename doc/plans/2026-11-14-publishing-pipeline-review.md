# Publishing pipeline — architecture review

## Status: OPEN — 2026-11-14

Analysis only. The fixes it recommends are not started.

**Related:** `2026-03-30-three-level-provisioning-and-workflow-service.md` settles
the three-level *account* model (system, tenant, party) and states that tenants
own the `ores_refdata_*` lookup tables. This review asks the adjacent question —
which reference data is published at which level, and by what. The two must agree
before either is implemented.

---

**Repository:** `/home/marco/Development/OreStudio/ores_dev_swift_curie`
**Scope:** the reference-data publishing pipeline (`ores.refdata`, `ores.dq`, tenant provisioning, populate tree, docs).
**Date:** review of the working tree at `a2c80d7a48`.
**Method:** reading the SQL, C++ and Org sources, plus read-only `grep`/parsing. No build, no database, no `compass` command was run. Every count below comes from a command recorded in §7.

---

## 1. The model as documented

Six documents describe the pipeline. They do not agree with each other, and two describe code that no longer exists.

| Claim | Source | Agrees with code? |
|---|---|---|
| Data divides into five tiers by substitutability. Tier 1 (platform vocabulary) arrives by a populate script written for the system tenant. Tier 2 (canonical reference data) arrives as "a DQ dataset, published into the owning service and installed as a bundle". | `doc/knowledge/architecture/data_tiers.org:38-103` | Partly. Tier 1 also arrives by a second mechanism the page does not name: the tenant provisioner copies ~36 tables into each new tenant (`projects/ores.sql/create/iam/iam_tenant_provisioner_create.sql:232-873`). |
| "`system_core` holds the installation's own data and is published into the system tenant once; every other bundle is published into each tenant a profile names." | `doc/knowledge/architecture/dq_artefacts.org:80-81` | Yes, for bundle membership. But `base` is *not* a system-level bundle: it is published per tenant (`doc/knowledge/journeys/conventions/journey_publish_conventions_from_data_quality.org:85`). The word "base" therefore names the opposite of "base system". |
| "The publication orders datasets by their dependencies and nothing else: the display order of a bundle member does not decide it." | `doc/knowledge/architecture/dq_artefacts.org:82` | Yes in the service (`publish_service::publication_order`). No in the workflow, whose own comment says "the order the request states them, which is their display order" (`projects/ores.dq/api/include/ores.dq.api/workflow/bundle_publish_workflow.hpp:137-139`). |
| A bundle publish calls one SQL orchestrator, `ores_dq_bundles_publish_fn`. | `doc/plans/2026-05-14-dq-publish-pattern.org:85-110` | **No.** The function exists only in the drop script (`projects/ores.sql/drop/dq/dq_bundle_publication_drop.sql:23`). The plan's "what publish does today" section is the superseded design it proposed to replace. |
| A bundle publish loads members in display order, starts `bundle_publish_workflow`, and "a final audit row is written to `ores_dq_bundle_publications_tbl`". | `projects/ores.dq/modeling/publication_pipeline.org:160-177` | **No** for the audit row. The table exists (`projects/ores.sql/create/dq/dq_bundle_publication_create.sql:41`) but nothing inserts into it anywhere in the tree. |
| "There are 57 functions across seven components … `refdata_publish_from_dq_create.sql` holds 47 of them." | `projects/ores.dq/modeling/publication_pipeline.org:185-187` | **No.** That one file holds 57 functions (`grep -c '^create or replace function ores_refdata_publish_'` → 57). |
| The pipeline's orchestration lives in `publication_handler.hpp`; the Librarian UI (`DataLibrarianWindow`) sends the request; "nothing sends the request today". | `projects/ores.dq/modeling/publication_pipeline.org:160,316` and Stage 5 | **No.** `publication_handler` is CRUD over publication rows. The orchestration is in `publish_handler.hpp`. The desktop client is gone, but two senders exist: the shell (`bundles publish`, `projects/ores.shell/application/src/app/commands/bundles_commands.cpp:52`) and tenant provisioning (`projects/ores.iam/core/include/ores.iam.core/messaging/tenant_provisioning_handler.hpp:976,2200`). |
| "Reference data is NOT published here. Publication is a runtime act … A freshly created database holds the schema and the staged DQ artefacts only, so the refdata tables are empty until that runs. A test that needs reference-data rows inserts them in its own setup." | `projects/ores.sql/populate/populate.sql:332-341` | Yes. This is the operative statement of the current model, and it is the direct cause of defect D1. |

**What the documentation says the model is, in one sentence:** populate writes tier-1 vocabularies into the system tenant; DQ datasets stage tier-2 reference data; a bundle published at runtime copies a dataset into the target tenant (or, for `system_core`, into the system tenant); dependencies order the copies.

---

## 2. The model as implemented

Five distinct write paths reach `ores_refdata_*` tables. Only two of them are in the documentation.

### Flow A — direct insert at populate time (system tenant)
`projects/ores.sql/populate/populate.sql:312` calls `refdata/refdata_populate.sql`, which seeds **DQ artefacts only**. The tier-1 vocabularies are loaded earlier, by `foundation/foundation_populate.sql:114-182`, which `\ir`s **57** scripts in `populate/refdata/` that `insert into ores_refdata_*_tbl` with `tenant_id = ores_utility_system_tenant_id_fn()`. These rows exist in a freshly created database.

### Flow B — DQ seeding (system tenant, artefacts)
`refdata_populate.sql` includes 17 scripts that (a) register a dataset with `ores_dq_datasets_upsert_fn`, (b) register dependency edges, and (c) insert rows into `ores_dq_*_artefact_tbl`. No refdata publish function is called here. A recreated database therefore holds the artefacts and an empty published catalogue.

### Flow C — runtime bundle publish (target tenant)
1. Caller sends `dq.v1.ops.publish_bundle` (shell `bundles publish`, or the provisioning workflow's `publish_bundle` step; the web app only has test doubles).
2. `publish_handler::publish_bundle` (`projects/ores.dq/core/include/ores.dq.core/messaging/publish_handler.hpp:147-232`) authorises `dq::datasets:write`, calls `publish_service::list_bundle_publishable_datasets`.
3. That reads `ores_dq_bundle_datasets_list_fn(bundle)` **ordered by `display_order`**, then calls `list_publishable_datasets(member_ids, resolve_dependencies = true)`. Dependency closure **adds datasets that are not bundle members** (`projects/ores.dq/core/src/service/publish_service.cpp:487`, `:403-450`). Datasets with no `target_subject` are silently dropped.
4. The handler filters `optional` members against `opted_in_datasets` (`publish_handler.hpp:180-200`), then starts one `bundle_publish_workflow` with one step per dataset.
5. `build_steps` chains each step on the previous one (`bundle_publish_workflow.hpp:141-142`) and dispatches `publish_from_dq_command` to the dataset's `target_subject`.
6. The target service's handler derives the SQL function from the subject (`<service>.v1.ops.<operation>` → `ores_<service>_<operation>_fn`, `projects/ores.refdata/core/src/messaging/publish_from_dq_handler.cpp:61-72`) and calls it with `(dataset_id, cmd.tenant_id, mode, params)` (`:127-135`).
7. **Tenant:** `cmd.tenant_id` is the caller's tenant (`publish_handler.hpp:255`, `start_publish_workflow`). For `system_core` the caller is the system tenant's super-admin (`tenant_provisioning_handler.hpp:1069-1096`); for `base` and `party_essentials` it is the tenant being provisioned. **Party:** `p_params->>'party_id'`, else the tenant's root party, else the system party (`refdata_publish_from_dq_create.sql:2223-2265`).

### Flow D — tenant provisioner copy (new tenant)
`ores_iam_provision_tenant_fn` copies ~36 system-tenant tables into the new tenant, row by row, with `change_commentary = 'Copied from system tenant during provisioning'` (`iam_tenant_provisioner_create.sql:232-873`). This is how a tenant gets `payment_frequencies`, `tenor_conventions`, `instrument_codes`, and the rest of the tier-1 vocabulary. It is invisible to the DQ model, to the bundle catalogue, and to `data_tiers.org`.

### Flow E — publish during populate (exceptions)
Three populate scripts do call a publish function as the DDL owner:
`populate/assets/system_avatars_populate.sql:66` (`ores_assets_publish_images_from_dq_fn`), `populate/iso/iso_populate.sql:39` and `populate/fpml/fpml_populate.sql:39` (`ores_dq_coding_schemes_publish_fn`). These are the only populate-time publishes; no refdata publish function is called there.

### Dead path — in-process publish
`publish_service::publish` (`publish_service.cpp:95-180`) has no caller. It also passes `artefact_type.target_subject` (a NATS subject) as a SQL function name (`:352-362`), so it could not work if called. Only `list_*` methods are live.

---

## 3. Inventory

### 3.1 The 17 datasets seeded in `populate/refdata/`

`published where` is bundle membership; **no** dataset is published by a populate script. Level is the reviewer's classification against requirements 3–4, using the bundle that carries it. `Direct-insert twin` names a script that writes the same live table without DQ.

| Dataset | Seed script | Artefact type | Publish function | Bundle(s) (display order, opt) | Depends on | Direct-insert twin |
|---|---|---|---|---|---|---|
| `refdata.calendar_events` | `refdata_calendar_events_seed_populate.sql` | `calendar_events` | `ores_refdata_publish_calendar_events_from_dq_fn` | `base` (20) | `refdata.calendars` | `refdata_calendar_events_populate.sql` |
| `refdata.calendar_exceptions` | `refdata_calendar_exceptions_seed_populate.sql` | `calendar_exceptions` | `ores_refdata_publish_calendar_exceptions_from_dq_fn` | `base` (19) | `refdata.calendars` | `refdata_calendar_exceptions_populate.sql` |
| `refdata.calendar_types` | `refdata_calendar_types_seed_populate.sql` | `calendar_types` | `ores_refdata_publish_calendar_types_from_dq_fn` | `base` (14) | — | `refdata_calendar_types_populate.sql` |
| `refdata.calendars` | `refdata_calendars_seed_populate.sql` | `calendars` | `ores_refdata_publish_calendars_from_dq_fn` | `base` (15) | `iso.coding_schemes`, `iso.countries`, `refdata.calendar_types` | `refdata_calendars_populate.sql` |
| `refdata.commodities` | `refdata_commodities_seed_populate.sql` | `commodities` | `ores_refdata_publish_commodities_from_dq_fn` | `base` (12) | — | — |
| `refdata.conventions` | `refdata_conventions_artefact_populate.sql` | `conventions` | `ores_refdata_publish_conventions_from_dq_fn` | `party_essentials` (5), `system_core` (21) | — | — |
| `refdata.crm_topology_bundles` | `refdata_crm_topology_bundles_populate.sql` | `crm_topology_bundles` | `ores_refdata_publish_crm_topology_bundles_from_dq_fn` | `crm_topology` (10), `party_essentials` (20) | `marketdata.fx_driver_rates` | — |
| `refdata.currency_calendars` | `refdata_currency_calendars_seed_populate.sql` | `currency_calendars` | `ores_refdata_publish_currency_calendars_from_dq_fn` | `base` (16) | `iso.currencies`, `refdata.calendars` | `refdata_currency_calendars_populate.sql` |
| `refdata.currency_countries` | `refdata_currency_countries_seed_populate.sql` | `currency_countries` | `ores_refdata_publish_currency_countries_from_dq_fn` | `base` (18) | `iso.countries`, `iso.currencies` | `refdata_currency_countries_populate.sql` |
| `refdata.currency_currency_groups` | `refdata_currency_currency_groups_seed_populate.sql` | `currency_currency_groups` | `ores_refdata_publish_currency_currency_groups_from_dq_fn` | `base` (22) | `iso.currencies`, `refdata.currency_groups` | — |
| `refdata.currency_groups` | `refdata_currency_groups_seed_populate.sql` | `currency_groups` | `ores_refdata_publish_currency_groups_from_dq_fn` | `base` (21) | — | — |
| `refdata.currency_pair_convention_calendars` | `refdata_currency_pair_convention_calendars_seed_populate.sql` | `currency_pair_convention_calendars` | `ores_refdata_publish_currency_pair_convention_calendars_from_dq_fn` | `base` (17) | `refdata.calendars`, `refdata.currency_calendars`, `refdata.currency_pair_conventions` | `refdata_currency_pair_convention_calendars_populate.sql` |
| `refdata.currency_pair_conventions` | `refdata_currency_pair_conventions_seed_populate.sql` | `currency_pair_conventions` | `ores_refdata_publish_currency_pair_conventions_from_dq_fn` | `base` (13) | `iso.currencies`, `refdata.currency_pairs` | — |
| `refdata.currency_pairs` | `refdata_currency_pairs_seed_populate.sql` | `currency_pairs` | `ores_refdata_publish_currency_pairs_from_dq_fn` | `base` (12) | `iso.currencies` | — |
| `refdata.equity_underlyings` | `refdata_equity_underlyings_seed_populate.sql` | `equity_underlyings` | `ores_refdata_publish_equity_underlyings_from_dq_fn` | `base` (12) | — | — |
| `refdata.fx_vol_surface_blueprints` | `refdata_fx_vol_surface_blueprints_artefact_populate.sql` | `fx_vol_surface_blueprints` | `ores_refdata_publish_fx_vol_surface_blueprints_from_dq_fn` | `system_core` (22) | `refdata.conventions` | — |
| `refdata.tenor_schedules` | `refdata_tenor_schedules_seed_populate.sql` | `tenor_schedules` | `ores_refdata_publish_tenor_schedules_from_dq_fn` | `base` (19) | `refdata.calendars` | `refdata_tenor_schedules_populate.sql` |


Notes:
- `refdata.conventions` and `refdata.crm_topology_bundles` are **party-scoped**: they take `party_id` from publish params and write party rows. They are members of `party_essentials`, so they are published once per party, not once per tenant.
- `refdata.calendars` depends on `iso.coding_schemes`, which is *not* a base member and which the bundle file says must not be published per tenant. The closure publishes it anyway (defect D4).
- `refdata.equity_underlyings` and `refdata.commodities` have **no dependencies** and are per-tenant copies of world data. Nothing declares that they must exist before a population runs; the bundle comment says they must (`dq_dataset_bundle_member_populate.sql:111-113`), but that is prose, not an edge.

### 3.2 The 57 functions in `refdata_publish_from_dq_create.sql`

**52 entity publishers** (one per registered subject) plus five helpers with no subject of their own: `ores_refdata_publish_target_party_fn` and the four convention sub-publishers (`tenor_basis_swap_conventions`, `xccy_fix_float_conventions`, `xccy_basis_conventions`, `commodity_future_conventions`) that `conventions` calls directly. The 52 subjects registered in `projects/ores.refdata/core/src/messaging/registrar.cpp:667-719` match the 52 publishers exactly.

| Function | Seeded dataset(s) using this artefact type | Writes to |
|---|---|---|
| `ores_refdata_publish_asset_classes_from_dq_fn` | `fpml.asset_class`, `ore.asset_class` | `ores_refdata_asset_classes_tbl` |
| `ores_refdata_publish_asset_measures_from_dq_fn` | `fpml.asset_measure` | `ores_refdata_asset_measures_tbl` |
| `ores_refdata_publish_benchmark_rates_from_dq_fn` | `fpml.benchmark_rate` | `ores_refdata_benchmark_rates_tbl` |
| `ores_refdata_publish_books_from_dq_fn` | `acme.acme_group.books`, `acme.acme_hk.books`, `acme.acme_uk.books`, `acme.acme_us.books`, `northwind.books`, `testdata.books` | `ores_refdata_book_product_types_tbl`, `ores_refdata_books_tbl` |
| `ores_refdata_publish_business_centres_from_dq_fn` | `fpml.business_center` | `ores_refdata_business_centres_tbl` |
| `ores_refdata_publish_business_processes_from_dq_fn` | `fpml.business_process` | `ores_refdata_business_processes_tbl` |
| `ores_refdata_publish_business_units_from_dq_fn` | `acme.acme_group.business_units`, `acme.acme_hk.business_units`, `acme.acme_uk.business_units`, `acme.acme_us.business_units`, `northwind.business_units`, `testdata.business_units` | `ores_refdata_business_unit_types_tbl`, `ores_refdata_business_units_tbl` |
| `ores_refdata_publish_calendar_events_from_dq_fn` | `refdata.calendar_events` | `ores_refdata_calendar_events_tbl` |
| `ores_refdata_publish_calendar_exceptions_from_dq_fn` | `refdata.calendar_exceptions` | `ores_refdata_calendar_exceptions_tbl` |
| `ores_refdata_publish_calendar_types_from_dq_fn` | `refdata.calendar_types` | `ores_refdata_calendar_types_tbl` |
| `ores_refdata_publish_calendars_from_dq_fn` | `refdata.calendars` | `ores_refdata_calendars_tbl` |
| `ores_refdata_publish_cashflow_types_from_dq_fn` | `fpml.cashflow_type` | `ores_refdata_cashflow_types_tbl` |
| `ores_refdata_publish_commodities_from_dq_fn` | `refdata.commodities` | `ores_refdata_commodities_tbl` |
| `ores_refdata_publish_commodity_future_conventions_from_dq_fn` | — (helper: called by `conventions`) | `ores_refdata_commodity_future_averaging_terms_tbl`, `ores_refdata_commodity_future_contract_terms_tbl`, `ores_refdata_commodity_future_expiry_anchors_tbl`, `ores_refdata_commodity_future_off_peak_indexes_tbl`, `ores_refdata_commodity_future_option_expiries_tbl`, `ores_refdata_conventions_tbl` |
| `ores_refdata_publish_conventions_from_dq_fn` | `refdata.conventions` | *dynamic SQL* |
| `ores_refdata_publish_counterparty_aliases_from_dq_fn` | `ore.counterparty_aliases` | `ores_refdata_counterparty_identifiers_tbl` |
| `ores_refdata_publish_countries_from_dq_fn` | `iso.countries` | `ores_refdata_countries_tbl` |
| `ores_refdata_publish_crm_topology_bundles_from_dq_fn` | `refdata.crm_topology_bundles` | `ores_refdata_crm_driver_pairs_tbl`, `ores_refdata_crm_enabled_derived_pairs_tbl`, `ores_refdata_crm_topology_configs_tbl` |
| `ores_refdata_publish_csas_from_dq_fn` | `acme.acme_group.csas`, `acme.acme_hk.csas`, `acme.acme_uk.csas`, `acme.acme_us.csas`, `ore.sample_csas` | `ores_refdata_csa_eligible_currencies_tbl`, `ores_refdata_csas_tbl` |
| `ores_refdata_publish_currencies_from_dq_fn` | `crypto.large`, `crypto.small`, `fpml.non_iso_currency`, `iso.currencies` | `ores_refdata_currencies_tbl` |
| `ores_refdata_publish_currency_calendars_from_dq_fn` | `refdata.currency_calendars` | `ores_refdata_currency_calendars_tbl` |
| `ores_refdata_publish_currency_countries_from_dq_fn` | `refdata.currency_countries` | `ores_refdata_currency_countries_tbl` |
| `ores_refdata_publish_currency_currency_groups_from_dq_fn` | `refdata.currency_currency_groups` | `ores_refdata_currency_currency_groups_tbl` |
| `ores_refdata_publish_currency_groups_from_dq_fn` | `refdata.currency_groups` | `ores_refdata_currency_groups_tbl` |
| `ores_refdata_publish_currency_pair_convention_calendars_from_dq_fn` | `refdata.currency_pair_convention_calendars` | `ores_refdata_currency_pair_convention_calendars_tbl` |
| `ores_refdata_publish_currency_pair_conventions_from_dq_fn` | `refdata.currency_pair_conventions` | `ores_refdata_currency_pair_convention_calendars_tbl`, `ores_refdata_currency_pair_conventions_tbl` |
| `ores_refdata_publish_currency_pairs_from_dq_fn` | `refdata.currency_pairs` | `ores_refdata_currency_pairs_tbl` |
| `ores_refdata_publish_curve_configuration_from_dq_fn` | `acme.acme_group.curve_configuration`, `acme.acme_hk.curve_configuration`, `acme.acme_uk.curve_configuration`, `acme.acme_us.curve_configuration`, `northwind.curve_configuration` | *dynamic SQL* |
| `ores_refdata_publish_entity_classifications_from_dq_fn` | `fpml.cftc_entity_classification`, `fpml.cftc_organization_type`, `fpml.entity_type` | `ores_refdata_entity_classifications_tbl` |
| `ores_refdata_publish_equity_underlyings_from_dq_fn` | `refdata.equity_underlyings` | `ores_refdata_equity_underlyings_tbl` |
| `ores_refdata_publish_fx_vol_surface_blueprints_from_dq_fn` | `refdata.fx_vol_surface_blueprints` | `ores_refdata_fx_vol_surface_blueprints_tbl`, `ores_refdata_fx_vol_surface_pillar_sources_tbl`, `ores_refdata_fx_vol_surface_pillars_tbl` |
| `ores_refdata_publish_legal_entities_from_dq_fn` | **none** | `ores_refdata_legal_entities_tbl` |
| `ores_refdata_publish_lei_counterparties_from_dq_fn` | `acme.lei_counterparties`, `gleif.lei_counterparties.large`, `gleif.lei_counterparties.small`, `gleif.lei_counterparties.tiny` | `ores_refdata_counterparties_tbl`, `ores_refdata_counterparty_business_centres_tbl`, `ores_refdata_counterparty_identifiers_tbl` |
| `ores_refdata_publish_lei_parties_from_dq_fn` | `acme.lei_parties`, `gleif.lei_parties.large`, `gleif.lei_parties.small`, `gleif.lei_parties.tiny`, `northwind.lei_parties` | `ores_refdata_parties_tbl`, `ores_refdata_party_identifiers_tbl` |
| `ores_refdata_publish_local_jurisdictions_from_dq_fn` | `fpml.local_jurisdiction` | `ores_refdata_local_jurisdictions_tbl` |
| `ores_refdata_publish_netting_agreements_from_dq_fn` | `acme.acme_group.netting_agreements`, `acme.acme_hk.netting_agreements`, `acme.acme_uk.netting_agreements`, `acme.acme_us.netting_agreements`, `ore.sample_netting_agreements` | `ores_refdata_netting_agreements_tbl` |
| `ores_refdata_publish_netting_set_aliases_from_dq_fn` | `ore.netting_set_aliases` | `ores_refdata_netting_set_identifiers_tbl` |
| `ores_refdata_publish_netting_sets_from_dq_fn` | `acme.acme_group.netting_sets`, `acme.acme_hk.netting_sets`, `acme.acme_uk.netting_sets`, `acme.acme_us.netting_sets`, `ore.sample_netting_sets` | `ores_refdata_netting_sets_tbl` |
| `ores_refdata_publish_party_contact_informations_from_dq_fn` | `acme.acme_group.party_contact_informations`, `acme.acme_hk.party_contact_informations`, `acme.acme_uk.party_contact_informations`, `acme.acme_us.party_contact_informations`, `northwind.party_contact_informations` | `ores_refdata_party_contact_informations_tbl` |
| `ores_refdata_publish_party_counterparties_from_dq_fn` | `acme.acme_group.party_counterparties`, `acme.acme_hk.party_counterparties`, `acme.acme_uk.party_counterparties`, `acme.acme_us.party_counterparties`, `northwind.party_counterparties` | `ores_refdata_party_counterparties_tbl` |
| `ores_refdata_publish_party_countries_from_dq_fn` | `acme.acme_group.party_countries`, `acme.acme_hk.party_countries`, `acme.acme_uk.party_countries`, `acme.acme_us.party_countries`, `northwind.party_countries` | `ores_refdata_party_countries_tbl` |
| `ores_refdata_publish_party_currencies_from_dq_fn` | `acme.acme_group.party_currencies`, `acme.acme_hk.party_currencies`, `acme.acme_uk.party_currencies`, `acme.acme_us.party_currencies`, `northwind.party_currencies` | `ores_refdata_party_currencies_tbl` |
| `ores_refdata_publish_party_relationships_from_dq_fn` | `fpml.hkma_rewrite_party_relationship_type`, `fpml.party_relationship_type` | `ores_refdata_party_relationships_tbl` |
| `ores_refdata_publish_party_roles_from_dq_fn` | `fpml.party_role`, `fpml.party_role_type` | `ores_refdata_party_roles_tbl` |
| `ores_refdata_publish_payment_frequencies_from_dq_fn` | **none** | `ores_refdata_payment_frequencies_tbl` |
| `ores_refdata_publish_person_roles_from_dq_fn` | `fpml.person_role` | `ores_refdata_person_roles_tbl` |
| `ores_refdata_publish_portfolios_from_dq_fn` | `acme.acme_group.portfolios`, `acme.acme_hk.portfolios`, `acme.acme_uk.portfolios`, `acme.acme_us.portfolios`, `northwind.portfolios`, `testdata.portfolios` | `ores_refdata_portfolios_tbl` |
| `ores_refdata_publish_regulatory_corporate_sectors_from_dq_fn` | `fpml.hkma_rewrite_regulatory_corporate_sector`, `fpml.regulatory_corporate_sector` | `ores_refdata_regulatory_corporate_sectors_tbl` |
| `ores_refdata_publish_reporting_regimes_from_dq_fn` | `fpml.reporting_regime` | `ores_refdata_reporting_regimes_tbl` |
| `ores_refdata_publish_sandbox_portfolios_from_dq_fn` | `ore.sample_portfolios` | `ores_refdata_portfolio_rights_tbl`, `ores_refdata_portfolios_tbl`, `ores_refdata_sandboxes_tbl` |
| `ores_refdata_publish_supervisory_bodies_from_dq_fn` | `fpml.supervisory_body` | `ores_refdata_supervisory_bodies_tbl` |
| `ores_refdata_publish_target_party_fn` | — (helper: resolves the party a party-scoped publish writes for) | *dynamic SQL* |
| `ores_refdata_publish_tenor_basis_swap_conventions_from_dq_fn` | — (helper: called by `conventions`) | `ores_refdata_conventions_tbl`, `ores_refdata_tenor_basis_swap_long_shorts_tbl`, `ores_refdata_tenor_basis_swap_pay_receives_tbl` |
| `ores_refdata_publish_tenor_schedules_from_dq_fn` | `refdata.tenor_schedules` | `ores_refdata_tenor_convention_resolutions_tbl`, `ores_refdata_tenor_schedules_tbl` |
| `ores_refdata_publish_trading_account_types_from_dq_fn` | `fpml.account_type` | `ores_refdata_trading_account_types_tbl` |
| `ores_refdata_publish_xccy_basis_conventions_from_dq_fn` | — (helper: called by `conventions`) | `ores_refdata_conventions_tbl`, `ores_refdata_cross_currency_basis_flat_legs_tbl`, `ores_refdata_cross_currency_basis_spread_legs_tbl`, `ores_refdata_cross_currency_basis_swap_terms_tbl` |
| `ores_refdata_publish_xccy_fix_float_conventions_from_dq_fn` | — (helper: called by `conventions`) | `ores_refdata_conventions_tbl`, `ores_refdata_cross_currency_fix_float_fixed_legs_tbl`, `ores_refdata_cross_currency_fix_float_float_legs_tbl`, `ores_refdata_cross_currency_fix_float_swap_terms_tbl` |


Findings:
- **Two artefact types are unreachable:** `payment_frequencies` and `legal_entities` have a registered type and a publish function, but no dataset in the tree uses either type. `ores_refdata_payment_frequencies_tbl` is therefore reachable only through Flow D, and `ores_refdata_legal_entities_tbl` has no writer at all.
- 33 of the 52 publishers have at least one dataset seeded outside `populate/refdata/` (iso, fpml, gleif, acme, northwind, testdata, ore, crypto). They are outside this review's dataset table but are listed above so the function inventory is complete.
- Every function is `security definer set search_path = public, pg_temp`, validates `mode`, and is tenant-scoped in its `exists` checks (checked in the bodies). The one exception is `publish_countries_from_dq_fn`, whose `v_exists` probe omits `tenant_id` (`refdata_publish_from_dq_create.sql:119-124`); it only affects `insert_only`.

### 3.3 Direct-insert inventory (57 files, 57 distinct tables)

`Has publish fn` = some `ores_refdata_publish_*_from_dq_fn` writes the table. `Copied` = `ores_iam_provision_tenant_fn` copies it into a new tenant. The table has 59 rows for 57 distinct tables because the two IR-curve files write the same two tables.

| Table seeded directly | Populate file | Has publish fn? | Copied by tenant provisioner? | Assessment |
|---|---|---|---|---|
| `ores_refdata_asset_class_codes_tbl` | `refdata_asset_class_codes_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_book_purpose_types_tbl` | `refdata_book_purpose_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_book_statuses_tbl` | `refdata_book_statuses_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_business_day_convention_types_tbl` | `refdata_business_day_convention_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_business_unit_types_tbl` | `refdata_business_unit_types_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_calendar_events_tbl` | `refdata_calendar_events_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_calendar_exceptions_tbl` | `refdata_calendar_exceptions_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_calendar_names_tbl` | `refdata_calendar_names_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_calendar_types_tbl` | `refdata_calendar_types_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_calendars_tbl` | `refdata_calendars_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_contact_types_tbl` | `refdata_contact_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_convention_kind_terms_tbl` | `refdata_convention_kind_terms_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_convention_kinds_tbl` | `refdata_convention_kinds_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_countries_tbl` | `refdata_countries_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_currency_calendars_tbl` | `refdata_currency_calendars_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_currency_countries_tbl` | `refdata_currency_countries_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_currency_market_tiers_tbl` | `refdata_currency_market_tiers_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_currency_pair_classifications_tbl` | `refdata_currency_pair_classifications_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_currency_pair_convention_calendars_tbl` | `refdata_currency_pair_convention_calendars_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_curve_configuration_columns_tbl` | `refdata_curve_configuration_columns_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_curve_roles_tbl` | `refdata_curve_roles_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_curve_sections_tbl` | `refdata_curve_sections_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_curve_segment_types_tbl` | `refdata_curve_segment_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_day_count_fraction_types_tbl` | `refdata_day_count_fraction_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_day_counters_tbl` | `refdata_day_counters_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_derivation_kinds_tbl` | `refdata_derivation_kinds_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_diary_entry_types_tbl` | `refdata_diary_entry_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_floating_index_types_tbl` | `refdata_floating_index_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_instrument_codes_tbl` | `refdata_instrument_codes_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_ir_curve_bootstrap_configs_tbl` | `refdata_ir_curve_bootstrap_configs_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_ir_curve_bootstrap_pillars_tbl` | `refdata_ir_curve_bootstrap_configs_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_ir_curve_bootstrap_configs_tbl` | `refdata_ir_curve_overnight_configs_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_ir_curve_bootstrap_pillars_tbl` | `refdata_ir_curve_overnight_configs_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_ledger_feed_types_tbl` | `refdata_ledger_feed_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_leg_types_tbl` | `refdata_leg_types_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_monetary_natures_tbl` | `refdata_monetary_natures_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_party_categories_tbl` | `refdata_party_categories_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_party_id_schemes_tbl` | `refdata_party_id_schemes_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_party_statuses_tbl` | `refdata_party_statuses_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_party_types_tbl` | `refdata_party_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_payment_frequencies_tbl` | `refdata_payment_frequencies_populate.sql` | yes | yes | three routes (direct + DQ + copy) |
| `ores_refdata_producer_kinds_tbl` | `refdata_producer_kinds_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_product_family_codes_tbl` | `refdata_product_family_codes_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_purpose_types_tbl` | `refdata_purpose_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_regulatory_book_types_tbl` | `refdata_regulatory_book_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_rounding_types_tbl` | `refdata_rounding_types_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_series_subclass_codes_tbl` | `refdata_series_subclass_codes_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_source_kinds_tbl` | `refdata_source_kinds_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_sub_periods_coupon_types_tbl` | `refdata_sub_periods_coupon_types_populate.sql` | no | no | **system tenant only** — no tenant route |
| `ores_refdata_business_centres_tbl` | `refdata_system_business_centre_populate.sql` | yes | yes | three routes (direct + DQ + copy) |
| `ores_refdata_parties_tbl` | `refdata_system_party_populate.sql` | yes | yes | three routes (direct + DQ + copy) |
| `ores_refdata_tenor_anchors_tbl` | `refdata_tenor_anchors_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_tenor_convention_resolutions_tbl` | `refdata_tenor_convention_resolutions_populate.sql` | yes | yes | three routes (direct + DQ + copy) |
| `ores_refdata_tenor_conventions_tbl` | `refdata_tenor_conventions_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_tenor_kinds_tbl` | `refdata_tenor_kinds_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_tenor_resolution_algorithms_tbl` | `refdata_tenor_resolution_algorithms_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_tenor_schedules_tbl` | `refdata_tenor_schedules_populate.sql` | yes | no | dual route: direct + DQ publish fn |
| `ores_refdata_tenor_units_tbl` | `refdata_tenor_units_populate.sql` | no | yes | dual route: direct + provisioner copy |
| `ores_refdata_tenors_tbl` | `refdata_tenors_populate.sql` | no | yes | dual route: direct + provisioner copy |


Groups:
- **Two writers (14 tables):** 10 tables are written by a populate script *and* by a DQ publish (`business_unit_types`, calendar events, calendar types, calendars, countries, currency calendars, currency countries, currency-pair-convention calendars, calendar exceptions, tenor schedules); 4 more also have a provisioner copy (`business_centres`, `parties`, `payment_frequencies`, `tenor_convention_resolutions`). For `countries` the direct script seeds a 42-row subset (ZZ plus calendar-referenced countries, `refdata_countries_populate.sql:20-40`) and the publish writes the full `iso.countries` set plus ZZ.
- **Copied to tenants (32 tables, no DQ route):** tier-1 vocabularies. Consistent with `data_tiers.org` tier 1, except that the delivery mechanism (`provisioner copy`) is undocumented.
- **System-tenant only (11 tables):** `convention_kind_terms`, `curve_configuration_columns`, `curve_roles`, `derivation_kinds`, `ir_curve_bootstrap_configs`/`_pillars`, `leg_types`, `producer_kinds`, `product_family_codes`, `source_kinds`, `sub_periods_coupon_types`. `derivation_kinds` is deliberately system-only (`projects/ores.sql/test/refdata_derived_source_system_tenant_test.sql:37-45`); the rest are unclassified. If any is read through a tenant-scoped repository, tenant screens will be empty.

### 3.4 Bundle levels

| Bundle | Members | Published into | Evidence |
|---|---|---|---|
| `system_core` | 21 | system tenant, once | `dq_dataset_bundle_member_populate.sql:44-93`, `data_tiers`/`dq_artefacts` |
| `base` | 57 | each tenant | `dq_dataset_bundle_member_populate.sql:103-190`, journey:85 |
| `party_essentials` | 2 (`refdata.conventions`, `refdata.crm_topology_bundles`) | each party | `:277-278` |
| `party_reporting` | 1 | each party | `:285` |
| `ore_samples` | 5 | each party | `:288-292` |
| `crm_topology`, `risk_management`, `lei_hierarchy`, `crypto` | 1–27 | per tenant, opt-in members possible | `:206-263` |

There is no bundle at "base system" level that carries the refdata catalogues. The only system-level bundle is `system_core`, and it carries conventions and FX vol blueprints, not calendars, currencies, commodities, or equity underlyings.

---

## 4. Where it hangs together, and where it does not

### Requirement 1 — tests must run without ACME provisioning

**Does not hold.** `populate.sql:332-341` states the choice explicitly: a fresh database has artefacts and empty refdata tables. `refdata.equity_underlyings_tbl` is populated only by the DQ publish, so `.SPX` exists in `ores_dq_equity_underlyings_artefact_tbl` and nowhere else. The check that refused the booking reads the live table (`trading_basket_constituent_functions_create.sql:49-56`). The 22 SQL tests that call publish functions run against a provisioned development database (e.g. `projects/ores.sql/test/refdata_currency_groups_publish_test.sql:30` says `pg_prove -d ores_dev_local1`), so they inherit whatever provisioning left behind.

### Requirement 2 — tests must still work after ACME provisioning

**Holds, by accident.** `ores_trading_basket_constituent_resolves_fn` is `security definer` and its query has no `tenant_id` predicate. The table enables RLS but does not force it (`refdata_equity_underlyings_create.sql:189-195`), and the owner bypasses RLS. So after ACME's `base` publish, *any* tenant's booking is accepted, because the resolver sees ACME's rows. The same mechanism lets a tenant book an underlying that it cannot itself see, and lets it fail when ACME has not been provisioned. The comment at `projects/ores.sql/create/trading/trading_basket_constituent_functions_create.sql:30-36` states the cross-tenant read as deliberate; the consequence is that the catalogue check is not a check on the caller's catalogue.

### Requirement 3 — datasets supply all static data needed to value every trade

**Partly holds.** The convention set is real: 24 convention kinds in one artefact type plus `currency_pair_conventions` gives 25 of 26, and the 26th (`tenor`) is copied from the system tenant (`journey_publish_conventions_from_data_quality.org:85-95`). What is missing is a statement of coverage. There is no dataset-level list of "the trade types this catalogue can value", and no test publishes `base` + `party_essentials` and then prices one trade of each type. Two concrete gaps:
- `refdata.equity_underlyings` and `refdata.commodities` are required before a population (`dq_dataset_bundle_member_populate.sql:111-113`) but are not dependencies of the populations that read them, and no test proves the order.
- `refdata.payment_frequencies` has no dataset, so it is not part of any bundle. A tenant gets it only through the provisioner copy.

### Requirement 4 — every dataset is published at the right level (base system or ACME)

**Cannot be answered as posed, because the tree uses three levels and the words collide.** The code has *installation* (`system_core`), *tenant* (`base`), and *party* (`party_essentials`). The owner's requirement names two. Under the code's naming, "base" is the per-tenant bundle; under the requirement's naming, "base system" reads as the system tenant. The concrete misclassifications are:
- `iso.countries`, `iso.currencies`, `fpml.asset_class`, `fpml.non_iso_currency` are in **both** `system_core` and `base`, on purpose (`dq_dataset_bundle_member_populate.sql:44-56`, `one_source_for_system_and_tenant_reference_data.org`). Two copies can drift; no reader is authoritative.
- `refdata.calendars` and `refdata.equity_underlyings` are **world data** (no `party_id`) but are published per tenant, so every tenant holds its own copy of a fact that should be identical. `world_data_versus_party_configuration.org` names this as an open question and still open.
- `refdata.conventions` is **both** the system party's copy (`system_core`) and each party's copy (`party_essentials`). It is the one dataset whose level is deliberately doubled.

### Requirement 5 — the publishing architecture hangs together conceptually

**Does not hold yet.** The model a reader can reconstruct from the code has five write paths (§2) and three levels, with two paths undocumented and two paths writing the same tables. The reader cannot answer "what turns a blank database into a usable one?" from any single document: `populate.sql` says "not here", `dq_artefacts` says "bundles", `data_tiers` says "populate for tier 1", and the provisioner says "copy". Each is true of a part.

### What does hang together

- The function contract is consistent: 57 functions, same signature, same modes, same return shape, `security definer`, tenant-scoped, idempotent-by-upsert.
- The subject naming is consistent and derived: 52 registered subjects ↔ 52 publishers; four helpers are internal.
- Dependency resolution is implemented and tested (`projects/ores.dq/core/tests/service_publication_order_tests.cpp`), and the handler wires it into the bundle path.
- The party resolution rule is explicit and documented in the function (`refdata_publish_from_dq_create.sql:2223-2245`).

---

## 5. Concrete defects

| # | Defect | Evidence | Consequence |
|---|---|---|---|
| **D1** | A recreated database has no published reference data. Populate seeds DQ artefacts only; no refdata publish runs. | `projects/ores.sql/populate/populate.sql:332-341`; `refdata/refdata_populate.sql` (no publish call); `grep -rn "publish_.*_from_dq_fn" populate/` finds only assets/iso/fpml. | Every live refdata catalogue is empty after `compass db recreate`. The `.SPX` booking refusal is one instance. Requirement 1 fails. |
| **D2** | The equity/FX/commodity catalogue checks read across tenants under `security definer`; RLS is enabled but not forced. | `projects/ores.sql/create/trading/trading_basket_constituent_functions_create.sql:49-56` (no `tenant_id`); `refdata_equity_underlyings_create.sql:189-195`. | A booking is accepted if *any* tenant published the code. Requirement 2 passes by accident; a tenant can book an instrument it cannot see. |
| **D3** | "Base" names two opposite levels. `base` is per tenant; `system_core` is the system tenant. No bundle is a system-level refdata catalogue. | `dq_artefacts.org:80-81`; `journey_publish_conventions_from_data_quality.org:85`; `dq_dataset_bundle_member_populate.sql:103`. | Requirement 4 cannot be stated unambiguously, so no dataset can be audited against it. |
| **D4** | A spurious dependency republishes `iso.coding_schemes` into every tenant, recreating the duplicates the bundle file forbids. | `refdata_dataset_dependency_populate.sql:54-58`; the calendars trigger reads `ores_dq_coding_schemes_tbl`, not the refdata table (`refdata_calendars_create.sql:97-102`); the exclusion comment (`dq_dataset_bundle_member_populate.sql:134-141`); closure at `publish_service.cpp:487`. | Every `base` publish writes coding schemes into the tenant, where no validator reads them and they appear twice in tenant-scoped lists. |
| **D5** | Fourteen tables have two independent writers (direct insert and DQ publish); four of those also have a provisioner copy. | `table_direct` rows for the 10 tables marked "direct + DQ publish fn" and the 4 marked "three routes". | The system tenant's rows and the tenants' rows come from different sources and can drift. No test compares them. |
| **D6** | Two publish functions are unreachable: no dataset uses their artefact type. | `dq_artefact_types_populate.sql:185-186` (`payment_frequencies`), `:249-251` (`legal_entities`); dataset parse finds no dataset with those types. | `payment_frequencies` reaches a tenant only via the provisioner copy; `ores_refdata_legal_entities_tbl` is never written. |
| **D7** | The publication audit table is never written, and `atomic` is never read. | Table at `dq_bundle_publication_create.sql:41`; `grep` finds no `insert into ores_dq_bundle_publications_tbl`; `publish_bundle_protocol.hpp:69` declares `atomic`, no reader; the shell sets it `true` (`bundles_commands.cpp:121`). | Publication history and the "preview/history" functions are always empty. A caller asking for all-or-nothing receives a partial publish reported as success. |
| **D8** | Documentation describes removed code as current. | `dq-publish-pattern.org:85-110`; `dq_bundle_publication_drop.sql:23`; `publication_pipeline.org:160,316`, Stage 5; `database_lifecycle.org:205-206` ("Data Librarian's Publish Datasets"). | A reader cannot learn the live model from the docs; the plan reads as if its own proposal is unimplemented. |
| **D9** | Documented function count is wrong (47 in refdata vs 57). | `publication_pipeline.org:185-187`; `grep -c` → 57. | Inventory understates the surface to be migrated by 10 functions. |
| **D10** | The bundle listing silently drops members with no dataset row and joins datasets by code without a tenant predicate. | `dq_bundle_publication_create.sql:130-141` (`left join … on d.code = m.dataset_code`); `publish_service.cpp:465-466` (`WHERE dataset_id IS NOT NULL`). | A typo in a member code produces a smaller, successful publish with no error. A dataset row in another tenant with the same code would be selected. |
| **D11** | The legacy in-process publish path is dead and passes a NATS subject where a SQL function name is required. | `publish_service.cpp:95-180` (no caller), `:352-362`. | A future caller gets a runtime SQL error; the code teaches the wrong contract. |
| **D12** | Tests assert against a provisioned database rather than creating the state they need. | 22 files call publish functions; e.g. `refdata_currency_groups_publish_test.sql:30` (`pg_prove -d ores_dev_local1`). | Test outcomes depend on whether ACME was provisioned; requirement 1 fails and requirement 2 is what makes the suite green. |
| **D13** | A fourth delivery mechanism (provisioner copy) is undocumented and overlaps the bundles. | `iam_tenant_provisioner_create.sql:232-873` (~36 copies); not in `data_tiers.org:99-103`; four tables are copied *and* published (`business_centres`, `parties`, `payment_frequencies`, `tenor_convention_resolutions`). | No single model explains how a tenant gets its vocabulary; a change to a lookup needs edits in two or three places. |
| **D14** | Eleven directly seeded tables have no tenant route at all. | `table_direct` rows marked "system tenant only"; `derivation_kinds` is deliberately so (`refdata_derived_source_system_tenant_test.sql:37-45`). | For the others (notably `curve_roles`, `convention_kind_terms`, `leg_types`), a tenant-scoped screen or soft FK will find nothing. Verify before relying on them. |
| **D15** | Documentation contradicts itself on ordering. | `dq_artefacts.org:82` (dependencies decide, display order does not) vs `bundle_publish_workflow.hpp:137-139` (display order). | The code follows dependencies; the workflow comment misleads the next editor into "fixing" the order by editing membership. |

---

## 6. Proposed end-to-end model

### 6.1 Levels (three, named so they cannot be confused)

| Level | Name | Who owns the rows | Written into | Examples |
|---|---|---|---|---|
| **L0** | installation | the platform | system tenant, once | coding schemes, template images, ORE parameters (if readers are changed) |
| **L1** | world | every tenant, identical | each tenant | ISO countries/currencies, FPML vocabularies, QuantLib calendars, currency pairs, commodities, equity underlyings, conventions-as-world-facts |
| **L2** | party | one party | the party | instrument conventions a party chooses, CRM topology, report definitions, portfolios/books |

Rule: **each table is written by exactly one mechanism, at exactly one level.** The mechanism follows the level: L0 and L1 canonical data through DQ datasets (because they are versioned, previewable and republishable); L1 *platform vocabulary* through the provisioner copy; L2 through party-scoped DQ datasets.

### 6.2 The boot sequence (blank database → usable)

1. **Schema.** Create tables, triggers, functions.
2. **L0 populate.** Insert the platform's own vocabularies into the system tenant. This is today's Flow A, limited to data the platform defines (trade statuses, code domains, tenant lookup tables, instrument codes, payment frequencies, convention kinds/terms, day counters, curve roles, leg types, product families).
3. **L0 DQ seed.** Register `system_core` datasets and their artefacts (ISO, FPML, GLEIF, images, providers).
4. **L0 publish.** Publish `system_core` into the system tenant as part of setup, not as a runtime act. A single function (`ores_refdata_bootstrap_publish_fn`) should do this, so populate.sql can call it deterministically.
5. **L1/L2 seeding.** Register the curated world and party datasets and their artefacts.
6. **Tenant creation.** Provisioner copies the L1 platform vocabulary only.
7. **Tenant onboarding.** Publish `base` (L1 world) then `party_essentials` (L2) into the tenant/party. This is today's Flow C, unchanged except for the dataset set.
8. **Test bootstrap.** A test fixture that needs published data calls the same function as step 4 and, for a tenant, steps 6–7 into a test tenant. No test asserts against a dev database's provisioning state.

### 6.3 What moves

| Change | Files |
|---|---|
| Make publishing part of database setup: add a bootstrap publish for `system_core` (and optionally a test tenant) and call it from populate. Delete the "reference data is NOT published here" block. | `populate/populate.sql:332-341`; new `create/refdata/*_bootstrap_publish_create.sql`; `populate/refdata/refdata_populate.sql` |
| Delete one side of the tables written both ways. Keep the DQ dataset and drop the direct calendar/country/tenor-schedule seeds, or keep the direct seeds and drop the datasets. Recommendation: keep DQ (versioned, resettable, per tenant) for the eight refdata datasets and for `iso.countries`; keep the direct seed only for the system party and the WRLD business centre, which are system-tenant rows. | `populate/refdata/refdata_calendars_populate.sql`, `_calendar_types_`, `_calendar_events_`, `_calendar_exceptions_`, `_currency_calendars_`, `_currency_countries_`, `_currency_pair_convention_calendars_`, `_tenor_schedules_`, `_countries_`; remove the `\ir` lines in `foundation/foundation_populate.sql:126-158` |
| Remove the spurious edge. The calendars trigger reads the DQ table, so no dependency is needed. | `refdata_dataset_dependency_populate.sql:54-58` |
| Give `payment_frequencies` and `legal_entities` a dataset, or delete their type/function/subject. Recommendation: dataset for payment frequencies (L1 vocabulary, or keep the copy and delete the function), delete the legal-entities publisher. | `dq_artefact_types_populate.sql`, `refdata_publish_from_dq_create.sql`, `registrar.cpp` |
| Replace the unscoped catalogue checks with tenant-scoped reads, or make the catalogue explicitly global (a view over the system tenant) and document it. | `create/trading/trading_basket_constituent_functions_create.sql` |
| Write the audit row when a bundle workflow completes, and honour `atomic` (or delete it). | `publish_handler.hpp`, `bundle_publish_workflow.hpp`, `dq_bundle_publication_create.sql` |
| Delete the dead `publish()` and `call_populate_function`. | `publish_service.cpp:95-180,339-401` |
| Classify the 11 system-only tables. Move L1 ones to the provisioner copy list; leave L0 ones and say so. | `iam_tenant_provisioner_create.sql`, `data_tiers.org` |
| Rewrite the docs around the three levels: delete the superseded sections, record 57 functions and 52 subjects, state the boot sequence. | `doc/plans/2026-05-14-dq-publish-pattern.org` (mark superseded), `projects/ores.dq/modeling/publication_pipeline.org`, `doc/knowledge/architecture/data_tiers.org`, `doc/knowledge/architecture/dq_artefacts.org` |

### 6.4 The strongest single recommendation

**Make publication part of bringing a database up, not a runtime act that tests must imitate.** Add one `system_core` bootstrap publish to the populate path and one tenant bootstrap publish to the test fixture, then delete one side of each dual-path table. That closes D1, D5, D12 and D13 in one wave, and it gives the three-level model a place to live.

---

## 7. Commands used (reproducible)

```text
grep -c '^create or replace function ores_refdata_publish_' projects/ores.sql/create/refdata/refdata_publish_from_dq_create.sql   # 57
grep -rn "ores_dq_datasets_upsert_fn" projects/ores.sql/populate --include=*.sql | wc -l                                          # 183 lines; parse finds 182 datasets
python3 parse of populate/**/*.sql → datasets.tsv (182), members.tsv (212), deps.tsv (160)
grep -l "insert into ores_refdata_" projects/ores.sql/populate/refdata/*.sql | wc -l                                              # 57
grep -rn "ores_dq_bundles_publish_fn" projects/ doc/                                                                             # only drop script + docs
grep -rn "insert into ores_dq_bundle_publications_tbl" projects/                                                                  # none
grep -c "publish_" projects/ores.refdata/core/src/messaging/registrar.cpp                                                         # 58 lines, 52 subjects
python3 set difference: direct-insert ∪ provisioner-copy ∪ publish-target tables                                                  # 57 direct tables = 4 triple + 10 dual + 32 copy-only + 11 system-only
```

---

## 8. Hypotheses (labelled, not proven)

- **H1.** The original booking failure was a C++ or seeding test that booked a trade with an equity basket constituent and ran against a freshly populated database. The mechanism is proven (§4, D1/D2); the exact test binary was not located, and no test in `projects/ores.sql/test/` mentions baskets.
- **H2.** The `iso.coding_schemes` edge was added when the calendars trigger validated the published coding-schemes table, and was not removed when the trigger moved to the DQ table. The commit that moved it would confirm this; the current code makes the edge unnecessary.
- **H3.** The 11 system-only tables are read through security-definer helpers or system-tenant-only screens, which is why no tenant complaint exists yet. `curve_roles` and `convention_kind_terms` are the two to check first.
