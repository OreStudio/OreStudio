/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Seed Profile Population Script
 *
 * Seeds the two starting points provisioning offers: Operational (the row
 * whose code is =empty_operational=) and the ACME demo. A seed profile is
 * registered data, so the first two live here and a new one is a row. The
 * cards' copy is the accepted prototype's.
 *
 * The step kinds come from the catalogue in code: publish_bundle,
 * import_lei_hierarchy, provision_party, load_staff, attach_photos and
 * start_market_feeds. ACME orders all six, because it produces every kind of
 * datum a demonstration needs. Operational orders the three that create real
 * data and no test data at all.
 *
 * Operational is offered first: it is the production starting point, and the
 * demonstration is the deliberate second choice.
 *
 * The insert trigger forces the system tenant on every row, so this script
 * states no tenant and the rows are readable by whoever provisions. The
 * script is idempotent: a profile that exists is left alone, and its steps
 * and parameters with it.
 */

\echo '--- Seed Profiles ---'

-- Operational: for real use, so the tenant it makes is a production one. It
-- publishes the base bundle, imports the parties under a GLEIF root LEI the
-- administrator names, and creates no test data. It prefills no tenant detail,
-- because the administrator supplies the tenant and its own identity.
--
-- Neither card asks its administrator to change the password the person who
-- provisioned the tenant gave it: the choice is the profile's to make, and a
-- deployment that wants the change states it here.
--
-- ACME demo: for demos and testing, so its tenant is an evaluation one. Its
-- tenant type is the only tenant detail the form does not ask for, and the
-- starting point the person chose states it.
--
-- The card copy is the prototype's: the accepted starting-point design states
-- the tagline, the audience line and the three bullets, and a new profile
-- states its own in its row.
insert into ores_iam_seed_profiles_tbl (
    id, code, name, summary, audience, bullets_json,
    tenant_type, tenant_name, tenant_code, tenant_hostname, admin_username, admin_email,
    inherits_admin_password, force_password_change, display_order,
    version, modified_by, performed_by, change_reason_code, change_commentary
) values (
    gen_random_uuid(),
    'empty_operational',
    'Operational',
    'Production-ready setup',
    'For real use',
    '["Standard reference data and counterparties", "Your legal entities, from their LEI", "No test data"]'::jsonb,
    'production', '', '', null, '', '',
    false, false, 10,
    0, current_user, current_user, 'system.initial_load',
    'Initial population of seed profiles'
), (
    gen_random_uuid(),
    'acme_demo',
    'ACME demo',
    'Pre-configured sandbox',
    'For demos and testing',
    '["4 legal entities, books and desks", "45 staff to sign in as", "Live synthetic market data"]'::jsonb,
    'evaluation', 'Acme Corporation', 'acme_corporation', 'acme_corporation',
    'tenant_admin', 'admin@acme_corporation.com',
    true, false, 20,
    0, current_user, current_user, 'system.initial_load',
    'Initial population of seed profiles'
), (
    gen_random_uuid(),
    'gleif_entity',
    'GLEIF entity',
    'A tenant built around a public registry hierarchy',
    'For demos and testing',
    '["A parent entity the deployment holds, from GLEIF", "Its hierarchy becomes the tenant''s parties", "The tenant is not that entity"]'::jsonb,
    'evaluation', '', '', null, 'tenant_admin', '',
    false, false, 15,
    0, current_user, current_user, 'system.initial_load',
    'Initial population of seed profiles'
)
on conflict (code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- The ordered step kinds. The arguments are the step kind's own: the kind
-- fixes their shape in code and the profile supplies the values. The bundles
-- differ between the two profiles, which is what makes them data.
--
-- A bundle publishes some of its members only when a starting point asks for
-- them, which is what "opted_in_datasets" names. The counterparty set is one of
-- those, and its size is the value of the profile's own "counterparty_size"
-- parameter: the row names the parameter in braces rather than one of its
-- values, so a person who chooses the large set gets the large set.
insert into ores_iam_seed_profile_steps_tbl (
    id, tenant_id, seed_profile_id, step_kind, arguments_json, display_order,
    version, modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), p.id,
       v.step_kind, v.arguments_json, v.display_order,
       0, current_user, current_user, 'system.initial_load',
       'Initial population of seed profile steps'
from ores_iam_seed_profiles_tbl p
cross join (values
    ('empty_operational', 'publish_bundle', 10, '{"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{counterparty_size}"]}'::jsonb),
    ('empty_operational', 'import_lei_hierarchy', 20, '{"bundles": ["lei_hierarchy"]}'::jsonb),
    ('empty_operational', 'provision_party', 30, '{"bundles": ["party_essentials"]}'::jsonb),
    ('gleif_entity', 'publish_bundle', 10, '{"bundles": ["base"], "opted_in_datasets": ["gleif.lei_counterparties.{counterparty_size}"]}'::jsonb),
    ('gleif_entity', 'import_lei_hierarchy', 20, '{"bundles": ["lei_hierarchy"]}'::jsonb),
    ('gleif_entity', 'provision_party', 30, '{"bundles": ["party_essentials"]}'::jsonb),
    ('acme_demo', 'publish_bundle', 10, '{"bundles": ["base", "risk_management"], "opted_in_datasets": ["gleif.lei_counterparties.small"]}'::jsonb),
    ('acme_demo', 'import_lei_hierarchy', 20, '{"bundles": ["acme_lei_import"], "root_lei": "9695ACMEGROUP0000030"}'::jsonb),
    ('acme_demo', 'provision_party', 30, '{"bundles": ["party_essentials"]}'::jsonb),
    ('acme_demo', 'load_staff', 40, '{"parties": [
        {"name": "Acme Corporation Plc", "bundles": ["acme_group"], "default": true},
        {"name": "ACME Corporation UK plc", "bundles": ["acme_uk"]},
        {"name": "ACME Corporation US Inc", "bundles": ["acme_us"]},
        {"name": "ACME Corporation HK Ltd", "bundles": ["acme_hk"]}
    ]}'::jsonb),
    ('acme_demo', 'attach_photos', 50, '{"party_logo": "acme_party_logo", "parties": [
        {"name": "Acme Corporation Plc", "dataset": "acme.acme_group.accounts"},
        {"name": "ACME Corporation UK plc", "dataset": "acme.acme_uk.accounts"},
        {"name": "ACME Corporation US Inc", "dataset": "acme.acme_us.accounts"},
        {"name": "ACME Corporation HK Ltd", "dataset": "acme.acme_hk.accounts"}
    ]}'::jsonb),
    ('acme_demo', 'start_market_feeds', 60, '{"bundles": ["synthetic_realistic_2026", "synthetic_ore_samples_2016", "marketdata.reference_vintage_2026_05_05"], "theme": "synthetic.themes.realistic_2026"}'::jsonb)
) as v(code, step_kind, display_order, arguments_json)
where p.code = v.code
  and p.valid_to = ores_utility_infinity_timestamp_fn()
on conflict (tenant_id, seed_profile_id, step_kind)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- The parameters the form declares. A parameter is what the administrator
-- supplies; the step kinds and the demonstration take nothing.
--
-- Operational's pair is the contract's: a GLEIF root LEI to import the parties
-- under, and the size of the counterparty set it publishes. The size is a
-- declared choice rather than a number, so a value the run cannot use is not
-- typeable.
insert into ores_iam_seed_profile_parameters_tbl (
    id, tenant_id, seed_profile_id, name, label, data_type, choices_json,
    default_value, is_required, description, display_order,
    version, modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), p.id,
       v.name, v.label, v.data_type, v.choices_json, v.default_value,
       v.is_required, v.description, v.display_order,
       0, current_user, current_user, 'system.initial_load',
       'Initial population of seed profile parameters'
from ores_iam_seed_profiles_tbl p
cross join (values
    ('empty_operational', 'root_lei', 'Root legal entity', 'legal_entity', null, '', false,
     'The top legal entity of the tenant, found by name or LEI among the entities '
     'the deployment holds. Its hierarchy becomes the tenant''s parties. A person '
     'who has none yet leaves it empty: the import then has nothing to read and '
     'says so, and the parties are added later.', 10),
    ('empty_operational', 'counterparty_size', 'Counterparty set', 'choice',
     '["small", "large"]'::jsonb, 'small', true,
     'small is about 13k GLEIF counterparties; large is about 500k.', 20),
    ('gleif_entity', 'root_lei', 'Parent legal entity', 'legal_entity', null, '', true,
     'Search the legal entities this deployment holds, and the one you choose becomes '
     'the tenant''s hierarchy.', 10),
    ('gleif_entity', 'counterparty_size', 'Counterparty set', 'choice',
     '["small", "large"]'::jsonb, 'small', true,
     'small is about 13k GLEIF counterparties; large is about 500k.', 20)
) as v(code, name, label, data_type, choices_json, default_value, is_required, description, display_order)
where p.code = v.code
  and p.valid_to = ores_utility_infinity_timestamp_fn()
on conflict (tenant_id, seed_profile_id, name)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'Seed Profiles' as entity, count(*) as count
from ores_iam_seed_profiles_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

select 'Seed Profile Steps' as entity, count(*) as count
from ores_iam_seed_profile_steps_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

select 'Seed Profile Parameters' as entity, count(*) as count
from ores_iam_seed_profile_parameters_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
