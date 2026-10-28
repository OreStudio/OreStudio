/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * The Acme curve this task publishes, and the market behind it.
 *
 * The Acme tenant holds no curve bootstrap configuration of its own: the one the
 * seed ships belongs to the system party and reads the synthetic FOMC feed's
 * meeting-dated quotes. The demonstration therefore needs a curve of Acme's. It
 * is a two-pillar USD funding curve on the spot-starting convention the seed
 * resolves (SPOT to 12M, 12M to 2Y), owned and quoted by ACME Corporation US
 * Inc. That office owns it because a party sees only itself and its own
 * children, and the Trading persona that raises a publish works in that office;
 * a curve of the holding group would be invisible to the person who has to
 * publish it. The whole approval flow therefore runs against Acme's own data
 * and no system party's.
 *
 * The configuration names itself USD-SOFR-ACME, the name the screens draw, and
 * it is a FUNDING curve, so discount_curve_config_id is null: nothing stands in
 * for a discount curve the curve does not have.
 *
 * The ids are RFC 4122 uuids, variant bits included: the BFF parses an id with
 * a strict uuid reader on the routes that take one in a body, so an id whose
 * variant nibble is 7 is refused as a malformed uuid before any read runs.
 *
 * The quote keys are the ones the ingest loop and the bootstrap derive from the
 * configuration: a spot pillar is IR_SWAP/RATE/USD/0D/1D/<end tenor>, and a
 * pillar that starts at a period is IR_SWAP/RATE/USD/<start tenor>/1D/<end
 * tenor>, because a template that names both ends of a pillar names two
 * periods. Each series is the key without its point, so a series and its quote
 * spell one identity.
 *
 * This file is evidence, not a seed: it is applied to the provisioned Acme
 * database by hand to drive the live flow, and it is idempotent so the flow can
 * be run again.
 */

\set ON_ERROR_STOP on

\echo '--- Acme curve bootstrap configuration ---'

-- The rows are guarded rather than upserted: the insert trigger refuses a
-- second write at version zero before ON CONFLICT could resolve it.
insert into ores_marketdata_market_series_tbl (
    id, tenant_id, version, party_id, series_subclass, oresmd_uri,
    provider_code, derivation_kind, derivation_config_id, derivation_config_version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select v.id, v.tenant_id, v.version, v.party_id, v.series_subclass, v.oresmd_uri,
       v.provider_code, v.derivation_kind, v.derivation_config_id, v.derivation_config_version,
       v.modified_by, v.performed_by, v.change_reason_code, v.change_commentary
from (values
    (
        'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5b'::uuid,
        '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
        0,
        '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid,
        'yield', 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-ACME',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
        'Acme USD SOFR curve: republish output, stamped IR_CURVE_BOOTSTRAP on first republish'
    ),
    (
        'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5c',
        '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
        0,
        '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid,
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
        'Acme USD SOFR curve: the spot-starting pillar''s series'
    ),
    (
        'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5d',
        '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
        0,
        '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid,
        'yield', 'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=12M&tenor=1D',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
        'Acme USD SOFR curve: the 12M-starting pillar''s series'
    ),
    (
        'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5e',
        '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
        0,
        '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid,
        'yield', 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-ADD',
        'ore', 'OBSERVED', ores_utility_nil_uuid_fn(), 0,
        'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
        'A second curve''s output series, so a blueprint can be added by hand in the walkthrough'
    )
) as v(id, tenant_id, version, party_id, series_subclass, oresmd_uri, provider_code,
       derivation_kind, derivation_config_id, derivation_config_version,
       modified_by, performed_by, change_reason_code, change_commentary)
where not exists (
    select 1 from ores_marketdata_market_series_tbl s
    where s.tenant_id = v.tenant_id::uuid and s.id = v.id::uuid
      and s.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_series_asset_classes_tbl (
    market_series_id, tenant_id, asset_class_code, version,
    modified_by, performed_by, change_reason_code, change_commentary
)
select s.id, s.tenant_id, 'interest_rates', 0,
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user', 'system.initial_load',
       'The Acme USD SOFR curve is an interest rates series'
from ores_marketdata_market_series_tbl s
where s.id in ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5b',
               'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5c',
               'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5d',
               'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5e')
  and s.valid_to = ores_utility_infinity_timestamp_fn()
on conflict (tenant_id, market_series_id, asset_class_code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- the identities the pickers read ---'

-- A series written straight into the table has no identity row, and the screen's
-- series pickers read the identity projection and not the table. Without these the
-- party holds three series that no picker offers.
insert into ores_marketdata_market_series_identity_tbl (
    series_id, tenant_id, party_id, identity_kind,
    asset_class, instrument_type, quote_type, ccy, curve_id, tenor, fwd_start
)
select v.series_id, v.tenant_id, v.party_id, 'series',
       v.asset_class, v.instrument_type, v.quote_type, v.ccy, v.curve_id, v.tenor, v.fwd_start
from (values
    ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5b'::uuid, '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
     '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid, 'ir', 'discount', 'rate', 'USD', 'USD-SOFR-ACME',
     null::text, null::text),
    ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5c'::uuid, '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
     '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid, 'ir', 'ir_swap', 'rate', 'USD', null,
     '1D', '0D'),
    ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5d'::uuid, '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
     '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid, 'ir', 'ir_swap', 'rate', 'USD', null,
     '1D', '12M'),
    ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5e'::uuid, '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
     '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid, 'ir', 'discount', 'rate', 'USD', 'USD-SOFR-ADD',
     null::text, null::text)
) as v(series_id, tenant_id, party_id, asset_class, instrument_type, quote_type, ccy, curve_id,
       tenor, fwd_start)
where not exists (
    select 1 from ores_marketdata_market_series_identity_tbl i
    where i.series_id = v.series_id
);

\echo '--- the two quotes the bootstrap reads ---'

insert into ores_marketdata_market_observations_tbl (
    id, tenant_id, party_id, series_id, observation_datetime,
    oresmd_uri, value, source, source_id, valid_from, valid_to
)
select v.id, '510343ba-e902-49a9-86de-6d9c1b3ebab4',
       '0462cff9-f09f-4d6e-b1e1-431de783217b', v.series_id,
       timestamptz '2026-10-27 12:00:00+00', v.oresmd_uri, v.value,
       'acme_demo.curve_publish_seed',
       -- catalogue_ids::ore_derived_source_id(), the source a bootstrap writes.
       '437b14be-6443-4492-b421-a51ce34f318a'::uuid,
       clock_timestamp(), ores_utility_infinity_timestamp_fn()
from (values
    ('c1a2b3c4-d5e6-4f80-8a9b-0c1d2e3f4a62'::uuid,
     'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5c'::uuid,
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=12M',
     '0.0432'),
    ('c1a2b3c4-d5e6-4f80-8a9b-0c1d2e3f4a63'::uuid,
     'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5d'::uuid,
     'oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=12M&tenor=1D&term=2Y',
     '0.0395')
) as v(id, series_id, oresmd_uri, value)
where not exists (
    select 1 from ores_marketdata_market_observations_tbl o
    where o.tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
      and o.series_id = v.series_id
      and o.oresmd_uri = v.oresmd_uri
      and o.observation_datetime = timestamptz '2026-10-27 12:00:00+00'
      and o.valid_to = ores_utility_infinity_timestamp_fn()
);

\echo '--- the configuration and its pillars ---'

insert into ores_refdata_ir_curve_bootstrap_configs_tbl (
    id, tenant_id, version, party_id, name, currency_code, index_tenor,
    output_series_id, curve_family_role, discount_curve_config_id,
    interpolation_method, day_count_convention, tenor_convention_code,
    split_tenor_code, modified_by, performed_by, change_reason_code,
    change_commentary, valid_from, valid_to
)
select
    v.id, v.tenant_id, v.version, v.party_id, v.name, v.currency_code, v.index_tenor,
    v.output_series_id::uuid, v.curve_family_role, v.discount_curve_config_id::uuid,
    v.interpolation_method, v.day_count_convention, v.tenor_convention_code,
    v.split_tenor_code, v.modified_by, v.performed_by, v.change_reason_code,
    v.change_commentary, v.valid_from, v.valid_to
from (values
    (
        'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6'::uuid,
        '510343ba-e902-49a9-86de-6d9c1b3ebab4'::uuid,
        0,
        '0462cff9-f09f-4d6e-b1e1-431de783217b'::uuid,
        'USD-SOFR-ACME', 'USD', '1D',
        'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5b',
        'FUNDING', null,
        'FLAT_FORWARD_THEN_LOG_LINEAR', 'A360', 'RATES_SPOT_FORWARD',
        '12M',
        'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user',
        'system.initial_load',
        'The Acme USD SOFR funding curve the demo publishes',
        clock_timestamp(), ores_utility_infinity_timestamp_fn()
    )
) as v(id, tenant_id, version, party_id, name, currency_code, index_tenor, output_series_id,
       curve_family_role, discount_curve_config_id, interpolation_method, day_count_convention,
       tenor_convention_code, split_tenor_code, modified_by, performed_by, change_reason_code,
       change_commentary, valid_from, valid_to)
where not exists (
    select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl c
    where c.tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
      and c.id = 'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6'
      and c.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_refdata_ir_curve_bootstrap_pillars_tbl (
    id, tenant_id, version, party_id, bootstrap_config_id, sequence_index,
    start_tenor_code, end_tenor_code, curve_role_code,
    modified_by, performed_by, change_reason_code, change_commentary,
    valid_from, valid_to
)
select v.id, '510343ba-e902-49a9-86de-6d9c1b3ebab4', 0,
       '0462cff9-f09f-4d6e-b1e1-431de783217b',
       'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6', v.sequence_index,
       v.start_tenor_code, v.end_tenor_code, v.curve_role_code,
       'ores_eager_maxwell_ddl_user', 'ores_eager_maxwell_ddl_user',
       'system.initial_load',
       'Acme USD SOFR curve pillar ' || v.sequence_index,
       clock_timestamp(), ores_utility_infinity_timestamp_fn()
from (values
    ('e1f2a3b4-c5d6-4e7f-8091-a2b3c4d5e6f7'::uuid, 0, 'SPOT', '12M', 'DEPOSIT'),
    ('e1f2a3b4-c5d6-4e7f-8091-a2b3c4d5e6f8'::uuid, 1, '12M', '2Y', 'SWAP')
) as v(id, sequence_index, start_tenor_code, end_tenor_code, curve_role_code)
where not exists (
    select 1 from ores_refdata_ir_curve_bootstrap_pillars_tbl p
    where p.tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
      and p.bootstrap_config_id = 'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6'
      and p.sequence_index = v.sequence_index
      and p.valid_to = ores_utility_infinity_timestamp_fn()
);

-- Summary.
select 'Acme curve bootstrap configuration' as entity, count(*) as count
from ores_refdata_ir_curve_bootstrap_configs_tbl
where tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
  and id = 'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6'
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'its pillars', count(*)
from ores_refdata_ir_curve_bootstrap_pillars_tbl
where tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
  and bootstrap_config_id = 'd0e1f2a3-b4c5-4d6e-8f80-91a2b3c4d5e6'
  and valid_to = ores_utility_infinity_timestamp_fn()
union all
select 'its quotes', count(*)
from ores_marketdata_market_observations_tbl
where tenant_id = '510343ba-e902-49a9-86de-6d9c1b3ebab4'
  and series_id in ('c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5c',
                    'c1a2b3c4-d5e6-4f70-8a9b-0c1d2e3f4a5d')
  and valid_to = ores_utility_infinity_timestamp_fn();
