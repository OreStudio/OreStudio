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
 * The Fixing Publishers Catalogue
 *
 * A fixing states who published it, so every administrator of the fixings the
 * run states is a publisher of the catalogue, of kind reference, with one source
 * served by the simulator on the session synthetic-local. The simulator states
 * the value from the run's own market, as if the administrator had published it
 * at its own fixing time; the source is the one that states it, and the
 * administrator is the one the item belongs to. The design is in
 * doc/knowledge/architecture/synthetic_generation_of_fixings.org.
 *
 * A public adapter serves some of these indices already, and the publishers of
 * those are installed by the official and the overnight rates catalogues. The
 * rows here are the administrators whose fixings have no public adapter in the
 * run: the WM/R cuts, whose benchmark is licensed, and the rates and the money
 * market indices that no free source carries.
 *
 * Three of the publishers state a term index the run computes rather than
 * observes: GPW Benchmark's WIBOR 6M from the run's own POLONIA curve, CME
 * Group's Term SOFR 1M from its SOFR curve, and Banco de la República's IBR 3M
 * from its IBR curve. The index is the administrator's, so it is stated under
 * the administrator's name, and the series' derivation_kind says the run
 * computed it. No publisher is a mechanism: there is no provider called derived
 * for these three.
 *
 * LIBOR is the one publisher whose settings the market retired. ICE Benchmark
 * Administration ceased to publish them between 2021 and 2023, and the teaching
 * material still uses them, so iba publishes them as if the settings continued.
 * The publisher's own description says so, so a reader does not read the
 * store's LIBOR fixings as stale data.
 *
 * The sources carry a shadow rate card of the same shape as any other source, so
 * a party that runs on a simulated publisher sees the bill the same use would
 * cost on a live one. This script is idempotent.
 */

\echo '--- Fixing Publishers ---'

insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order, precedence,
    allows_storage, modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'wmr', 0, 'WM/Reuters FX Benchmarks', 'reference',
     'LSEG administers the WM/Reuters FX benchmarks, whose rates are taken at seven cuts a day: the cut codes the reference data holds. The benchmark is licensed, so the run states it from its own market.',
     50, 50, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'iba', 0, 'ICE Benchmark Administration', 'reference',
     'ICE Benchmark Administration set the LIBOR settings until the market retired them between 2021 and 2023. The run publishes them as if the settings continued, because the teaching material uses them.',
     51, 51, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'emmi', 0, 'European Money Markets Institute', 'reference',
     'EMMI administers EURIBOR, the euro interbank offered rate, fixed each TARGET business day.',
     52, 52, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'hkab', 0, 'The Hong Kong Association of Banks', 'reference',
     'The Hong Kong Association of Banks fixes HIBOR, the Hong Kong interbank offered rate, and HONIA, the Hong Kong dollar overnight index average.',
     53, 53, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'gpw', 0, 'GPW Benchmark', 'reference',
     'GPW Benchmark administers WIBOR, the Warsaw interbank offered rate, and publishes POLONIA. The run states the WIBOR settings from its own POLONIA curve, so the index the administrator owns is stated under its name.',
     54, 54, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'banrep', 0, 'Banco de la República', 'reference',
     'Banco de la República publishes the IBR, the Colombian reference banking indicator, and its overnight rate.',
     55, 55, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'snb', 0, 'Swiss National Bank', 'reference',
     'The Swiss National Bank publishes SARON, the Swiss average rate overnight.',
     56, 56, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'danmarks-nationalbank', 0, 'Danmarks Nationalbank', 'reference',
     'Danmarks Nationalbank publishes DESTR, the Danish short-term rate.',
     57, 57, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'mnb', 0, 'Magyar Nemzeti Bank', 'reference',
     'The Magyar Nemzeti Bank publishes HUFONIA, the Hungarian overnight index average.',
     58, 58, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'bok', 0, 'Bank of Korea', 'reference',
     'The Bank of Korea publishes KOFR, the Korea overnight risk-free rate.',
     59, 59, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'nbp', 0, 'Narodowy Bank Polski', 'reference',
     'The Narodowy Bank Polski publishes POLONIA, the Polish overnight index average.',
     60, 60, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'rbnz', 0, 'Reserve Bank of New Zealand', 'reference',
     'The Reserve Bank of New Zealand publishes NZONIA, the New Zealand overnight index average.',
     61, 61, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'sarb', 0, 'South African Reserve Bank', 'reference',
     'The South African Reserve Bank publishes ZARONIA, the South African rand overnight index average.',
     62, 62, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'boi', 0, 'Bank of Israel', 'reference',
     'The Bank of Israel publishes SHIR, the shekel interbank rate.',
     63, 63, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'nifc', 0, 'National Interbank Funding Center', 'reference',
     'The National Interbank Funding Center publishes SHIBOR, the Shanghai interbank offered rate.',
     64, 64, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'fbil', 0, 'Financial Benchmarks India', 'reference',
     'Financial Benchmarks India publishes MIBOR, the Mumbai interbank offered rate.',
     65, 65, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'tba', 0, 'Taiwan Bankers Association', 'reference',
     'The Taiwan Bankers Association publishes TAIBOR, the Taipei interbank offered rate.',
     66, 66, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'bist', 0, 'Borsa İstanbul', 'reference',
     'Borsa İstanbul publishes TLREF, the Turkish lira reference rate.',
     67, 67, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers'),
    (ores_utility_system_tenant_id_fn(), 'cme', 0, 'CME Group', 'reference',
     'CME Group administers the Term SOFR rates, which it publishes from the overnight SOFR it also administers. The run states the term settings from its own SOFR curve, so the index the administrator owns is stated under its name.',
     68, 68, true, current_user, current_user, 'system.initial_load',
     'Initial population of the fixing publishers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Fixing Sources ---'

-- One source for each publisher, on the simulator's session. The source is the publisher's default,
-- so a route that names the publisher reaches it without naming the source.
insert into ores_marketdata_feeds_tbl (
    id, tenant_id, version, name, publisher_code, session_code, service, provider_key,
    is_default, description, modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
       p.code || '-a', p.code, 'synthetic-local', 'end_of_day', '', true, p.description,
       current_user, current_user, 'system.initial_load',
       'Initial population of the fixing sources'
from (values
    ('wmr', 'The WM/R cuts the simulator states from the run''s own market, at the cut times the reference data holds.'),
    ('iba', 'The LIBOR settings the simulator states, as if the market had not retired them.'),
    ('emmi', 'The EURIBOR settings the simulator states from the run''s own market.'),
    ('hkab', 'The HIBOR settings and the HONIA overnight rate the simulator states.'),
    ('gpw', 'The WIBOR settings the simulator states from the run''s own POLONIA curve.'),
    ('banrep', 'The IBR the simulator states from the run''s own market.'),
    ('snb', 'SARON, as the simulator states it.'),
    ('danmarks-nationalbank', 'DESTR, as the simulator states it.'),
    ('mnb', 'HUFONIA, as the simulator states it.'),
    ('bok', 'KOFR, as the simulator states it.'),
    ('nbp', 'POLONIA, as the simulator states it.'),
    ('rbnz', 'NZONIA, as the simulator states it.'),
    ('sarb', 'ZARONIA, as the simulator states it.'),
    ('boi', 'SHIR, as the simulator states it.'),
    ('nifc', 'SHIBOR settings, as the simulator states them.'),
    ('fbil', 'MIBOR settings, as the simulator states them.'),
    ('tba', 'TAIBOR settings, as the simulator states them.'),
    ('bist', 'TLREF, as the simulator states it.'),
    ('cme', 'The Term SOFR settings the simulator states from the run''s own SOFR curve.')
) as p(code, description)
on conflict do nothing;

\echo '--- Fixing Rate Cards ---'

-- A shadow bill of the same shape as a live source's, so a party that runs on a simulated publisher
-- sees what the same use would cost.
insert into ores_marketdata_rate_card_components_tbl (
    id, tenant_id, version, feed_id, driver, licence_right, currency, charge_micros,
    effective_from, modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, s.id, c.driver,
       c.licence_right, 'USD', c.charge_micros, timestamptz '2026-01-01 00:00:00+00',
       current_user, current_user, 'system.initial_load', 'Initial population of the fixing rate cards'
from ores_marketdata_feeds_tbl s
cross join (values
    ('fixed_per_period',   '',            100000000::bigint),
    ('per_instrument',     '',                50000::bigint),
    ('per_party',          '',             10000000::bigint),
    ('per_licence',        'non_display',   5000000::bigint),
    ('per_licence',        'derived',       8000000::bigint),
    ('per_thousand_ticks', '',                20000::bigint)
) as c(driver, licence_right, charge_micros)
where s.tenant_id = ores_utility_system_tenant_id_fn()
  and s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.session_code = 'synthetic-local'
  and s.name in ('wmr-a', 'iba-a', 'emmi-a', 'hkab-a', 'gpw-a', 'banrep-a', 'snb-a',
                 'danmarks-nationalbank-a', 'mnb-a', 'bok-a', 'nbp-a', 'rbnz-a', 'sarb-a',
                 'boi-a', 'nifc-a', 'fbil-a', 'tba-a', 'bist-a', 'cme-a')
  and not exists (
      select 1 from ores_marketdata_rate_card_components_tbl x
      where x.tenant_id = ores_utility_system_tenant_id_fn()
        and x.feed_id = s.id
        and x.driver = c.driver
        and x.licence_right = c.licence_right
        and x.valid_to = ores_utility_infinity_timestamp_fn()
  );

\echo '--- Fixing Subscriptions and Routes of the System Party ---'

-- The system party takes every fixing publisher and routes to its source, so a fixing stated by the
-- simulator is stored once, for every party to read. The route trigger refuses a route without the
-- subscription, so the subscriptions are written first.
do $$
declare
    v_party uuid := ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn());
begin
    if v_party is null then
        raise notice 'No system party yet: the fixing subscriptions wait for it';
        return;
    end if;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v_party, p.code,
           true, true, false, current_user, current_user, 'system.initial_load',
           'The system party takes the fixings of ' || p.code
    from (values ('wmr'), ('iba'), ('emmi'), ('hkab'), ('gpw'), ('banrep'), ('snb'),
                 ('danmarks-nationalbank'), ('mnb'), ('bok'), ('nbp'), ('rbnz'), ('sarb'),
                 ('boi'), ('nifc'), ('fbil'), ('tba'), ('bist'), ('cme')) as p(code)
    on conflict do nothing;

    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v_party, p.code,
           p.asset_class, f.id, current_user, current_user, 'system.initial_load',
           'The system party reads the fixings of ' || p.code || ' through ' || p.code || '-a'
    from (values
        ('wmr', 'fx'), ('iba', 'ir'), ('emmi', 'ir'), ('hkab', 'ir'), ('gpw', 'ir'),
        ('banrep', 'ir'), ('snb', 'ir'), ('danmarks-nationalbank', 'ir'), ('mnb', 'ir'),
        ('bok', 'ir'), ('nbp', 'ir'), ('rbnz', 'ir'), ('sarb', 'ir'), ('boi', 'ir'),
        ('nifc', 'ir'), ('fbil', 'ir'), ('tba', 'ir'), ('bist', 'ir'), ('cme', 'ir')
    ) as p(code, asset_class)
    join ores_marketdata_feeds_tbl f
      on f.publisher_code = p.code
     and f.name = p.code || '-a'
     and f.valid_to = ores_utility_infinity_timestamp_fn()
    on conflict do nothing;
end $$;

-- Summary
select 'fixing publishers' as entity, count(*) as count
from ores_marketdata_providers_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and code in ('wmr', 'iba', 'emmi', 'hkab', 'gpw', 'banrep', 'snb', 'danmarks-nationalbank',
               'mnb', 'bok', 'nbp', 'rbnz', 'sarb', 'boi', 'nifc', 'fbil', 'tba', 'bist',
               'cme')
union all
select 'fixing sources', count(*)
from ores_marketdata_feeds_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
  and session_code = 'synthetic-local'
  and name in ('wmr-a', 'iba-a', 'emmi-a', 'hkab-a', 'gpw-a', 'banrep-a', 'snb-a',
               'danmarks-nationalbank-a', 'mnb-a', 'bok-a', 'nbp-a', 'rbnz-a', 'sarb-a',
               'boi-a', 'nifc-a', 'fbil-a', 'tba-a', 'bist-a', 'cme-a')
union all
select 'fixing routes of the system party', count(*)
from ores_marketdata_routes_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and party_id = ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn())
  and valid_to = ores_utility_infinity_timestamp_fn()
  and publisher_code in ('wmr', 'iba', 'emmi', 'hkab', 'gpw', 'banrep', 'snb',
                         'danmarks-nationalbank', 'mnb', 'bok', 'nbp', 'rbnz', 'sarb',
                         'boi', 'nifc', 'fbil', 'tba', 'bist', 'cme');
