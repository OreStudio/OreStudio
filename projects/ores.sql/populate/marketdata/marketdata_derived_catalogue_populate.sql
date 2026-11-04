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
 * The Derived Source of FX Volatility Quotes
 *
 * The derived source is our own computation over the fixings the store already
 * holds. It infers the ATM, risk reversal and butterfly quotes of an FX pair from
 * the pair's daily reference fixings and publishes them as quotes of the same
 * fx_option items a vendor would quote, under its own provider, so a vendor of the
 * items ranked above it replaces it. The quotes carry no credit of the publishers
 * whose fixings fed them, and the provider carries no terms of its own.
 *
 * The rows: the provider, the session and feed the synthetic service serves it
 * through, a vendor instrument for each pair, expiry and label mapped to its
 * fx_option item, the feed's scopes, and the system party's subscription and route.
 *
 * This script is idempotent.
 */

\echo '--- Derived Source: Provider, Session and Feed ---'

insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    terms_url, terms_credit, allows_storage, credential_ref,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'derived', 0, 'Derived', 'reference',
     'The ATM, risk reversal and butterfly quotes of FX pairs, inferred by our own computation from their stored reference fixings. Plausible, not observed.',
     30, '', '', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the derived source')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_marketdata_sessions_tbl (
    code, tenant_id, version, name, supplier_code, adapter_code, environment,
    endpoint, credential_ref, settings, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('derived-local', ores_utility_system_tenant_id_fn(), 0, 'Derived local',
     'derived', 'derived', 'production', '', '', '{}', true,
     'The session the synthetic service serves the derived source through.',
     current_user, current_user, 'system.initial_load', 'Initial population of the derived source')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_marketdata_feeds_tbl (
    id, tenant_id, version, name, publisher_code, session_code, service, provider_key,
    is_default, description, modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('afd34349-1c6c-56b2-a686-97b623ee051a', ores_utility_system_tenant_id_fn(), 0,
     'derived-fx-vol', 'derived', 'derived-local', 'end_of_day', '', true,
     'The FX volatility quotes the derived source infers once a day, after the day''s fixings.',
     current_user, current_user, 'system.initial_load', 'Initial population of the derived source')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Derived Source: Vendor Instruments, Mappings and Scopes ---'

-- A vendor id names a pair, an expiry and a label, such as EURUSD/1Y/25RR, in the
-- derived source's own notation. Each maps to the fx_option quote of the pair in its
-- base/quote order.
create temporary table t_derived_inventory (
    external_id text, oresmd_uri text
) on commit drop;

insert into t_derived_inventory (external_id, oresmd_uri)
select p.base || p.quote || '/' || e.expiry || '/' || l.label,
       'oresmd://fx/' || p.base || '?type=quote&instrument=fx_option&quote=rate_lnvol&ccy='
           || p.quote || '&expiry=' || e.expiry || '&strike_label=' || l.label
from (values ('EUR', 'USD'), ('GBP', 'USD'), ('USD', 'JPY'), ('USD', 'CHF'),
             ('AUD', 'USD'), ('USD', 'CAD'), ('NZD', 'USD'), ('EUR', 'GBP'),
             ('EUR', 'JPY'), ('EUR', 'CHF')) as p (base, quote)
cross join (values ('1W'), ('1M'), ('2M'), ('3M'), ('6M'), ('9M'), ('1Y'), ('2Y')) as e (expiry)
cross join (values ('ATM'), ('25RR'), ('25BF'), ('10RR'), ('10BF')) as l (label);

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5('derived|derived_fx_vol|' || t.external_id || '|rate_lnvol')::uuid,
       ores_utility_system_tenant_id_fn(), 0, 'derived', 'derived_fx_vol', t.external_id,
       'rate_lnvol', false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the derived source'
from t_derived_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = 'derived'
      and v.id_scheme = 'derived_fx_vol'
      and v.external_id = t.external_id
      and v.field = 'rate_lnvol'
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the derived source'
from t_derived_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = 'derived'
 and v.id_scheme = 'derived_fx_vol'
 and v.external_id = t.external_id
 and v.field = 'rate_lnvol'
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_feed_scopes_tbl (
    id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0,
       'afd34349-1c6c-56b2-a686-97b623ee051a', 'fx', m.oresmd_uri, v.id,
       current_user, current_user, 'system.initial_load', 'Initial population of the derived source'
from ores_marketdata_market_data_vendor_instruments_tbl v
join ores_marketdata_market_data_mappings_tbl m
  on m.vendor_instrument_id = v.id
 and m.tenant_id = ores_utility_system_tenant_id_fn()
 and m.valid_to = ores_utility_infinity_timestamp_fn()
where v.tenant_id = ores_utility_system_tenant_id_fn()
  and v.provider_code = 'derived'
  and v.valid_to = ores_utility_infinity_timestamp_fn()
  and not exists (
      select 1 from ores_marketdata_feed_scopes_tbl s
      where s.tenant_id = ores_utility_system_tenant_id_fn()
        and s.feed_id = 'afd34349-1c6c-56b2-a686-97b623ee051a'
        and s.item = m.oresmd_uri
        and s.valid_to = ores_utility_infinity_timestamp_fn()
  );

\echo '--- Derived Source: Subscription and Route of the System Party ---'

do $$
declare
    v_party uuid := ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn());
begin
    if v_party is null then
        raise notice 'No system party yet: the derived source subscription waits for it';
        return;
    end if;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'aa096fb1-5d92-5b63-a1e7-e1d83ac4a242', ores_utility_system_tenant_id_fn(), 0, v_party,
        'derived', true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the derived FX volatility quotes for the surface generators'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '0ab9581e-a2c2-5be1-969e-7964b1352a55', ores_utility_system_tenant_id_fn(), 0, v_party,
        'derived', 'fx', 'afd34349-1c6c-56b2-a686-97b623ee051a',
        current_user, current_user, 'system.initial_load',
        'The system party reads the derived FX volatility quotes through the derived source'
    ) on conflict do nothing;
end $$;
