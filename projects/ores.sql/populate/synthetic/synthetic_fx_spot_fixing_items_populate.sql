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
 * Synthetic FX Spot Fixing Items Population Script
 *
 * Ranks, for every FX spot config artefact whose price source is 'fixing', the
 * public fixings it may start from: each official provider's item of the
 * config's pair, published as the feed quotes it or the other way round. The
 * order is the providers' (ECB, IMF, Bank of Canada, Central Bank of the
 * Republic of Turkiye), and within a provider an item of the pair as quoted
 * comes before one of its inverse. A pair no official provider fixes gets no
 * item and keeps its configured level.
 *
 * Execution order: after marketdata_populate.sql, whose official catalogue
 * holds the mappings the items are read from.
 *
 * This script is idempotent.
 */

do $$
declare
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_providers text[] := array['ecb', 'imf', 'boc', 'tcmb'];
    v_count integer;
begin
    insert into ores_dq_fx_spot_fixing_items_artefact_tbl (
        dataset_id, tenant_id, base_currency_code, quote_currency_code,
        rank, item, provider_code, policy, window_days
    )
    select c.dataset_id, c.tenant_id, c.base_currency_code, c.quote_currency_code,
           (row_number() over (
                partition by c.dataset_id, c.base_currency_code, c.quote_currency_code
                order by array_position(v_providers, f.provider_code), f.inverted, f.item
           ) - 1)::integer,
           f.item, f.provider_code, 'latest_on_or_before_clock', 90
    from ores_dq_fx_spot_generation_configs_artefact_tbl c
    join lateral (
        select distinct m.oresmd_uri as item, vi.provider_code,
               (m.oresmd_uri like 'oresmd://fx/' || c.quote_currency_code || '?%') as inverted
        from ores_marketdata_market_data_mappings_tbl m
        join ores_marketdata_market_data_vendor_instruments_tbl vi
          on vi.id = m.vendor_instrument_id
         and vi.valid_to = ores_utility_infinity_timestamp_fn()
        where m.tenant_id = v_tenant_id
          and m.valid_to = ores_utility_infinity_timestamp_fn()
          and vi.provider_code = any(v_providers)
          and m.oresmd_uri like '%type=fixing%'
          and (m.oresmd_uri like 'oresmd://fx/' || c.base_currency_code || '?%&ccy=' || c.quote_currency_code
               or m.oresmd_uri like 'oresmd://fx/' || c.quote_currency_code || '?%&ccy=' || c.base_currency_code)
    ) f on true
    where c.tenant_id = v_tenant_id
      and c.price_source = 'fixing'
      and not exists (
          select 1 from ores_dq_fx_spot_fixing_items_artefact_tbl x
          where x.dataset_id = c.dataset_id
            and x.base_currency_code = c.base_currency_code
            and x.quote_currency_code = c.quote_currency_code
      );

    get diagnostics v_count = row_count;
    raise debug 'Ranked % public fixing item(s) for the synthetic FX spot configs', v_count;
end $$;
