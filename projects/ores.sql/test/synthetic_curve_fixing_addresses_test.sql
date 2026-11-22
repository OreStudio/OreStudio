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

/*
 * One fixing address per index. A synthetic curve states the fixings of its own
 * index at the address its currency, family and tenor give; a trade names the
 * index through its convention, and a public publisher through its mapping. If
 * the curve's address is one no convention and no mapping names, the curve is
 * writing a second spelling of the index, and its fixings are found by nothing.
 * The address is built as index_fixing_uri builds it in ores.synthetic; the
 * meeting-dated FOMC variant states no fixings.
 */

begin;

select plan(1);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is_empty(
    $$with curve_addresses as (
          select distinct
              c.currency_code || '-' || c.index_family || coalesce('-' || nullif(c.tenor, ''), '') as curve,
              'oresmd://ir/' || c.currency_code || '?type=fixing&index=ibor&name=' || upper(c.index_family)
                  || coalesce('&tenor=' || nullif(c.tenor, ''), '') as uri
          from ores_synthetic_ir_curve_generation_configs_tbl c
          where c.valid_to = ores_utility_infinity_timestamp_fn()
            and coalesce(c.tenor, '') <> 'FOMC'
      ),
      named as (
          select a.oresmd_uri as uri
          from ores_dq_overnight_index_conventions_artefact_tbl a
          join ores_dq_datasets_tbl d on d.id = a.dataset_id and d.code = 'refdata.conventions'
           and d.valid_to = ores_utility_infinity_timestamp_fn()
          union select a.oresmd_uri
          from ores_dq_ibor_index_conventions_artefact_tbl a
          join ores_dq_datasets_tbl d on d.id = a.dataset_id and d.code = 'refdata.conventions'
           and d.valid_to = ores_utility_infinity_timestamp_fn()
          union select a.oresmd_uri
          from ores_dq_ois_conventions_artefact_tbl a
          join ores_dq_datasets_tbl d on d.id = a.dataset_id and d.code = 'refdata.conventions'
           and d.valid_to = ores_utility_infinity_timestamp_fn()
          union select a.oresmd_uri
          from ores_dq_swap_conventions_artefact_tbl a
          join ores_dq_datasets_tbl d on d.id = a.dataset_id and d.code = 'refdata.conventions'
           and d.valid_to = ores_utility_infinity_timestamp_fn()
          union select m.oresmd_uri
          from ores_marketdata_market_data_mappings_tbl m
          where m.valid_to = ores_utility_infinity_timestamp_fn()
      )
      select curve || ' ' || uri
      from curve_addresses
      where uri not in (select uri from named where uri is not null)
      order by 1$$,
    'every synthetic curve states its fixings at an address a convention or a mapping names'
);

select * from finish();

rollback;
