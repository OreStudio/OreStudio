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
 * Convention reference validation
 *
 * A curve segment names its convention by ORE's id, and the id may belong to
 * any of ORE's convention kinds, which refdata holds as one table each. This
 * function checks the id against all of them, so a reference to a convention
 * the tenant does not hold is refused on insert.
 *
 * An FX convention is held per currency pair, and its id is not stored: the
 * conventions mapper writes it back as <base>-<quote>-FX-CONVENTIONS. That is
 * the only FX id this function can resolve. Keeping each FX convention's own
 * id is tracked as its own task.
 */

create or replace function ores_refdata_validate_convention_id_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    if p_value is null or p_value = '' then
        raise exception 'A convention id is required.' using errcode = '23502';
    end if;

    if exists (
        select 1 from ores_refdata_average_ois_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_bma_basis_swap_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_bond_yield_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_cds_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_cms_spread_option_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_commodity_forward_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_commodity_future_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_cross_currency_basis_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_cross_currency_fix_float_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_deposit_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_fra_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_future_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_fx_option_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_ibor_index_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_inflation_swap_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_intraday_power_load_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_ois_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_overnight_index_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_swap_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_swap_index_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_tenor_basis_swap_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_tenor_basis_two_swap_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_zero_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select 1 from ores_refdata_zero_inflation_index_conventions_tbl
        where tenant_id = p_tenant_id and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    if exists (
        select 1 from ores_refdata_currency_pairs_tbl
        where tenant_id = p_tenant_id
          and base_currency || '-' || quote_currency || '-FX-CONVENTIONS' = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    raise exception 'Invalid conventions: %. No active convention of any kind has this id.', p_value
        using errcode = '23503';
end;
$$ language plpgsql;
