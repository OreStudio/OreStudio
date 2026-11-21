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
 * The tenor rule of an IR curve's index family.
 *
 * A curve config names a family and a tenor, and the family's catalogue row says
 * which tenors it admits: no tenor for an overnight curve, any tenor for a term
 * curve, and the one dated tenor, such as SOFR's FOMC, for a meeting-dated
 * curve. The insert trigger of ores_synthetic_ir_curve_generation_configs_tbl
 * calls this with both columns, so a new family is a catalogue row and not an
 * edit to a check.
 *
 * Returns the family, as every insert-trigger validation returns its column.
 */
create or replace function ores_synthetic_validate_ir_curve_index_family_tenor_fn(
    p_tenant_id uuid,
    p_family text,
    p_tenor text
) returns text as $$
declare
    v_family ores_synthetic_ir_curve_index_families_tbl%rowtype;
    v_tenor text := coalesce(p_tenor, '');
begin
    p_family := ores_synthetic_validate_ir_curve_index_family_fn(p_tenant_id, p_family);

    select * into v_family
    from ores_synthetic_ir_curve_index_families_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = p_family
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- The family validation passes everything through while the catalogue is
    -- still empty, during bootstrap; so does the tenor rule.
    if not found then
        return p_family;
    end if;

    if v_tenor = '' then
        if not v_family.admits_overnight then
            raise exception 'Invalid tenor for index family %: the family has no overnight curve, so a curve of it names a tenor.',
                p_family using errcode = '23514';
        end if;
    elsif v_tenor = v_family.dated_tenor then
        return p_family;
    elsif not v_family.admits_term then
        raise exception 'Invalid tenor % for index family %: the family has no term curve%.',
            v_tenor, p_family,
            case when v_family.dated_tenor <> ''
                 then ', and its one dated tenor is ' || v_family.dated_tenor
                 else '' end
            using errcode = '23514';
    end if;

    return p_family;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
