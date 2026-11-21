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

-- =============================================================================
-- Hand-written tenor rule for ir_curve_generation_config
-- =============================================================================
-- A curve config names an index family and a tenor, and the family's row in
-- ores_synthetic_ir_curve_index_families_tbl says which tenors it admits: no
-- tenor for an overnight curve, any tenor for a term curve, and its one dated
-- tenor, such as SOFR's FOMC, for a meeting-dated curve. The codegen
-- single-argument Validations mechanism cannot express a two-column rule, so
-- this trigger is written by hand, as the process type cross-check of the
-- parameter values is; the generated insert trigger validates the family
-- alone. Fires on insert and on update of either column.
--
-- The families are system-tenant reference data, so the lookup resolves
-- there, as the family's own validation function does.

create or replace function ores_synthetic_validate_ir_curve_index_family_tenor_fn()
returns trigger as $$
declare
    v_family ores_synthetic_ir_curve_index_families_tbl%rowtype;
    v_tenor text := coalesce(NEW.tenor, '');
begin
    select * into v_family
    from ores_synthetic_ir_curve_index_families_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = NEW.index_family
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- The family validation passes everything through while the catalogue is
    -- still empty, during bootstrap; so does the tenor rule.
    if not found then
        return NEW;
    end if;

    if v_tenor = '' then
        if not v_family.admits_overnight then
            raise exception 'Invalid tenor for index family %: the family has no overnight curve, so a curve of it names a tenor.',
                NEW.index_family using errcode = '23514';
        end if;
    elsif v_tenor <> v_family.dated_tenor and not v_family.admits_term then
        raise exception 'Invalid tenor % for index family %: the family has no term curve%.',
            v_tenor, NEW.index_family,
            case when v_family.dated_tenor <> ''
                 then ', and its one dated tenor is ' || v_family.dated_tenor
                 else '' end
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_ir_curve_configs_family_tenor_trg
before insert or update of index_family, tenor on "ores_synthetic_ir_curve_generation_configs_tbl"
for each row execute function ores_synthetic_validate_ir_curve_index_family_tenor_fn();
