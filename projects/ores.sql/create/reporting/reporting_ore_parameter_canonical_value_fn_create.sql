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
 * The form a configuration holds for one value of one set.
 *
 * ORE reads one fact several ways. NONE, INDIFF, NotApplicable and Unadjusted
 * are one business day convention, and a document may write any of them; the
 * store holds one. A value row names that form in =canonical_value= and is
 * empty when the row is the form, so this function answers with the row's own
 * canonical value, or with the value itself when the set states no other
 * rendering of it.
 *
 * The caller has already decided whether the value is in the set: a rendering
 * outside it is refused there, naming the set. This function states only what
 * is stored, so the two questions stay separate and a set that accepts a value
 * because it states no rule for it stores the value as it arrived.
 *
 * The sets are shared rows, so the answer does not depend on the tenant that
 * asks. A tenant that states its own row for one value is read after the
 * shared one, so the shared statement wins where both exist.
 */
create or replace function ores_reporting_canonical_value_fn(
    p_set_code text,
    p_value text
) returns text
security definer
set search_path = public, pg_temp
stable
as $$
declare
    v_canonical text;
begin
    if p_value is null or p_value = '' then
        return p_value;
    end if;

    select coalesce(v.canonical_value, v.value)
      into v_canonical
      from ores_reporting_ore_parameter_value_set_values_tbl v
     where v.value_set_code = p_set_code
       and v.value = p_value
       and v.valid_to = ores_utility_infinity_timestamp_fn()
     order by (v.tenant_id = ores_utility_system_tenant_id_fn()) desc,
              v.valid_from desc
     limit 1;

    return coalesce(v_canonical, p_value);
end;
$$ language plpgsql;
