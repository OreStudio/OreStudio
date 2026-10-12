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
-- Whether the analytics of a configuration make a run
-- =============================================================================

-- The analytics of a configuration are rows, so a database check cannot count
-- them. This function states what a run needs, and the binding of a definition
-- and the publish of a configuration both ask it:
--   - at least one analytic is on, because an ORE run with none does nothing;
--   - each analytic that reads the output of another has that analytic on, as the
--     analytic type names it in requires_analytic_type_code.
-- It answers null when the analytics make a run, and the reason when they do not.
-- Security definer: the answer must not depend on what the session may see.
create or replace function ores_reporting_configuration_analytics_problem_fn(
    p_tenant_id        uuid,
    p_configuration_id uuid
)
returns text as $$
declare
    v_reason text;
begin
    if not exists (
        select 1 from ores_reporting_configuration_analytics_tbl a
        where a.tenant_id = p_tenant_id
          and a.configuration_id = p_configuration_id
          and a.active = 'Y'
          and a.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return 'The configuration switches no analytic on, and an ORE run needs one.';
    end if;

    select format('The %s analytic needs the %s analytic.',
                  a.analytic_type_code, t.requires_analytic_type_code)
    into v_reason
    from ores_reporting_configuration_analytics_tbl a
    join ores_reporting_analytic_types_tbl t
      on t.tenant_id = ores_utility_system_tenant_id_fn()
     and t.code = a.analytic_type_code
     and t.valid_to = ores_utility_infinity_timestamp_fn()
    where a.tenant_id = p_tenant_id
      and a.configuration_id = p_configuration_id
      and a.active = 'Y'
      and a.valid_to = ores_utility_infinity_timestamp_fn()
      and t.requires_analytic_type_code is not null
      and not exists (
          select 1 from ores_reporting_configuration_analytics_tbl b
          where b.tenant_id = p_tenant_id
            and b.configuration_id = p_configuration_id
            and b.analytic_type_code = t.requires_analytic_type_code
            and b.active = 'Y'
            and b.valid_to = ores_utility_infinity_timestamp_fn())
    order by a.display_order
    limit 1;

    return v_reason;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

comment on function ores_reporting_configuration_analytics_problem_fn(uuid, uuid) is
'Answers null when the analytics of a configuration make an ORE run, and the reason
 when they do not: none is on, or one reads the output of an analytic that is not.';
