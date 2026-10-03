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
-- Official reports read no sandbox
-- =============================================================================

-- Whether a risk report config belongs to an official report definition. A
-- config whose definition cannot be found is treated as official, the safe
-- side. Security definer: the answer must not depend on what the session may
-- see.
create or replace function ores_reporting_config_is_official_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid
)
returns boolean as $$
begin
    return coalesce((
        select d.is_official
        from ores_reporting_risk_report_configs_tbl c
        join ores_reporting_report_definitions_tbl d
          on d.tenant_id = c.tenant_id
         and d.id = c.report_definition_id
         and d.valid_to = ores_utility_infinity_timestamp_fn()
        where c.tenant_id = p_tenant_id
          and c.id = p_risk_report_config_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
        limit 1
    ), true);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a book is a virtual book. Security definer: row-level security hides
-- sandbox books from most sessions, and the check must see them to refuse them.
create or replace function ores_reporting_book_is_virtual_fn(
    p_tenant_id uuid,
    p_book_id   uuid
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = p_tenant_id
          and id = p_book_id
          and sandbox_id is not null
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a portfolio is a sandbox portfolio, for the same reason.
create or replace function ores_reporting_portfolio_is_sandbox_fn(
    p_tenant_id    uuid,
    p_portfolio_id uuid
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = p_tenant_id
          and id = p_portfolio_id
          and sandbox_id is not null
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a risk report config names a sandbox portfolio or a virtual book in
-- its scope.
create or replace function ores_reporting_config_scope_has_sandbox_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_reporting_risk_report_config_books_tbl b
        where b.tenant_id = p_tenant_id
          and b.risk_report_config_id = p_risk_report_config_id
          and b.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_book_is_virtual_fn(p_tenant_id, b.book_id)
    ) or exists (
        select 1 from ores_reporting_risk_report_config_portfolios_tbl p
        where p.tenant_id = p_tenant_id
          and p.risk_report_config_id = p_risk_report_config_id
          and p.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_portfolio_is_sandbox_fn(p_tenant_id, p.portfolio_id)
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a report definition is official. A definition that cannot be found
-- is treated as official, the safe side.
create or replace function ores_reporting_definition_is_official_fn(
    p_tenant_id            uuid,
    p_report_definition_id uuid
)
returns boolean as $$
begin
    return coalesce((
        select is_official from ores_reporting_report_definitions_tbl
        where tenant_id = p_tenant_id
          and id = p_report_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ), true);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether any risk report config of a definition names a sandbox portfolio or a
-- virtual book in its scope. A definition cannot become official while it does.
create or replace function ores_reporting_definition_scope_has_sandbox_fn(
    p_tenant_id            uuid,
    p_report_definition_id uuid
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_reporting_risk_report_configs_tbl c
        where c.tenant_id = p_tenant_id
          and c.report_definition_id = p_report_definition_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_config_scope_has_sandbox_fn(p_tenant_id, c.id)
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- =============================================================================
-- Resolve Book IDs for a Risk Report Config
-- =============================================================================

/**
 * Given a risk_report_config_id, returns the set of book UUIDs in scope
 * for that config.
 *
 * Resolution order:
 *   1. If the config has explicit book entries in the books junction table,
 *      return those book IDs directly.
 *   2. Otherwise, if the config has portfolio entries in the portfolios
 *      junction table, resolve each portfolio to its books (including
 *      the full portfolio subtree) via ores_trading_get_book_ids_by_portfolio_fn.
 *   3. Otherwise (no book scope and no portfolio scope), return all active
 *      books visible to the tenant.
 *
 * Only active (valid_to = infinity) junction rows and book rows are considered.
 */
create or replace function ores_reporting_resolve_book_ids_for_config_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid
)
returns setof uuid as $$
declare
    v_has_books      boolean;
    v_has_portfolios boolean;
    v_official       boolean := ores_reporting_config_is_official_fn(
        p_tenant_id, p_risk_report_config_id);
begin
    -- Check if explicit book scope exists.
    select exists(
        select 1
        from ores_reporting_risk_report_config_books_tbl b
        where b.tenant_id = p_tenant_id
          and b.risk_report_config_id = p_risk_report_config_id
          and b.valid_to = ores_utility_infinity_timestamp_fn()
    ) into v_has_books;

    if v_has_books then
        return query
            select b.book_id
            from ores_reporting_risk_report_config_books_tbl b
            where b.tenant_id = p_tenant_id
              and b.risk_report_config_id = p_risk_report_config_id
              and b.valid_to = ores_utility_infinity_timestamp_fn()
              and not (v_official
                       and ores_reporting_book_is_virtual_fn(p_tenant_id, b.book_id));
        return;
    end if;

    -- Check if portfolio scope exists.
    select exists(
        select 1
        from ores_reporting_risk_report_config_portfolios_tbl p
        where p.tenant_id = p_tenant_id
          and p.risk_report_config_id = p_risk_report_config_id
          and p.valid_to = ores_utility_infinity_timestamp_fn()
    ) into v_has_portfolios;

    if v_has_portfolios then
        return query
            select distinct bk.id
            from ores_reporting_risk_report_config_portfolios_tbl p
            cross join lateral ores_trading_get_book_ids_by_portfolio_fn(
                p_tenant_id, p.portfolio_id) as bk(id)
            where p.tenant_id = p_tenant_id
              and p.risk_report_config_id = p_risk_report_config_id
              and p.valid_to = ores_utility_infinity_timestamp_fn()
              and not (v_official
                       and (ores_reporting_portfolio_is_sandbox_fn(p_tenant_id, p.portfolio_id)
                            or ores_reporting_book_is_virtual_fn(p_tenant_id, bk.id)));
        return;
    end if;

    -- No scope configured — return empty set.
    -- The application layer treats this as a configuration error.
    return;
end;
$$ language plpgsql stable security definer;

comment on function ores_reporting_resolve_book_ids_for_config_fn(uuid, uuid) is
'Resolves the set of book UUIDs in scope for a risk_report_config. Checks
 explicit book scope first, then portfolio scope (with subtree expansion).
 Returns an empty set when neither scope is configured.';
