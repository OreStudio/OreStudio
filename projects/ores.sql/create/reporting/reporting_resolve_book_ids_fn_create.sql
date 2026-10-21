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

-- Whether a risk report config is read by an official report definition. A
-- config is official when any definition that binds it is official, and also
-- when no definition binds it yet, the safe side. Security definer: the answer
-- must not depend on what the session may see.
create or replace function ores_reporting_config_is_official_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid
)
returns boolean as $$
begin
    return coalesce((
        select bool_or(d.is_official)
        from ores_reporting_risk_report_configs_tbl c
        join ores_reporting_report_configurations_tbl b
          on b.tenant_id = c.tenant_id
         and b.configuration_id = c.configuration_id
         and b.configuration_type_code = 'risk_report_config'
         and b.valid_to = ores_utility_infinity_timestamp_fn()
        join ores_reporting_report_definitions_tbl d
          on d.tenant_id = b.tenant_id
         and d.id = b.report_definition_id
         and d.valid_to = ores_utility_infinity_timestamp_fn()
        where c.tenant_id = p_tenant_id
          and c.id = p_risk_report_config_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
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

-- Whether the risk report config that a definition binds names a sandbox
-- portfolio or a virtual book in its scope. A definition cannot become official
-- while it does.
create or replace function ores_reporting_definition_scope_has_sandbox_fn(
    p_tenant_id            uuid,
    p_report_definition_id uuid
)
returns boolean as $$
begin
    return exists (
        select 1
        from ores_reporting_report_configurations_tbl b
        join ores_reporting_risk_report_configs_tbl c
          on c.tenant_id = b.tenant_id
         and c.configuration_id = b.configuration_id
         and c.valid_to = ores_utility_infinity_timestamp_fn()
        where b.tenant_id = p_tenant_id
          and b.report_definition_id = p_report_definition_id
          and b.configuration_type_code = 'risk_report_config'
          and b.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_config_scope_has_sandbox_fn(p_tenant_id, c.id)
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- =============================================================================
-- A typed report reads the books of its regulatory book type
-- =============================================================================

-- The regulatory book type of a book, null when the book is not found. Security
-- definer for the same reason as the checks above.
create or replace function ores_reporting_book_regulatory_type_fn(
    p_tenant_id uuid,
    p_book_id   uuid
)
returns text as $$
begin
    return (
        select regulatory_book_type from ores_refdata_books_tbl
        where tenant_id = p_tenant_id
          and id = p_book_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- The regulatory book type a definition is limited to, null when the definition
-- reads any book or cannot be found.
create or replace function ores_reporting_definition_regulatory_type_fn(
    p_tenant_id            uuid,
    p_report_definition_id uuid
)
returns text as $$
begin
    return (
        select regulatory_book_type from ores_reporting_report_definitions_tbl
        where tenant_id = p_tenant_id
          and id = p_report_definition_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether the scope of a risk report config names, as an explicit book, a book of
-- a regulatory book type other than p_type. A portfolio in the scope is not
-- checked: the resolver reads the books of the type below it.
create or replace function ores_reporting_config_scope_names_other_type_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid,
    p_regulatory_book_type   text
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_reporting_risk_report_config_books_tbl b
        where b.tenant_id = p_tenant_id
          and b.risk_report_config_id = p_risk_report_config_id
          and b.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_book_regulatory_type_fn(p_tenant_id, b.book_id)
              is distinct from p_regulatory_book_type
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a risk report config that a definition binds names, as an explicit
-- book, a book of a regulatory book type other than p_type. A definition cannot
-- be typed while it does.
create or replace function ores_reporting_definition_scope_names_other_type_fn(
    p_tenant_id            uuid,
    p_report_definition_id uuid,
    p_regulatory_book_type text
)
returns boolean as $$
begin
    return exists (
        select 1
        from ores_reporting_report_configurations_tbl b
        join ores_reporting_risk_report_configs_tbl c
          on c.tenant_id = b.tenant_id
         and c.configuration_id = b.configuration_id
         and c.valid_to = ores_utility_infinity_timestamp_fn()
        where b.tenant_id = p_tenant_id
          and b.report_definition_id = p_report_definition_id
          and b.configuration_type_code = 'risk_report_config'
          and b.valid_to = ores_utility_infinity_timestamp_fn()
          and ores_reporting_config_scope_names_other_type_fn(
                  p_tenant_id, c.id, p_regulatory_book_type)
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a definition that is typed differently from a book binds a risk report
-- config. A book cannot join the scope of such a config.
create or replace function ores_reporting_config_refuses_book_fn(
    p_tenant_id             uuid,
    p_risk_report_config_id uuid,
    p_book_id               uuid
)
returns boolean as $$
begin
    return exists (
        select 1
        from ores_reporting_risk_report_configs_tbl c
        join ores_reporting_report_configurations_tbl b
          on b.tenant_id = c.tenant_id
         and b.configuration_id = c.configuration_id
         and b.configuration_type_code = 'risk_report_config'
         and b.valid_to = ores_utility_infinity_timestamp_fn()
        join ores_reporting_report_definitions_tbl d
          on d.tenant_id = b.tenant_id
         and d.id = b.report_definition_id
         and d.valid_to = ores_utility_infinity_timestamp_fn()
        where c.tenant_id = p_tenant_id
          and c.id = p_risk_report_config_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
          and d.regulatory_book_type is not null
          and d.regulatory_book_type
              is distinct from ores_reporting_book_regulatory_type_fn(p_tenant_id, p_book_id)
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
 *
 * A report definition may be named. When the definition is typed, only the books
 * of its regulatory book type are returned, whichever way the scope names them.
 * The configuration is shared by every definition that binds it, so the type is
 * the definition's and not the configuration's.
 */
drop function if exists ores_reporting_resolve_book_ids_for_config_fn(uuid, uuid);

create or replace function ores_reporting_resolve_book_ids_for_config_fn(
    p_tenant_id              uuid,
    p_risk_report_config_id  uuid,
    p_report_definition_id   uuid default null
)
returns setof uuid as $$
declare
    v_has_books      boolean;
    v_has_portfolios boolean;
    v_official       boolean := ores_reporting_config_is_official_fn(
        p_tenant_id, p_risk_report_config_id);
    v_type           text := ores_reporting_definition_regulatory_type_fn(
        p_tenant_id, p_report_definition_id);
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
                       and ores_reporting_book_is_virtual_fn(p_tenant_id, b.book_id))
              and (v_type is null
                   or ores_reporting_book_regulatory_type_fn(p_tenant_id, b.book_id) = v_type);
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
                            or ores_reporting_book_is_virtual_fn(p_tenant_id, bk.id)))
              and (v_type is null
                   or ores_reporting_book_regulatory_type_fn(p_tenant_id, bk.id) = v_type);
        return;
    end if;

    -- No scope configured — return empty set.
    -- The application layer treats this as a configuration error.
    return;
end;
$$ language plpgsql stable security definer;

comment on function ores_reporting_resolve_book_ids_for_config_fn(uuid, uuid, uuid) is
'Resolves the set of book UUIDs in scope for a risk_report_config. Checks
 explicit book scope first, then portfolio scope (with subtree expansion).
 When a typed report definition is named, only the books of its regulatory book
 type are returned. Returns an empty set when neither scope is configured or no
 book of the type is left.';
