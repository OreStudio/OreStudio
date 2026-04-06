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
begin
    -- Check if explicit book scope exists.
    select exists(
        select 1
        from ores_reporting_risk_report_config_books_tbl b
        where b.tenant_id = p_tenant_id
          and b.risk_report_config_id = p_risk_report_config_id
          and b.valid_to = 'infinity'
    ) into v_has_books;

    if v_has_books then
        return query
            select b.book_id
            from ores_reporting_risk_report_config_books_tbl b
            where b.tenant_id = p_tenant_id
              and b.risk_report_config_id = p_risk_report_config_id
              and b.valid_to = 'infinity';
        return;
    end if;

    -- Check if portfolio scope exists.
    select exists(
        select 1
        from ores_reporting_risk_report_config_portfolios_tbl p
        where p.tenant_id = p_tenant_id
          and p.risk_report_config_id = p_risk_report_config_id
          and p.valid_to = 'infinity'
    ) into v_has_portfolios;

    if v_has_portfolios then
        return query
            select distinct bk.id
            from ores_reporting_risk_report_config_portfolios_tbl p
            cross join lateral ores_trading_get_book_ids_by_portfolio_fn(
                p_tenant_id, p.portfolio_id) as bk(id)
            where p.tenant_id = p_tenant_id
              and p.risk_report_config_id = p_risk_report_config_id
              and p.valid_to = 'infinity';
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
