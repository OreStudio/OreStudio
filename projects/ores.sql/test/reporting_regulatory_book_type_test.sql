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
 * pgTAP tests for a report that reads the books of one regulatory book type.
 *
 * Tests cover:
 * - A definition with no type reads the books of both types
 * - A typed definition reads the books of its type only, through a portfolio
 * - A definition refuses a code that is not a regulatory book type
 * - A typed scope refuses an explicit book of another type, and accepts one of its type
 * - A definition cannot take a type while its explicit scope names a book of another type
 * - A typed definition cannot bind a configuration that scopes a book of another type
 * - A scope with no book of the type resolves no book
 * - A portfolio that holds only books of another type resolves no book
 * - A definition that is named and cannot be found is refused
 * - Omitting the definition reads the scope with no type filter
 *
 * Run with: pg_prove -d <database> test/reporting_regulatory_book_type_test.sql
 */

begin;

select plan(16);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

-- =============================================================================
-- Setup: a portfolio of one trading book and one banking book, and a report
-- =============================================================================

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, sandbox_id, status, modified_by, performed_by,
    change_reason_code, change_commentary
)
select v.id, ores_utility_system_tenant_id_fn(), 0, p.id, v.name, null, 'Risk', false, null,
    'Active', current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl p,
     (values ('00000000-0000-0000-0000-0000000cb001'::uuid, 'RBT-FIXTURE-MIXED'),
             ('00000000-0000-0000-0000-0000000cb002'::uuid, 'RBT-FIXTURE-BANKING-ONLY'))
     v(id, name)
where p.tenant_id = ores_utility_system_tenant_id_fn() and p.short_code = 'system_party'
  and p.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_refdata_books_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, functional_currency,
    book_status, regulatory_book_type, is_sweepable, rates_centre_code, sandbox_id,
    book_purpose_type, ledger_feed_type,
    modified_by, performed_by, change_reason_code, change_commentary
)
select v.id, ores_utility_system_tenant_id_fn(), 0, p.party_id, v.name, v.parent,
    'USD', 'Active', v.type, false, 'WRLD', null, 'Test', 'None',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_portfolios_tbl p,
     (values ('00000000-0000-0000-0000-0000000cb011'::uuid, 'RBT-TRADING',
              '00000000-0000-0000-0000-0000000cb001'::uuid, 'Trading'),
             ('00000000-0000-0000-0000-0000000cb012'::uuid, 'RBT-BANKING',
              '00000000-0000-0000-0000-0000000cb001'::uuid, 'Banking'),
             ('00000000-0000-0000-0000-0000000cb013'::uuid, 'RBT-BANKING-ONLY',
              '00000000-0000-0000-0000-0000000cb002'::uuid, 'Banking'))
     v(id, name, parent, type)
where p.id = '00000000-0000-0000-0000-0000000cb001'::uuid
  and p.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_report_definitions_tbl (
    id, tenant_id, version, name, party_id, description, report_type,
    schedule_expression, concurrency_policy, pre_processing, post_processing,
    is_official, modified_by, performed_by, change_reason_code, change_commentary
)
select v.id, ores_utility_system_tenant_id_fn(), 0, v.name, p.id,
    'Regulatory book type pgTAP fixture', 'risk', '0 0 * * *', 'skip', '', '',
    true, current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl p,
     (values ('00000000-0000-0000-0000-0000000cb301'::uuid, 'RBT-FIXTURE-DEFINITION'),
             ('00000000-0000-0000-0000-0000000cb305'::uuid, 'RBT-FIXTURE-OTHER'))
     v(id, name)
where p.tenant_id = ores_utility_system_tenant_id_fn() and p.short_code = 'system_party'
  and p.valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_configurations_tbl (
    tenant_id, id, version, party_id, name, configuration_type_code, owning_component,
    modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb303'::uuid, 0,
    id, 'RBT-FIXTURE-SETTINGS', 'risk_report_config', 'ores.reporting',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_risk_report_configs_tbl (
    id, tenant_id, version, party_id, name, configuration_id, base_currency,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cb302'::uuid, ores_utility_system_tenant_id_fn(), 0,
    id, 'RBT-FIXTURE-SETTINGS', '00000000-0000-0000-0000-0000000cb303'::uuid, 'USD',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_configuration_analytics_tbl (
    tenant_id, id, version, party_id, configuration_id, analytic_type_code, display_order,
    active, modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb306'::uuid, 0,
    id, '00000000-0000-0000-0000-0000000cb303'::uuid, 'npv', 1, 'Y',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_report_configurations_tbl (
    tenant_id, id, version, party_id, report_definition_id, configuration_type_code,
    configuration_id, modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb304'::uuid, 0,
    id, '00000000-0000-0000-0000-0000000cb301'::uuid, 'risk_report_config',
    '00000000-0000-0000-0000-0000000cb303'::uuid,
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

-- A second configuration, bound to the second definition, holds an explicit book scope.
insert into ores_reporting_configurations_tbl (
    tenant_id, id, version, party_id, name, configuration_type_code, owning_component,
    modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb403'::uuid, 0,
    id, 'RBT-FIXTURE-SETTINGS-2', 'risk_report_config', 'ores.reporting',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_risk_report_configs_tbl (
    id, tenant_id, version, party_id, name, configuration_id, base_currency,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cb402'::uuid, ores_utility_system_tenant_id_fn(), 0,
    id, 'RBT-FIXTURE-SETTINGS-2', '00000000-0000-0000-0000-0000000cb403'::uuid, 'USD',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_configuration_analytics_tbl (
    tenant_id, id, version, party_id, configuration_id, analytic_type_code, display_order,
    active, modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb406'::uuid, 0,
    id, '00000000-0000-0000-0000-0000000cb403'::uuid, 'npv', 1, 'Y',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_report_configurations_tbl (
    tenant_id, id, version, party_id, report_definition_id, configuration_type_code,
    configuration_id, modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb404'::uuid, 0,
    id, '00000000-0000-0000-0000-0000000cb305'::uuid, 'risk_report_config',
    '00000000-0000-0000-0000-0000000cb403'::uuid,
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

create temp table t_ctx on commit drop as
select (select username from ores_iam_accounts_tbl where account_type = 'service'
        and valid_to = ores_utility_infinity_timestamp_fn() order by username limit 1) as owner_name;

select set_config('app.current_actor', (select owner_name from t_ctx), true);

create or replace function pg_temp.set_type(p_definition uuid, p_type text)
returns void as $$
declare
    r ores_reporting_report_definitions_tbl;
begin
    select * into r from ores_reporting_report_definitions_tbl
    where id = p_definition and valid_to = ores_utility_infinity_timestamp_fn();
    r.regulatory_book_type := p_type;
    insert into ores_reporting_report_definitions_tbl select (r).*;
end;
$$ language plpgsql;

create or replace function pg_temp.scope_book(p_config uuid, p_book uuid)
returns void as $$
    insert into ores_reporting_risk_report_config_books_tbl (tenant_id,
        risk_report_config_id, book_id, valid_from, valid_to)
    values (ores_utility_system_tenant_id_fn(), p_config, p_book, now(),
            ores_utility_infinity_timestamp_fn());
$$ language sql;

create or replace function pg_temp.scope_portfolio(p_config uuid, p_portfolio uuid)
returns void as $$
    insert into ores_reporting_risk_report_config_portfolios_tbl (tenant_id,
        risk_report_config_id, portfolio_id, valid_from, valid_to)
    values (ores_utility_system_tenant_id_fn(), p_config, p_portfolio, now(),
            ores_utility_infinity_timestamp_fn());
$$ language sql;

create or replace function pg_temp.resolved(p_config uuid, p_definition uuid)
returns setof uuid as $$
    select ores_reporting_resolve_book_ids_for_config_fn(
        ores_utility_system_tenant_id_fn(), p_config, p_definition);
$$ language sql;

-- =============================================================================
-- A portfolio scope
-- =============================================================================

select lives_ok($$select pg_temp.scope_portfolio('00000000-0000-0000-0000-0000000cb302', '00000000-0000-0000-0000-0000000cb001')$$,
    'a portfolio is scoped');

select is((select count(*) from pg_temp.resolved('00000000-0000-0000-0000-0000000cb302', '00000000-0000-0000-0000-0000000cb301')), 2::bigint,
    'a definition with no type reads the books of both types');

select lives_ok($$select pg_temp.set_type('00000000-0000-0000-0000-0000000cb301', 'Trading')$$,
    'a definition takes the Trading type');
select results_eq($$select * from pg_temp.resolved('00000000-0000-0000-0000-0000000cb302', '00000000-0000-0000-0000-0000000cb301')$$,
    $$values ('00000000-0000-0000-0000-0000000cb011'::uuid)$$,
    'a Trading report reads the trading book of the portfolio and not the banking book');

select lives_ok($$select pg_temp.set_type('00000000-0000-0000-0000-0000000cb301', 'Banking')$$,
    'a definition takes the Banking type');
select results_eq($$select * from pg_temp.resolved('00000000-0000-0000-0000-0000000cb302', '00000000-0000-0000-0000-0000000cb301')$$,
    $$values ('00000000-0000-0000-0000-0000000cb012'::uuid)$$,
    'a Banking report reads the banking book of the portfolio and not the trading book');

select throws_ok($$select pg_temp.set_type('00000000-0000-0000-0000-0000000cb301', 'Nonsense')$$,
    '23503', null, 'a definition refuses a code that is not a regulatory book type');

-- =============================================================================
-- An explicit book scope, on the second configuration, which the second definition binds
-- =============================================================================

select pg_temp.set_type('00000000-0000-0000-0000-0000000cb305', 'Banking');

select throws_ok($$select pg_temp.scope_book('00000000-0000-0000-0000-0000000cb402', '00000000-0000-0000-0000-0000000cb011')$$,
    '23514', null, 'a Banking report refuses an explicit trading book');
select lives_ok($$select pg_temp.scope_book('00000000-0000-0000-0000-0000000cb402', '00000000-0000-0000-0000-0000000cb012')$$,
    'a Banking report accepts an explicit banking book');

select throws_ok($$select pg_temp.set_type('00000000-0000-0000-0000-0000000cb305', 'Trading')$$,
    '23514', null,
    'a definition cannot take a type while its explicit scope names a book of another type');

select pg_temp.set_type('00000000-0000-0000-0000-0000000cb301', 'Trading');

select throws_ok($$insert into ores_reporting_report_configurations_tbl (
                       tenant_id, id, version, party_id, report_definition_id,
                       configuration_type_code, configuration_id,
                       modified_by, performed_by, change_reason_code, change_commentary)
                   select ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, party_id,
                       id, 'risk_report_config', '00000000-0000-0000-0000-0000000cb403'::uuid,
                       current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
                   from ores_reporting_report_definitions_tbl
                   where id = '00000000-0000-0000-0000-0000000cb301'::uuid
                     and valid_to = ores_utility_infinity_timestamp_fn()$$,
    '23514', null,
    'a Trading definition cannot bind a configuration that scopes a banking book');

-- =============================================================================
-- A scope with no book of the type
-- =============================================================================

-- The scope of the second configuration holds a banking book only, and the
-- first definition is Trading.
select is((select count(*) from pg_temp.resolved('00000000-0000-0000-0000-0000000cb402', '00000000-0000-0000-0000-0000000cb301')), 0::bigint,
    'a Trading report scoped to a banking book resolves no book');

-- A portfolio that holds only banking books, scoped on a configuration of its own.
insert into ores_reporting_configurations_tbl (
    tenant_id, id, version, party_id, name, configuration_type_code, owning_component,
    modified_by, performed_by, change_reason_code, change_commentary
)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000cb503'::uuid, 0,
    id, 'RBT-FIXTURE-SETTINGS-3', 'risk_report_config', 'ores.reporting',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_reporting_risk_report_configs_tbl (
    id, tenant_id, version, party_id, name, configuration_id, base_currency,
    modified_by, performed_by, change_reason_code, change_commentary
)
select '00000000-0000-0000-0000-0000000cb502'::uuid, ores_utility_system_tenant_id_fn(), 0,
    id, 'RBT-FIXTURE-SETTINGS-3', '00000000-0000-0000-0000-0000000cb503'::uuid, 'USD',
    current_user, current_user, 'system.test', 'Regulatory book type pgTAP fixture'
from ores_refdata_parties_tbl
where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
  and valid_to = ores_utility_infinity_timestamp_fn();

select lives_ok($$select pg_temp.scope_portfolio('00000000-0000-0000-0000-0000000cb502', '00000000-0000-0000-0000-0000000cb002')$$,
    'a portfolio of banking books only is scoped');

select is((select count(*) from pg_temp.resolved('00000000-0000-0000-0000-0000000cb502', '00000000-0000-0000-0000-0000000cb301')), 0::bigint,
    'a Trading report scoped to a portfolio of banking books only resolves no book');

-- =============================================================================
-- A definition that cannot be found, and no definition
-- =============================================================================

select throws_ok($$select * from pg_temp.resolved('00000000-0000-0000-0000-0000000cb302', '00000000-0000-0000-0000-0000000cb999')$$,
    'P0002', null, 'a definition that is named and cannot be found is refused');

select is((select count(*) from pg_temp.resolved('00000000-0000-0000-0000-0000000cb302', null)), 2::bigint,
    'omitting the definition reads the books of both types');

select * from finish();

rollback;
