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
 * pgTAP tests for the ORE counterparty aliases and the ORE sample banks.
 *
 * Tests cover:
 * - Every counterparty name the ORE samples use resolves to one counterparty
 * - A counterparty may answer to several ORE names
 * - An ORE name answers to one counterparty per tenant
 * - The LEI counterparty publish adds what a tenant lacks and skips the rest
 *
 * Run with: pg_prove -d <database> test/refdata_counterparty_ore_aliases_test.sql
 */

begin;

select plan(6);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create or replace function pg_temp.alias_target(p_alias text)
returns text as $$
    select ci.counterparty_id::text
    from ores_refdata_counterparty_identifiers_tbl ci
    where ci.tenant_id = ores_utility_system_tenant_id_fn()
      and ci.id_scheme = 'ORE'
      and ci.id_value = p_alias
      and ci.valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql;

create or replace function pg_temp.add_alias(p_counterparty uuid, p_alias text)
returns void as $$
    insert into ores_refdata_counterparty_identifiers_tbl (tenant_id, id, version,
        counterparty_id, id_scheme, id_value, modified_by, performed_by,
        change_reason_code, change_commentary)
    values (ores_utility_system_tenant_id_fn(), gen_random_uuid(), 0, p_counterparty,
        'ORE', p_alias, current_user, current_user, 'system.new_record', 'test');
$$ language sql;

select is(
    (select count(*)::int from (values ('CPTY_A'), ('CPTY_B'), ('CPTY'), ('CPTY_C'), ('CP'),
        ('DUMMY_CP'), ('EquityOption1'), ('EquityOption2'), ('CPTY_D'), ('ABC'), ('A'),
        ('001B456BCDEFGH67XY89'), ('DUMMY_CPTY'), ('CPTY_1'), ('CPTY_2'), ('CPTY_3'),
        ('CPTY_4'), ('CPTY_5'), ('CPTY_6'), ('CPTY_7'), ('CPTY_8'), ('CPTY_9'), ('CPTY_10'))
        as n(alias)
     where pg_temp.alias_target(n.alias) is not null),
    23,
    'every counterparty name the ORE samples use resolves');

select is(pg_temp.alias_target('CPTY_A'), pg_temp.alias_target('A'),
    'a counterparty may answer to several ORE names');

select is(
    (select count(distinct pg_temp.alias_target(n.alias))::int
     from (values ('CPTY_1'), ('CPTY_2'), ('CPTY_3'), ('CPTY_4'), ('CPTY_5'), ('CPTY_6'),
                  ('CPTY_7'), ('CPTY_8'), ('CPTY_9'), ('CPTY_10')) as n(alias)),
    10,
    'the ten names one example uses together map onto ten counterparties');

select throws_ok(
    $$select pg_temp.add_alias(pg_temp.alias_target('CPTY_B')::uuid, 'CPTY_A')$$,
    '23505', null,
    'an ORE name answers to one counterparty per tenant');

select lives_ok(
    $$select pg_temp.add_alias(pg_temp.alias_target('CPTY_B')::uuid, 'CPTY_B_EXTRA')$$,
    'a counterparty takes another ORE name');

select results_eq(
    $$select action from ores_refdata_publish_lei_counterparties_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'ore.sample_counterparties'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    array['skipped'],
    'a second publish of the same banks adds nothing');

select * from finish();

rollback;
