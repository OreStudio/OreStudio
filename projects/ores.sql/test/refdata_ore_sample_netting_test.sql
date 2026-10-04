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
 * pgTAP tests for the ORE sample netting.
 *
 * Tests cover:
 * - Every netting set id the ORE sample trades use resolves to a set of the
 *   system tenant's root party
 * - Each set's counterparty is the one the sample trades name with that id
 * - The CSAs carry ORE's terms and their eligible currencies
 * - A second publish writes nothing
 * - A publish with no party, and a set whose agreement is unknown, are skipped
 *
 * Run with: pg_prove -d <database> test/refdata_ore_sample_netting_test.sql
 */

begin;

select plan(10);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

-- The netting set id and counterparty pairs of the ORE sample trades.
create temp table t_ore_pairs (netting_set_id text, counterparty text) on commit drop;
insert into t_ore_pairs values
    ('CPTY_A', 'CPTY_A'), ('NS', 'CPTY'), ('NS', 'CP'), ('CPTY_B', 'CPTY_B'),
    ('CPTY', 'CPTY'), ('CPTY_C', 'CPTY_C'), ('PricerStaticDate', 'CPTY_A'),
    ('DUMMY_NS', 'DUMMY_CP'), ('EquityOption1', 'EquityOption1'),
    ('EquityOption2', 'EquityOption2'), ('CRIF_20191230', 'CPTY_A'), ('1234', 'ABC'),
    ('CPTY_A_tradeTypeWrapper', 'CPTY_A'), ('CS', 'CPTY'), ('CPTY_D', 'CPTY_D'),
    ('CPTY_A_full', 'CPTY_A'), ('CPTY_B_full', 'CPTY_B'),
    ('ABC1234', '001B456BCDEFGH67XY89'), ('Dummy1', 'CPTY_C'), ('Dummy4', 'CPTY_C');

create temp view t_resolved as
select p.netting_set_id, p.counterparty, ns.id as set_id, ns.party_id,
    ns.counterparty_id as set_counterparty_id, ci.counterparty_id as trade_counterparty_id
from t_ore_pairs p
left join ores_refdata_netting_set_identifiers_tbl nsi
  on nsi.tenant_id = ores_utility_system_tenant_id_fn()
 and nsi.id_scheme = 'ORE' and nsi.id_value = p.netting_set_id
 and nsi.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_refdata_netting_sets_tbl ns
  on ns.id = nsi.netting_set_id
 and ns.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_refdata_counterparty_identifiers_tbl ci
  on ci.tenant_id = ores_utility_system_tenant_id_fn()
 and ci.id_scheme = 'ORE' and ci.id_value = p.counterparty
 and ci.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select count(distinct netting_set_id) from t_resolved where set_id is not null),
    19::bigint,
    'every netting set id the ORE samples use resolves');

select is(
    (select count(*) from t_resolved
     where set_counterparty_id is distinct from trade_counterparty_id),
    0::bigint,
    'each set''s counterparty is the one its trades name');

select is(
    (select count(*) from t_resolved r
     join ores_refdata_parties_tbl pa
       on pa.id = r.party_id
      and pa.valid_to = ores_utility_infinity_timestamp_fn()
     where pa.parent_party_id is null and pa.party_category <> 'System'),
    20::bigint,
    'every set belongs to the root party');

select is(
    (select count(*) from ores_refdata_csas_tbl c
     join ores_refdata_netting_set_identifiers_tbl nsi
       on nsi.netting_set_id = c.netting_set_id
      and nsi.id_scheme = 'ORE'
      and nsi.valid_to = ores_utility_infinity_timestamp_fn()
     where c.tenant_id = ores_utility_system_tenant_id_fn()
       and c.valid_to = ores_utility_infinity_timestamp_fn()
       and nsi.id_value in ('CPTY_A', 'CPTY_B', 'CPTY_C', 'CPTY_D', 'CPTY',
                            'CPTY_A_full', 'CPTY_B_full')),
    7::bigint,
    'the sets ORE gives a CSA hold one');

select results_eq(
    $$select c.is_active, c.minimum_transfer_amount_pay, c.margin_period_of_risk, c.index_name
      from ores_refdata_csas_tbl c
      join ores_refdata_netting_set_identifiers_tbl nsi
        on nsi.netting_set_id = c.netting_set_id
       and nsi.id_scheme = 'ORE' and nsi.id_value = 'CPTY_B'
       and nsi.valid_to = ores_utility_infinity_timestamp_fn()
      where c.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values (true, 5000000::double precision, '2W'::text, 'EUR-EONIA'::text)$$,
    'the CPTY_B CSA carries ORE''s terms');

select is(
    (select count(*) from ores_refdata_csa_eligible_currencies_tbl e
     join ores_refdata_csas_tbl c
       on c.id = e.csa_id
      and c.valid_to = ores_utility_infinity_timestamp_fn()
     join ores_refdata_netting_sets_tbl ns
       on ns.id = c.netting_set_id
      and ns.code like 'NS-%'
      and ns.valid_to = ores_utility_infinity_timestamp_fn()
     where e.currency_code = 'EUR' and e.position = 0
       and e.valid_to = ores_utility_infinity_timestamp_fn()),
    7::bigint,
    'each CSA takes EUR as its first eligible currency');

select is(
    (select string_agg(r.action || ' ' || r.record_count, ', ')
     from (select * from ores_refdata_publish_netting_agreements_from_dq_fn(
               (select id from ores_dq_datasets_tbl where code = 'ore.sample_netting_agreements'
                  and valid_to = ores_utility_infinity_timestamp_fn()),
               ores_utility_system_tenant_id_fn())
           union all
           select * from ores_refdata_publish_netting_sets_from_dq_fn(
               (select id from ores_dq_datasets_tbl where code = 'ore.sample_netting_sets'
                  and valid_to = ores_utility_infinity_timestamp_fn()),
               ores_utility_system_tenant_id_fn())
           union all
           select * from ores_refdata_publish_csas_from_dq_fn(
               (select id from ores_dq_datasets_tbl where code = 'ore.sample_csas'
                  and valid_to = ores_utility_infinity_timestamp_fn()),
               ores_utility_system_tenant_id_fn())
           union all
           select * from ores_refdata_publish_netting_set_aliases_from_dq_fn(
               (select id from ores_dq_datasets_tbl where code = 'ore.netting_set_aliases'
                  and valid_to = ores_utility_infinity_timestamp_fn()),
               ores_utility_system_tenant_id_fn())) r),
    'skipped 8, skipped 19, skipped 7, skipped 19',
    'a second publish writes nothing');

select results_eq(
    $$select action, record_count from ores_refdata_publish_netting_agreements_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'ore.sample_netting_agreements'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        '00000000-0000-0000-0000-00000000a0a0'::uuid)$$,
    $$values ('skipped_no_party'::text, 0::bigint)$$,
    'a publish to a tenant with no party is skipped');

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.netting_set_without_agreement', 'ORE', 'Netting and Collateral',
        'Reference Data', 'NONE', 'Derived', 'Synthetic', 'Raw',
        'ORE Sample Netting Synthesis', 'Netting set publish test', 'Test dataset.',
        'ORE', 'Test', current_date, 'Modified BSD License', 'netting_sets');
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'test.netting_set_without_agreement'
      and valid_to = ores_utility_infinity_timestamp_fn();
    insert into ores_dq_netting_sets_artefact_tbl (dataset_id, tenant_id, code, version,
        agreement_number, description)
    values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'NSTEST-ORPHAN', 0,
         'NO-SUCH-AGREEMENT', 'test'),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'NSTEST-STANDALONE', 0,
         null, 'test');
end $$;

select results_eq(
    $$select action, record_count from ores_refdata_publish_netting_sets_from_dq_fn(
        (select id from ores_dq_datasets_tbl where code = 'test.netting_set_without_agreement'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())$$,
    $$values ('inserted'::text, 1::bigint), ('skipped'::text, 1::bigint)$$,
    'a set whose agreement is unknown is skipped, a set with none is written');

select is(
    (select netting_agreement_id::text from ores_refdata_netting_sets_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = 'NSTEST-STANDALONE'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    null,
    'a set staged with no agreement is written with none');

select * from finish();

rollback;
