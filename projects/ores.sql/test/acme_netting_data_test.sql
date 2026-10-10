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
 * pgTAP tests for the ACME netting data.
 *
 * The tests read the staged datasets, so they need no publish. They cover:
 * - The agreements, sets and CSAs each legal entity holds
 * - Every set names an agreement of its own entity, every CSA a set of its own
 * - Every intragroup agreement is held by both entities under one reference
 * - Every CSA is in the currency of a live overnight index, margin currency first
 * - All three collateral regimes are present
 * - Governing law follows the entity's jurisdiction
 * - Nothing reads as sample or test data
 *
 * Run with: pg_prove -d <database> test/acme_netting_data_test.sql
 */

begin;

select plan(14);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_agreements as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.netting_agreements$', '') as company,
       a.*
from ores_dq_netting_agreements_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.netting_agreements';

create temp view t_sets as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.netting_sets$', '') as company,
       s.*
from ores_dq_netting_sets_artefact_tbl s
join ores_dq_datasets_tbl d
  on d.id = s.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.netting_sets';

create temp view t_csas as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.csas$', '') as company,
       c.*
from ores_dq_csas_artefact_tbl c
join ores_dq_datasets_tbl d
  on d.id = c.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.csas';

create temp table t_entity_lei (company text, lei text);
insert into t_entity_lei values
    ('acme_group', '9695ACMEGROUP0000030'), ('acme_uk', '9695ACMEUK0000000047'),
    ('acme_us', '9695ACMEUS0000000043'), ('acme_hk', '9695ACMEHK0000000018');

select results_eq(
    $$select company, count(*) from t_agreements group by company order by company$$,
    $$values ('acme_group'::text, 5::bigint), ('acme_hk', 4), ('acme_uk', 7), ('acme_us', 5)$$,
    'each legal entity holds its agreements');

select is(
    (select count(*) from t_sets where agreement_number is null
        or not exists (select 1 from t_agreements a
                       where a.company = t_sets.company
                         and a.agreement_number = t_sets.agreement_number)),
    0::bigint,
    'every netting set names an agreement of its own entity');

select is(
    (select count(*) from t_csas c
     where not exists (select 1 from t_sets s
                       where s.company = c.company and s.code = c.netting_set_code)),
    0::bigint,
    'every CSA names a netting set of its own entity');

select is(
    (select count(*) from t_agreements a
     join t_entity_lei e on e.lei = a.counterparty_lei
     where not exists (select 1 from t_agreements m
                       where m.company = e.company
                         and m.agreement_number = a.agreement_number
                         and m.counterparty_lei = (select lei from t_entity_lei where company = a.company))),
    0::bigint,
    'every intragroup agreement is held by both entities under one reference');

select is(
    (select count(*) from t_agreements where counterparty_lei like '9695ACME%'),
    8::bigint,
    'the four intragroup agreements are held on both sides');

select is(
    (select count(*) from t_csas
     where index_name not in ('USD-SOFR', 'GBP-SONIA', 'EUR-ESTER')),
    0::bigint,
    'every CSA uses a live overnight index');

select is(
    (select count(*) from t_csas
     where split_part(index_name, '-', 1) != csa_currency
        or split_part(eligible_currencies, ',', 1) != csa_currency),
    0::bigint,
    'every CSA is in the currency of its index, with that currency first');

select is(
    (select count(*) from t_csas where call_frequency != '1D' or post_frequency != '1D'
        or margin_period_of_risk != '10D'),
    0::bigint,
    'every CSA is margined daily over a ten day margin period');

select results_eq(
    $$select
         (select count(*) from t_csas where apply_initial_margin) as with_im,
         (select count(*) from t_csas where not apply_initial_margin) as vm_only,
         (select count(*) from t_sets s
          where not exists (select 1 from t_csas c
                            where c.company = s.company and c.netting_set_code = s.code)) as uncollateralised$$,
    $$values (2::bigint, 12::bigint, 9::bigint)$$,
    'variation and initial margin, variation margin only and uncollateralised are all present');

select is(
    (select count(*) from t_csas
     where not apply_initial_margin and (threshold_pay != 0 or threshold_receive != 0)),
    0::bigint,
    'a variation margin CSA has a zero threshold');

select is(
    (select count(*) from t_csas
     where apply_initial_margin and non_exempt_im_regulations is null),
    0::bigint,
    'an initial margin CSA names its regulations');

select is(
    (select count(*) from t_agreements a
     where a.counterparty_lei not like '9695ACME%'
       and a.governing_law != case when a.company = 'acme_us'
                                          or (a.company = 'acme_group'
                                              and a.counterparty_lei = '7H6GLXDRUGQFU57RNE97')
                                   then 'New York' else 'English' end),
    0::bigint,
    'governing law follows the entity and the bank it deals with');

select is(
    (select count(*) from t_agreements where agreement_type != 'ISDA'),
    0::bigint,
    'every agreement is an ISDA master agreement');

select is(
    (select count(*) from (
        select description from t_sets
        union all select description from t_agreements) d
     where d.description ~* '\y(test|sample|dummy|pricing|wrapper)\y'),
    0::bigint,
    'nothing reads as sample or test data');

select * from finish();

rollback;
