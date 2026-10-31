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
 * The ore.conventions dataset publishes its atomised tenor basis swap kind.
 *
 * Publishing the real dataset for a party writes the central convention row and
 * the one leg form the corpus stages. A repeat publish changes nothing. A
 * convention staged in the other form closes the form it held, so it never
 * carries both at once.
 *
 * Run with: pg_prove -d <database> test/refdata_conventions_publish_test.sql
 */

begin;

select plan(11);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- A convention names its currencies, and a currency term is checked against the
-- currency catalogue. A fresh database has published nothing, so the test
-- publishes the ISO codes the conventions are about to name.
select * from ores_refdata_publish_currencies_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where code = 'iso.currencies'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

-- A party of the test's own. The corpus is published into the tenant on
-- provisioning, so publishing it for the system party would race that
-- publication; a fresh party is the test's alone.
insert into ores_refdata_parties_tbl (
    id, tenant_id, version, full_name, short_code, party_category, party_type,
    parent_party_id, business_center_code, status,
    modified_by, performed_by, change_reason_code, change_commentary
) values (
    'a0000000-0000-0000-0000-00000000c0de'::uuid,
    ores_utility_system_tenant_id_fn(), 0, 'pgTAP conventions party', 'PGTAP-CONV',
    'Operational', 'Bank', null, 'WRLD', 'Active',
    current_user, current_user, 'system.test', 'pgTAP conventions publish fixture'
);

create temporary table t_ctx on commit drop as
select (select id from ores_dq_datasets_tbl
        where code = 'ore.conventions'
          and valid_to = ores_utility_infinity_timestamp_fn()) as dataset_id,
       'a0000000-0000-0000-0000-00000000c0de'::uuid as party_id;

select set_config('app.visible_party_ids',
    '{' || (select party_id::text from t_ctx) || '}', true);

-- One extra convention of the atomised kind, staged in the paying and receiving
-- form. The corpus is staged in the same dataset, so the real publish runs over
-- it too; this row gives the test an identity of its own.
insert into ores_dq_conventions_artefact_tbl (
    dataset_id, tenant_id, kind_code, name, oresmd_uri)
select dataset_id, ores_utility_system_tenant_id_fn(),
    'TENOR_BASIS_SWAP', 'PGTAP-TBS-BASIS', null
from t_ctx;

insert into ores_dq_tenor_basis_swap_pay_receives_artefact_tbl (
    dataset_id, tenant_id, convention_name, kind_code,
    pay_index, pay_frequency, receive_index, receive_frequency,
    spread_on_rec, include_spread, sub_periods_coupon_type,
    pay_is_averaged, rec_is_averaged)
select dataset_id, ores_utility_system_tenant_id_fn(),
    'PGTAP-TBS-BASIS', 'TENOR_BASIS_SWAP',
    'EUR-EURIBOR-6M', null, 'EUR-EURIBOR-3M', null,
    null, null, null, null, null
from t_ctx;

select ores_refdata_publish_conventions_from_dq_fn(
    (select dataset_id from t_ctx),
    ores_utility_system_tenant_id_fn(),
    'upsert',
    jsonb_build_object('party_id', (select party_id::text from t_ctx)));

create temporary table t_central on commit drop as
select c.id, c.version
from ores_refdata_conventions_tbl c, t_ctx
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.party_id = t_ctx.party_id
  and c.kind_code = 'TENOR_BASIS_SWAP'
  and c.name = 'PGTAP-TBS-BASIS'
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select count(*)::int from t_central),
    1,
    'the publish resolves the staged convention to one live central row');

select is(
    (select p.pay_index
     from ores_refdata_tenor_basis_swap_pay_receives_tbl p, t_central c
     where p.convention_id = c.id
       and p.valid_to = ores_utility_infinity_timestamp_fn()),
    'EUR-EURIBOR-6M',
    'the staged paying and receiving form lands under the central row');

select is(
    (select count(*)::int
     from ores_refdata_tenor_basis_swap_long_shorts_tbl l, t_central c
     where l.convention_id = c.id
       and l.valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the other form is not live');

select is(
    (select l.long_index
     from ores_refdata_tenor_basis_swap_long_shorts_tbl l
     join ores_refdata_conventions_tbl c on c.id = l.convention_id
     where c.tenant_id = ores_utility_system_tenant_id_fn()
       and c.kind_code = 'TENOR_BASIS_SWAP'
       and c.name = 'AUD-BBSW-1M-3M-BASIS'
       and c.valid_to = ores_utility_infinity_timestamp_fn()
       and l.valid_to = ores_utility_infinity_timestamp_fn()),
    'AUD-BBSW-3M',
    'a corpus convention lands in the long and short form the corpus stages');

select is(
    (select count(*)::int
     from ores_refdata_publish_conventions_from_dq_fn(
         (select dataset_id from t_ctx),
         ores_utility_system_tenant_id_fn(),
         'upsert',
         jsonb_build_object('party_id', (select party_id::text from t_ctx)))
     where action = 'inserted'),
    0,
    'a repeat publish inserts nothing');

select is(
    (select count(*)::int
     from ores_refdata_publish_conventions_from_dq_fn(
         (select dataset_id from t_ctx),
         ores_utility_system_tenant_id_fn(),
         'upsert',
         jsonb_build_object('party_id', (select party_id::text from t_ctx)))
     where action = 'deleted'),
    0,
    'a repeat publish closes nothing');

select is(
    (select c.version
     from ores_refdata_conventions_tbl c, t_central t
     where c.id = t.id
       and c.valid_to = ores_utility_infinity_timestamp_fn()),
    (select version from t_central),
    'a repeat publish leaves the central row at the version it held');

-- The staged form changes: the paying and receiving row is replaced by a long
-- and short one, for the same convention identity.
delete from ores_dq_tenor_basis_swap_pay_receives_artefact_tbl
where dataset_id = (select dataset_id from t_ctx)
  and convention_name = 'PGTAP-TBS-BASIS';

insert into ores_dq_tenor_basis_swap_long_shorts_artefact_tbl (
    dataset_id, tenant_id, convention_name, kind_code,
    long_index, long_pay_tenor, short_index, short_pay_tenor,
    spread_on_short, include_spread, sub_periods_coupon_type)
select dataset_id, ores_utility_system_tenant_id_fn(),
    'PGTAP-TBS-BASIS', 'TENOR_BASIS_SWAP',
    'EUR-EURIBOR-6M', null, 'EUR-EURIBOR-3M', null,
    null, null, null
from t_ctx;

create temporary table t_form_change on commit drop as
select * from ores_refdata_publish_conventions_from_dq_fn(
    (select dataset_id from t_ctx),
    ores_utility_system_tenant_id_fn(),
    'upsert',
    jsonb_build_object('party_id', (select party_id::text from t_ctx)));

select is(
    (select record_count::int from t_form_change where action = 'deleted'),
    1,
    'the form change closes exactly the paying and receiving form');

select is(
    (select count(*)::int
     from ores_refdata_tenor_basis_swap_pay_receives_tbl p, t_central c
     where p.convention_id = c.id
       and p.valid_to = ores_utility_infinity_timestamp_fn()),
    0,
    'the previous form is no longer live');

select is(
    (select l.long_index
     from ores_refdata_tenor_basis_swap_long_shorts_tbl l, t_central c
     where l.convention_id = c.id
       and l.valid_to = ores_utility_infinity_timestamp_fn()),
    'EUR-EURIBOR-6M',
    'the new form is live under the same central row');

select is(
    (select count(*)::int from t_central),
    1,
    'the form change leaves one central row, not two');

select * from finish();
rollback;
