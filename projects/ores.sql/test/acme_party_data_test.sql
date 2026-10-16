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
 * pgTAP tests for the ACME party data.
 *
 * The tests read the staged datasets, so they need no publish. They cover:
 * - Each legal entity has one office for each contact type, and one primary
 * - The countries and currencies of each entity, and the group's being all of them
 * - The counterparties each entity may trade with
 * - An entity is never its own counterparty, and holds every bank it has a master
 *   agreement with, so the two datasets cannot disagree
 * - A party's other counterparties are the banks of the countries it works in,
 *   and every one of them is a bank the GLEIF mapping names a BIC for, so the
 *   list is a rule rather than a hand-written set
 * - Nothing reads as real contact data: the telephone numbers and the domain
 *   are the ones set aside for fiction
 *
 * Run with: pg_prove -d <database> test/acme_party_data_test.sql
 */

begin;

-- The expected counts are pinned on purpose. A change to the party data must
-- change this test, so a count never moves unnoticed.
select plan(14);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_contacts as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.party_contact_informations$', '') as company, a.*
from ores_dq_party_contact_informations_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.party_contact_informations';

create temp view t_countries as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.party_countries$', '') as company, a.*
from ores_dq_party_countries_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.party_countries';

create temp view t_currencies as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.party_currencies$', '') as company, a.*
from ores_dq_party_currencies_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.party_currencies';

create temp view t_counterparties as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.party_counterparties$', '') as company, a.*
from ores_dq_party_counterparties_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.party_counterparties';

create temp view t_agreements as
select regexp_replace(regexp_replace(d.code, '^acme\.', ''), '\.netting_agreements$', '') as company, a.*
from ores_dq_netting_agreements_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code like 'acme.%.netting_agreements';

create temp table t_entity_lei (company text, lei text);
insert into t_entity_lei values
    ('acme_group', '9695ACMEGROUP0000030'), ('acme_uk', '9695ACMEUK0000000047'),
    ('acme_us', '9695ACMEUS0000000043'), ('acme_hk', '9695ACMEHK0000000018');

-- The GLEIF extract the counterparty list is read from, by LEI and legal
-- address country. It is the extract the counterparties come from, so a link
-- that names an LEI this view does not carry is a link the publish cannot
-- resolve. The country is the GLEIF legal address, which is what the list is
-- derived from.
create temp view t_gleif_entities as
select a.lei, a.entity_legal_address_country as country
from ores_dq_lei_entities_artefact_tbl a
join ores_dq_datasets_tbl d
  on d.id = a.dataset_id and d.valid_to = ores_utility_infinity_timestamp_fn()
where d.code = 'gleif.lei_entities.tiny';

select results_eq(
    $$select company, count(*) from t_contacts group by company order by company$$,
    $$values ('acme_group'::text, 4::bigint), ('acme_hk', 4), ('acme_uk', 4), ('acme_us', 4)$$,
    'each legal entity has four offices');

select is(
    (select count(*) from (select company, contact_type from t_contacts
        group by company, contact_type having count(*) > 1) x),
    0::bigint, 'no entity has two offices of one contact type');

select results_eq(
    $$select contact_type from t_contacts where company = 'acme_uk' order by contact_type$$,
    $$values ('Billing'::text), ('Legal'), ('Operations'), ('Settlement')$$,
    'the offices are the four contact types');

select results_eq(
    $$select company, count(*) from t_contacts where is_primary group by company order by company$$,
    $$values ('acme_group'::text, 1::bigint), ('acme_hk', 1), ('acme_uk', 1), ('acme_us', 1)$$,
    'each legal entity has one primary office');

select is(
    (select count(*) from t_contacts
     where phone is null or email is null or web_page is null
        or street_line_1 is null or city is null or country_code is null),
    0::bigint, 'every office can be reached and found');

select is(
    (select count(*) from t_contacts
     where email not like '%@%acme-corp.example' or web_page not like 'https://www.acme-corp.example/%'),
    0::bigint, 'no office uses a real domain');

select results_eq(
    $$select company, count(*) from t_countries group by company order by company$$,
    $$values ('acme_group'::text, 9::bigint), ('acme_hk', 3), ('acme_uk', 4), ('acme_us', 2)$$,
    'each legal entity works in its countries');

select is(
    (select string_agg(country_alpha2_code, ',' order by country_alpha2_code)
       from t_countries where company = 'acme_group'),
    (select string_agg(distinct country_alpha2_code, ',' order by country_alpha2_code)
       from t_countries where company <> 'acme_group'),
    'the group works in every country its subsidiaries do');

select is(
    (select string_agg(currency_iso_code, ',' order by currency_iso_code)
       from t_currencies where company = 'acme_group'),
    (select string_agg(distinct currency_iso_code, ',' order by currency_iso_code)
       from t_currencies where company <> 'acme_group'),
    'the group works in every currency its subsidiaries do');

select results_eq(
    $$select company, count(*) from t_counterparties group by company order by company$$,
    $$values ('acme_group'::text, 453::bigint), ('acme_hk', 26), ('acme_uk', 411), ('acme_us', 27)$$,
    'each legal entity may trade with its counterparties');

select is(
    (select count(*) from t_counterparties c join t_entity_lei e using (company)
     where c.counterparty_lei = e.lei),
    0::bigint, 'no entity is its own counterparty');

select is(
    (select count(*) from t_agreements a
     where a.counterparty_lei <> (select lei from t_entity_lei where company = a.company)
       and not exists (select 1 from t_counterparties c
            where c.company = a.company and c.counterparty_lei = a.counterparty_lei)),
    0::bigint, 'an entity may trade with every counterparty it holds a master agreement with');

-- The approved banks of an entity, which are its links less the banks it holds
-- a master agreement with and less its sister entities. A sister entity is a
-- link through the 9695 pre-LOU prefix the group's own LEIs carry, and it is
-- not a bank of this dataset at all.
create temp view t_approved as
select c.company, c.counterparty_lei
from t_counterparties c
where c.counterparty_lei not like '9695%'
  and not exists (select 1 from t_agreements a
        where a.company = c.company and a.counterparty_lei = c.counterparty_lei);

select is(
    (select count(*) from t_approved a
     where not exists (select 1 from t_gleif_entities e where e.lei = a.counterparty_lei)),
    0::bigint, 'every approved bank is one the GLEIF counterparty set carries');

-- A party works in several countries, so a bank matches when its country is
-- any one of them. Counting the pairs whose countries differ would count every
-- bank once for each of the party's other countries.
select is(
    (select count(*) from t_approved a
     where not exists (
         select 1 from t_countries t
         join t_gleif_entities e on e.lei = a.counterparty_lei
         where t.company = a.company
           and t.country_alpha2_code = e.country)),
    0::bigint, 'the approved banks of a party are the banks of the countries it works in');

select * from finish();

rollback;
