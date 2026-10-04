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
-- ORE Sample Counterparties Publication
--
-- Publishes the ORE sample banks as counterparties of the system tenant, beside
-- ACME's own, and gives every counterparty name the ORE examples use an ORE
-- alias onto one of them. An ORE import resolves an envelope's CounterParty
-- through these aliases, so the examples import against real GLEIF banks.
--
-- The names come from an inventory of external/ore/examples (recorded on the
-- task that added this script). The busiest names map onto the largest dealers;
-- CPTY_1 to CPTY_10 appear together in one example and map onto ten different
-- banks. Runs after the ACME publication, which publishes the business centres
-- a counterparty is validated against. Idempotent.
-- =============================================================================

\echo '--- ORE Sample Counterparties Publication ---'

do $$
declare
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_dataset_id uuid;
    v_result record;
    v_aliases bigint;
begin
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_counterparties'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_id is null then
        raise warning 'ore.sample_counterparties dataset absent; ORE sample counterparties not published.';
        return;
    end if;

    for v_result in
        select * from ores_refdata_publish_lei_counterparties_from_dq_fn(v_dataset_id, v_tenant_id)
    loop
        raise notice 'ore sample counterparties: % %', v_result.action, v_result.record_count;
    end loop;

    insert into ores_refdata_counterparty_identifiers_tbl (
        tenant_id, id, version, counterparty_id, id_scheme, id_value, description,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select v_tenant_id, gen_random_uuid(), 0, ci.counterparty_id, 'ORE', a.alias,
        'Counterparty name used by the ORE sample documents',
        current_user, current_user, 'system.initial_load', 'ORE sample counterparty alias'
    from (values
        ('CPTY_A',               'G5GSEF7VJP5I7OUK5573'),
        ('A',                    'G5GSEF7VJP5I7OUK5573'),
        ('CPTY_B',               '7LTWFZYICNSX8D621K86'),
        ('CPTY',                 '7H6GLXDRUGQFU57RNE97'),
        ('CPTY_C',               'R0MUWSFPU8MPRO8K5P83'),
        ('CP',                   'MP6I5ZYZBEU3UXPYFY54'),
        ('CPTY_D',               'BFM8T61CT2L1QCEMIK50'),
        ('DUMMY_CP',             'BFM8T61CT2L1QCEMIK50'),
        ('DUMMY_CPTY',           'BFM8T61CT2L1QCEMIK50'),
        ('ABC',                  'O2RNE8IBXP4R0TD8PU41'),
        ('EquityOption1',        'RR3QWICWWIPCS8A4S074'),
        ('EquityOption2',        'RR3QWICWWIPCS8A4S074'),
        ('001B456BCDEFGH67XY89', 'B4TYDEB6GKMZO031MB27'),
        ('CPTY_1',               'O2RNE8IBXP4R0TD8PU41'),
        ('CPTY_2',               'RR3QWICWWIPCS8A4S074'),
        ('CPTY_3',               'B4TYDEB6GKMZO031MB27'),
        ('CPTY_4',               '1VUV7VQFKUOQSJ21A208'),
        ('CPTY_5',               'K6Q0W1PS1L1O4IQL9C32'),
        ('CPTY_6',               'G5GSEF7VJP5I7OUK5573'),
        ('CPTY_7',               '7LTWFZYICNSX8D621K86'),
        ('CPTY_8',               '7H6GLXDRUGQFU57RNE97'),
        ('CPTY_9',               'R0MUWSFPU8MPRO8K5P83'),
        ('CPTY_10',              'MP6I5ZYZBEU3UXPYFY54')
    ) as a(alias, lei)
    join ores_refdata_counterparty_identifiers_tbl ci
      on ci.tenant_id = v_tenant_id
     and ci.id_scheme = 'LEI'
     and ci.id_value = a.lei
     and ci.valid_to = ores_utility_infinity_timestamp_fn()
    where not exists (
        select 1 from ores_refdata_counterparty_identifiers_tbl o
        where o.tenant_id = v_tenant_id
          and o.id_scheme = 'ORE'
          and o.id_value = a.alias
          and o.valid_to = ores_utility_infinity_timestamp_fn());

    get diagnostics v_aliases = row_count;
    raise notice 'ore sample counterparty aliases: %', v_aliases;
end $$;
