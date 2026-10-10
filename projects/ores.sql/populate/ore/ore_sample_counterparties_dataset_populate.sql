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
-- ORE Sample Counterparties Dataset
--
-- The GLEIF banks the ORE sample documents' placeholder counterparty names map
-- onto. The ORE examples name their counterparties CPTY_A, CPTY_B, CPTY and the
-- like; an ORE import resolves an envelope's CounterParty through an ORE
-- identifier, so each placeholder is an alias of one of these banks. The
-- aliases are staged here and published at runtime by the bundle publish.
--
-- The rows are the banks' own GLEIF records, selected from the small GLEIF
-- dataset rather than copied, so they stay what GLEIF publishes. Loaded after
-- the GLEIF staging data, which this selects from.
-- =============================================================================

\echo '--- ORE Sample Counterparties Dataset ---'

do $$
declare
    v_dataset_id uuid;
    v_source_id uuid;
    v_copied bigint;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_counterparties',
        'ORE',
        'Parties',
        'Reference Data',
        'LEI',
        'Primary',
        'Actual',
        'Raw',
        'GLEIF Golden Copy Extraction',
        'ORE Sample Counterparties',
        'ORE sample data: the GLEIF banks the ORE sample documents'' placeholder counterparty names map onto.',
        'GLEIF',
        'Counterparties for importing the ORE samples',
        '2026-10-04'::date,
        'Open Data',
        'lei_entities'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_counterparties'
      and valid_to = ores_utility_infinity_timestamp_fn();
    select id into v_source_id from ores_dq_datasets_tbl
    where code = 'gleif.lei_entities.small'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_source_id is null then
        raise exception 'Dataset not found: gleif.lei_entities.small. Load the GLEIF catalogue first.';
    end if;

    delete from ores_dq_lei_entities_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_lei_entities_artefact_tbl (dataset_id, tenant_id, lei, version, entity_legal_name, entity_entity_category, entity_entity_sub_category, entity_entity_status, entity_legal_form_entity_legal_form_code, entity_legal_form_other_legal_form, entity_legal_jurisdiction, entity_legal_address_first_address_line, entity_legal_address_city, entity_legal_address_region, entity_legal_address_country, entity_legal_address_postal_code, entity_headquarters_address_first_address_line, entity_headquarters_address_city, entity_headquarters_address_region, entity_headquarters_address_country, entity_headquarters_address_postal_code, entity_entity_creation_date, registration_initial_registration_date, registration_last_update_date, registration_next_renewal_date, registration_registration_status, entity_transliterated_name_1, entity_transliterated_name_1_type)
    select v_dataset_id, tenant_id, lei, version, entity_legal_name, entity_entity_category, entity_entity_sub_category, entity_entity_status, entity_legal_form_entity_legal_form_code, entity_legal_form_other_legal_form, entity_legal_jurisdiction, entity_legal_address_first_address_line, entity_legal_address_city, entity_legal_address_region, entity_legal_address_country, entity_legal_address_postal_code, entity_headquarters_address_first_address_line, entity_headquarters_address_city, entity_headquarters_address_region, entity_headquarters_address_country, entity_headquarters_address_postal_code, entity_entity_creation_date, registration_initial_registration_date, registration_last_update_date, registration_next_renewal_date, registration_registration_status, entity_transliterated_name_1, entity_transliterated_name_1_type
    from ores_dq_lei_entities_artefact_tbl
    where dataset_id = v_source_id
      and lei in (
        select lei from (values
            ('G5GSEF7VJP5I7OUK5573', 'Barclays Bank PLC'),
            ('7LTWFZYICNSX8D621K86', 'Deutsche Bank AG'),
            ('7H6GLXDRUGQFU57RNE97', 'JPMorgan Chase Bank, National Association'),
            ('R0MUWSFPU8MPRO8K5P83', 'BNP Paribas'),
            ('MP6I5ZYZBEU3UXPYFY54', 'HSBC Bank PLC'),
            ('BFM8T61CT2L1QCEMIK50', 'UBS AG'),
            ('O2RNE8IBXP4R0TD8PU41', 'Societe Generale'),
            ('RR3QWICWWIPCS8A4S074', 'NatWest Markets PLC'),
            ('B4TYDEB6GKMZO031MB27', 'Bank of America, National Association'),
            ('1VUV7VQFKUOQSJ21A208', 'Credit Agricole Corporate and Investment Bank'),
            ('K6Q0W1PS1L1O4IQL9C32', 'J.P. Morgan Securities PLC')
        ) as bank(lei, legal_name)
      );

    get diagnostics v_copied = row_count;
    if v_copied <> 11 then
        raise exception 'ore.sample_counterparties selected % of its 11 banks from gleif.lei_entities.small.', v_copied;
    end if;

    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_counterparties', 'gleif.lei_entities.small', 'entity_reference');
end $$;

-- =============================================================================
-- ORE Counterparty Aliases Dataset
--
-- The names the ORE examples use for their counterparties, from an inventory of
-- external/ore/examples recorded on the task that added this script, each
-- keyed to the LEI of the bank it maps onto. The busiest names take the largest
-- dealers; CPTY_1 to CPTY_10 appear together in one example and take ten
-- different banks. CP takes the same bank as CPTY, because both trade under
-- the netting set NS, which belongs to one counterparty. Published as ORE
-- counterparty identifiers.
-- =============================================================================

\echo '--- ORE Counterparty Aliases Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.counterparty_aliases',
        'ORE',
        'Parties',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'GLEIF Golden Copy Extraction',
        'ORE Counterparty Aliases',
        'ORE sample data: the counterparty names the ORE sample documents use, mapped onto GLEIF banks by LEI.',
        'ORE',
        'Counterparty aliases for importing the ORE samples',
        '2026-10-04'::date,
        'Open Data',
        'counterparty_aliases'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.counterparty_aliases'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_counterparty_aliases_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_counterparty_aliases_artefact_tbl (
        dataset_id, tenant_id, id_value, version, id_scheme, lei, description
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), a.alias, 0, 'ORE', a.lei,
        'Counterparty name used by the ORE sample documents'
    from (values
        ('CPTY_A', 'G5GSEF7VJP5I7OUK5573'),
        ('A', 'G5GSEF7VJP5I7OUK5573'),
        ('CPTY_B', '7LTWFZYICNSX8D621K86'),
        ('CPTY', '7H6GLXDRUGQFU57RNE97'),
        ('CPTY_C', 'R0MUWSFPU8MPRO8K5P83'),
        ('CP', '7H6GLXDRUGQFU57RNE97'),
        ('CPTY_D', 'BFM8T61CT2L1QCEMIK50'),
        ('DUMMY_CP', 'BFM8T61CT2L1QCEMIK50'),
        ('DUMMY_CPTY', 'BFM8T61CT2L1QCEMIK50'),
        ('ABC', 'O2RNE8IBXP4R0TD8PU41'),
        ('EquityOption1', 'RR3QWICWWIPCS8A4S074'),
        ('EquityOption2', 'RR3QWICWWIPCS8A4S074'),
        ('001B456BCDEFGH67XY89', 'B4TYDEB6GKMZO031MB27'),
        ('CPTY_1', 'O2RNE8IBXP4R0TD8PU41'),
        ('CPTY_2', 'RR3QWICWWIPCS8A4S074'),
        ('CPTY_3', 'B4TYDEB6GKMZO031MB27'),
        ('CPTY_4', '1VUV7VQFKUOQSJ21A208'),
        ('CPTY_5', 'K6Q0W1PS1L1O4IQL9C32'),
        ('CPTY_6', 'G5GSEF7VJP5I7OUK5573'),
        ('CPTY_7', '7LTWFZYICNSX8D621K86'),
        ('CPTY_8', '7H6GLXDRUGQFU57RNE97'),
        ('CPTY_9', 'R0MUWSFPU8MPRO8K5P83'),
        ('CPTY_10', 'MP6I5ZYZBEU3UXPYFY54')
    ) as a(alias, lei);

    -- The aliases resolve against counterparties published from GLEIF.
    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.counterparty_aliases', 'gleif.lei_counterparties.small', 'counterparty_reference');
end $$;
