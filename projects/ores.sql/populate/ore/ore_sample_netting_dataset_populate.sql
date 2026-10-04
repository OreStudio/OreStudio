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
-- ORE Sample Netting Datasets
--
-- The netting a party needs to import the ORE sample documents: a master
-- agreement with each bank the samples' counterparties map onto, a netting set
-- for every NettingSetId the sample trades use, the CSA ORE defines for a set,
-- and the ORE alias each set answers to. The four datasets publish in that
-- order, each to the party a publish names, and are the members of the
-- ore_samples bundle.
--
-- The netting set ids and their counterparties come from an inventory of the
-- trade envelopes under external/ore/examples; the CSA terms are the most
-- common netting set definition ORE gives each id. Both are recorded on the
-- task that added this script.
-- =============================================================================

\echo '--- ORE Sample Netting Methodology ---'

do $$
begin
    perform ores_dq_methodologies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ORE Sample Netting Synthesis',
        'Synthetic netting agreements, netting sets and CSAs for the netting set ids the ORE sample documents use, built from the samples'' own netting set definitions',
        'https://github.com/OpenSourceRisk/Engine/tree/master/Examples',
        'Data Sourcing and Generation Steps:

    1. INVENTORY THE NETTING SET IDS
       Source: the trade envelopes of every portfolio under
       external/ore/examples, the ORE examples vendored at the engine commit
       recorded in external/ore/examples/manifest.json.
       Each NettingSetId is paired with the CounterParty its trades name.
       Every id names one counterparty; NS is shared by CPTY and CP, which
       map onto the same bank.

    2. MAP COUNTERPARTIES ONTO BANKS
       Each CounterParty maps onto a GLEIF bank through the
       ore.counterparty_aliases dataset. The bank decides the master
       agreement a netting set is opened under.

    3. SYNTHESISE THE AGREEMENTS
       One master agreement per bank, between the party a publish names
       and the bank: ISDA 2002 for banks under English or New York law,
       FBF for the French banks. The numbers follow the pattern
       ACME-<type>-<bank>-<year>.

    4. SYNTHESISE THE NETTING SETS
       One netting set per NettingSetId, opened under the agreement with its
       counterparty''s bank, with a code of the pattern NS-<bank>-<purpose>-<n>.
       The NettingSetId becomes the set''s ORE alias.

    5. TAKE THE CSA TERMS FROM ORE
       Source: the NettingSetDefinitions files under external/ore/examples.
       For each id ORE defines, the most common definition is taken
       verbatim, including its ActiveCSAFlag and index. An id ORE defines
       without a CSA, and an id ORE does not define, gets no CSA: its set is
       uncollateralised.

    6. PUBLISH
       The ore_samples bundle publishes the four datasets in
       dependency order to the party named by the publish parameters.'
    );
end $$;

\echo '--- ORE Sample Netting Agreements Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_netting_agreements',
        'ORE',
        'Netting and Collateral',
        'Reference Data',
        'NONE',
        'Derived',
        'Synthetic',
        'Raw',
        'ORE Sample Netting Synthesis',
        'ORE Sample Netting Agreements',
        'A master netting agreement with each bank the ORE sample counterparties map onto.',
        'ORE',
        'Netting agreements for importing the ORE samples',
        '2026-10-04'::date,
        'Modified BSD License',
        'netting_agreements'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_netting_agreements'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_netting_agreements_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_netting_agreements_artefact_tbl (
        dataset_id, tenant_id, agreement_number, version, counterparty_lei,
        agreement_type, governing_law, description
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), a.agreement_number, 0,
        a.lei, a.agreement_type, a.governing_law, a.description
    from (values
        ('ACME-ISDA-BARC-2002', 'G5GSEF7VJP5I7OUK5573', 'ISDA', 'English',
         'ISDA 2002 Master Agreement with Barclays Bank PLC'),
        ('ACME-ISDA-DEUT-2002', '7LTWFZYICNSX8D621K86', 'ISDA', 'English',
         'ISDA 2002 Master Agreement with Deutsche Bank AG'),
        ('ACME-ISDA-JPMC-2002', '7H6GLXDRUGQFU57RNE97', 'ISDA', 'New York',
         'ISDA 2002 Master Agreement with JPMorgan Chase Bank, N.A.'),
        ('ACME-FBF-BNPP-2013', 'R0MUWSFPU8MPRO8K5P83', 'FBF', 'French',
         'FBF Master Agreement with BNP Paribas'),
        ('ACME-ISDA-UBS-2002', 'BFM8T61CT2L1QCEMIK50', 'ISDA', 'English',
         'ISDA 2002 Master Agreement with UBS AG'),
        ('ACME-FBF-SOGE-2013', 'O2RNE8IBXP4R0TD8PU41', 'FBF', 'French',
         'FBF Master Agreement with Societe Generale'),
        ('ACME-ISDA-NWM-2002', 'RR3QWICWWIPCS8A4S074', 'ISDA', 'English',
         'ISDA 2002 Master Agreement with NatWest Markets Plc'),
        ('ACME-ISDA-BOFA-2002', 'B4TYDEB6GKMZO031MB27', 'ISDA', 'New York',
         'ISDA 2002 Master Agreement with Bank of America, N.A.')
    ) as a(agreement_number, lei, agreement_type, governing_law, description);

    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_netting_agreements', 'gleif.lei_counterparties.small', 'counterparty_reference');
end $$;

\echo '--- ORE Sample Netting Sets Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_netting_sets',
        'ORE',
        'Netting and Collateral',
        'Reference Data',
        'NONE',
        'Derived',
        'Synthetic',
        'Raw',
        'ORE Sample Netting Synthesis',
        'ORE Sample Netting Sets',
        'A netting set for every netting set id the ORE sample trades use, each under the agreement with its counterparty''s bank.',
        'ORE',
        'Netting sets for importing the ORE samples',
        '2026-10-04'::date,
        'Modified BSD License',
        'netting_sets'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_netting_sets'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_netting_sets_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_netting_sets_artefact_tbl (
        dataset_id, tenant_id, code, version, agreement_number, description
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), s.code, 0,
        s.agreement_number, s.description
    from (values
        ('NS-BARC-CSA-01', 'ACME-ISDA-BARC-2002', 'Barclays rates and FX book under the CSA'),
        ('NS-BARC-VMIM-01', 'ACME-ISDA-BARC-2002', 'Barclays book under the variation and initial margin CSA'),
        ('NS-BARC-PRICING-01', 'ACME-ISDA-BARC-2002', 'Barclays pricing test book, uncollateralised'),
        ('NS-BARC-SIMM-01', 'ACME-ISDA-BARC-2002', 'Barclays book for the SIMM sensitivities run, uncollateralised'),
        ('NS-BARC-WRAP-01', 'ACME-ISDA-BARC-2002', 'Barclays wrapped trades, uncollateralised'),
        ('NS-DEUT-CSA-01', 'ACME-ISDA-DEUT-2002', 'Deutsche Bank book under the CSA'),
        ('NS-DEUT-VMIM-01', 'ACME-ISDA-DEUT-2002', 'Deutsche Bank book under the variation and initial margin CSA'),
        ('NS-JPMC-MAIN-01', 'ACME-ISDA-JPMC-2002', 'JPMorgan main book, uncollateralised'),
        ('NS-JPMC-CSA-01', 'ACME-ISDA-JPMC-2002', 'JPMorgan book under the CSA'),
        ('NS-JPMC-CS-01', 'ACME-ISDA-JPMC-2002', 'JPMorgan credit book, uncollateralised'),
        ('NS-BNPP-CSA-01', 'ACME-FBF-BNPP-2013', 'BNP Paribas book under the CSA'),
        ('NS-BNPP-UNC-01', 'ACME-FBF-BNPP-2013', 'BNP Paribas first uncollateralised book'),
        ('NS-BNPP-UNC-02', 'ACME-FBF-BNPP-2013', 'BNP Paribas second uncollateralised book'),
        ('NS-UBS-CSA-01', 'ACME-ISDA-UBS-2002', 'UBS book under the CSA'),
        ('NS-UBS-UNC-01', 'ACME-ISDA-UBS-2002', 'UBS uncollateralised book'),
        ('NS-SOGE-01', 'ACME-FBF-SOGE-2013', 'Societe Generale book, uncollateralised'),
        ('NS-NWM-EQ-01', 'ACME-ISDA-NWM-2002', 'NatWest first equity option book, uncollateralised'),
        ('NS-NWM-EQ-02', 'ACME-ISDA-NWM-2002', 'NatWest second equity option book, uncollateralised'),
        ('NS-BOFA-01', 'ACME-ISDA-BOFA-2002', 'Bank of America book, uncollateralised')
    ) as s(code, agreement_number, description);

    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_netting_sets', 'ore.sample_netting_agreements', 'agreement_reference');
end $$;

\echo '--- ORE Sample CSAs Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_csas',
        'ORE',
        'Netting and Collateral',
        'Reference Data',
        'NONE',
        'Derived',
        'Synthetic',
        'Raw',
        'ORE Sample Netting Synthesis',
        'ORE Sample CSAs',
        'The credit support annexes ORE defines for the sample netting sets, with the terms of each id''s most common ORE definition.',
        'ORE',
        'CSAs for importing the ORE samples',
        '2026-10-04'::date,
        'Modified BSD License',
        'csas'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_csas'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_csas_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_csas_artefact_tbl (
        dataset_id, tenant_id, netting_set_code, version, is_active, bilateral,
        csa_currency, index_name, threshold_pay, threshold_receive,
        minimum_transfer_amount_pay, minimum_transfer_amount_receive,
        independent_amount_held, independent_amount_type, call_frequency,
        post_frequency, margin_period_of_risk, collateral_compounding_spread_receive,
        collateral_compounding_spread_pay, eligible_currencies
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), c.netting_set_code, 0,
        c.is_active, 'Bilateral', 'EUR', c.index_name, c.threshold, c.threshold,
        c.mta, c.mta, 0, 'FIXED', '1D', '1D', c.mpor, 0, 0, 'EUR'
    from (values
        ('NS-BARC-CSA-01', false, 'EUR-EONIA', 100000::double precision, 0::double precision, '0W'),
        ('NS-BARC-VMIM-01', true, 'EUR-ESTER', 0, 0, '2W'),
        ('NS-DEUT-CSA-01', true, 'EUR-EONIA', 0, 5000000, '2W'),
        ('NS-DEUT-VMIM-01', true, 'EUR-ESTER', 0, 0, '2W'),
        ('NS-JPMC-CSA-01', false, 'EUR-EONIA', 0, 50000, '2W'),
        ('NS-BNPP-CSA-01', false, 'EUR-EONIA', 0, 0, '0W'),
        ('NS-UBS-CSA-01', false, 'EUR-EONIA', 0, 0, '2W')
    ) as c(netting_set_code, is_active, index_name, threshold, mta, mpor);

    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_csas', 'ore.sample_netting_sets', 'netting_set_reference');
    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.sample_csas', 'iso.currencies', 'currency_reference');
end $$;

\echo '--- ORE Netting Set Aliases Dataset ---'

do $$
declare
    v_dataset_id uuid;
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.netting_set_aliases',
        'ORE',
        'Netting and Collateral',
        'Reference Data',
        'NONE',
        'Derived',
        'Synthetic',
        'Raw',
        'ORE Sample Netting Synthesis',
        'ORE Netting Set Aliases',
        'The netting set ids the ORE sample documents use, mapped onto the sample netting sets by code.',
        'ORE',
        'Netting set aliases for importing the ORE samples',
        '2026-10-04'::date,
        'Modified BSD License',
        'netting_set_aliases'
    );

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.netting_set_aliases'
      and valid_to = ores_utility_infinity_timestamp_fn();

    delete from ores_dq_netting_set_aliases_artefact_tbl where dataset_id = v_dataset_id;

    insert into ores_dq_netting_set_aliases_artefact_tbl (
        dataset_id, tenant_id, id_value, version, id_scheme, netting_set_code, description
    )
    select v_dataset_id, ores_utility_system_tenant_id_fn(), a.alias, 0, 'ORE', a.code,
        'Netting set id used by the ORE sample documents'
    from (values
        ('CPTY_A', 'NS-BARC-CSA-01'),
        ('CPTY_A_full', 'NS-BARC-VMIM-01'),
        ('PricerStaticDate', 'NS-BARC-PRICING-01'),
        ('CRIF_20191230', 'NS-BARC-SIMM-01'),
        ('CPTY_A_tradeTypeWrapper', 'NS-BARC-WRAP-01'),
        ('CPTY_B', 'NS-DEUT-CSA-01'),
        ('CPTY_B_full', 'NS-DEUT-VMIM-01'),
        ('NS', 'NS-JPMC-MAIN-01'),
        ('CPTY', 'NS-JPMC-CSA-01'),
        ('CS', 'NS-JPMC-CS-01'),
        ('CPTY_C', 'NS-BNPP-CSA-01'),
        ('Dummy1', 'NS-BNPP-UNC-01'),
        ('Dummy4', 'NS-BNPP-UNC-02'),
        ('CPTY_D', 'NS-UBS-CSA-01'),
        ('DUMMY_NS', 'NS-UBS-UNC-01'),
        ('1234', 'NS-SOGE-01'),
        ('EquityOption1', 'NS-NWM-EQ-01'),
        ('EquityOption2', 'NS-NWM-EQ-02'),
        ('ABC1234', 'NS-BOFA-01')
    ) as a(alias, code);

    perform ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.netting_set_aliases', 'ore.sample_netting_sets', 'netting_set_reference');
end $$;
