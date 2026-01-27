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
 * DQ Artefact FpML Regulatory Corporate Sector Population Script
 *
 * Populates the dq_regulatory_corporate_sectors_artefact_tbl with reference data.
 * Dataset: fpml.regulatory_corporate_sector
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_regulatory_corporate_sectors_fn() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Regulatory Corporate Sector
-- =============================================================================

\echo '--- DQ Artefact FpML Regulatory Corporate Sector ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.regulatory_corporate_sector'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.regulatory_corporate_sector not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_regulatory_corporate_sectors_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AccommodationFoodService',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 9 = Accommodation and food service activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AdministrationSupport',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 14 = Administrative and support service activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AgricultureForestryFishing',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 1 = Agriculture, forestry and fishing. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AlternativeInvestmentFund',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'L=Alternative investment fund managed by AIFMs authorised or registered in accordance with Directive 2011/61/EU;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ArtsEntertainmentRecreation',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 18 = Arts, entertainment and recreation. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AssuranceUndertaking',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'A=Assurance undertaking authorised in accordance with Directive 2002/83/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Construction',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 6 = Construction. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Corporate',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'Corporate, as defined by HKMA.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CreditInstitution',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'C=Credit institution authorised in accordance with Directive 2006/48/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Education',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 16 = Education. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EletricityGas',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 4 = Electricity, gas, steam and air conditioning supply. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ExtraterritorialOrganizations',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 21 = Activities of extraterritorial organisations and bodies. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FinanceInsurance',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 11 = Financial and insurance activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FinancialEntity',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'The identification of Financial Entity can be determined by the entityClassification Coding scheme under reportingRegime/entityClassification'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HealthSocialWork',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 17 = Human health and social work activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Household',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 20 = Activities of households as employers; undifferentiated goods – and services –producing activities of households for own use. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Individual',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'Individual, as defined by HKMA.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InformationCommunication',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 10 = Information and communication. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InstitutionForOccupationalRetirementProvision',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'O=Institution for occupational retirement provision within the meaning of Article 6(a) of Directive 2003/41/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InsuranceUndertaking',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'I=Insurance undertaking authorised in accordance with Directive 73/239/EEC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'InvestmentFirm',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'F=Investment firm in accordance with Directive 2004/39/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Manufacturing',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 3 =Manufacturing. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Mining',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 2 = Mining and quarrying. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NonFinancial',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'The identification of Non Financial Entity can be determined by the entityClassification Coding scheme under reportingRegime/entityClassification'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'OtherServices',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 19 = Other service activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ProfessionalScientificTechnical',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 13 = Professional, scientific and technical activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PublicAdminDefenceSocialSecurity',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 15 = Public administration and defence; compulsory social security. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RealEstate',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 12 = Real estate activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ReinsuranceUndertaking',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'R=Reinsurance undertaking authorised in accordance with Directive 2005/68/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TransportationStorage',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 8 = Transportation and storage. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UCITS',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        'U=UCITS and its management company, authorised in accordance with Directive 2009/65/EC;'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WaterSewerWasteManagement',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 5 = Water supply, sewerage, waste management and remediation activities. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_regulatory_corporate_sectors_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'WholesaleRetailTradeMotorRepair',
        1,
        'FPML_REGULATORY_CORPORATE_SECTOR',
        'FpML',
        '(Non Financial) - 7 = Wholesale and retail trade, repair of motor vehicles and motorcycles. Classification as defined in Regulation (EC) No 1893/2006 and ESMA/2015/1645.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_regulatory_corporate_sectors_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_regulatory_corporate_sectors_artefact' as entity, count(*) as count
from metadata.dq_regulatory_corporate_sectors_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_regulatory_corporate_sectors_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
