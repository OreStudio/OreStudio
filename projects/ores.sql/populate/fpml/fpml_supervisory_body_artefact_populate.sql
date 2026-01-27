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
 * DQ Artefact FpML Supervisory Body Population Script
 *
 * Populates the dq_supervisory_bodies_artefact_tbl with reference data.
 * Dataset: fpml.supervisory_body
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_supervisory_bodies_fn() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Supervisory Body
-- =============================================================================

\echo '--- DQ Artefact FpML Supervisory Body ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.supervisory_body'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.supervisory_body not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_supervisory_bodies_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ASIC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Australian Securities and Investments Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BankOfRussia',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Central Bank of the Russian Federation'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.AB.ASC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Alberta Securities Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.BC.BCSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'British Columbia Securities Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.MB.MSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'The Manitoba Securities Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NB.FCSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial and Consumer Services Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NL.DSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Deputy Superintendent of Securities, Service Newfoundland and Labrador'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NS.NSSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Nova Scotia Securities Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NT.NTSO',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Northwest Territories Securities Office'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.NU.NSO',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Nunavut Securities Office, Government of Nunavut'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.ON.OSC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Ontario Securities Commission'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.PEI.OSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Office of the Superintendent of Securities'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.QC.AMF',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Autorite des marches financiers'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.SK.FCAA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial and Consumer Affairs Authority of Saskatchewan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CA.YT.OSS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Office of the Superintendent of Securities'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CFTC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Commodity Futures Trading Commission (US)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESMA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'European Securities and Markets Authority (European Union)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FCA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Financial Conduct Authority (UK)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'Fed',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Federal Reserve (US)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HKMA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Hong Kong Monetary Authority (China)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JFSA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Japan Financial Services Authority (Japan)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MAS',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'The Monetary Authority of Singapore'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ODRF',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'OTC Derivatives Regulators Forum'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SEC',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Securities and Exchange Commission (US)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_supervisory_bodies_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UKFSA',
        1,
        'FPML_SUPERVISORY_BODY',
        'FpML',
        'Deprecated usage: FCA replaces UKFSA'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_supervisory_bodies_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_supervisory_bodies_artefact' as entity, count(*) as count
from metadata.dq_supervisory_bodies_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_supervisory_bodies_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
