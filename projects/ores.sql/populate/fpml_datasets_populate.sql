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
 * FpML Account Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.account_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Account Type Dataset
-- =============================================================================

\echo '--- FpML Account Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.account_type',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_ACCOUNT_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Account Type',
    'Contains a code representing the type of an account, for example in a clearing or exchange model.',
    'FPML',
    'Reference data for FpML Account Type',
    current_date,
    'FpML Public License 2.0',
    'account_types'
);
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
 * FpML Asset Class Dataset Population Script
 *
 * Creates the dataset entry for fpml.asset_class.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Asset Class Dataset
-- =============================================================================

\echo '--- FpML Asset Class Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.asset_class',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_ASSET_CLASS',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Asset Class',
    'Defines a simple asset class categorization. Used for classification of the risk class of the trade.',
    'FPML',
    'Reference data for FpML Asset Class',
    current_date,
    'FpML Public License 2.0',
    'asset_classes'
);
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
 * FpML Asset Measure Dataset Population Script
 *
 * Creates the dataset entry for fpml.asset_measure.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Asset Measure Dataset
-- =============================================================================

\echo '--- FpML Asset Measure Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.asset_measure',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_ASSET_MEASURE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Asset Measure',
    'The type of measure about an asset. Used for escribing valuation, sensitivity, and risk measures.',
    'FPML',
    'Reference data for FpML Asset Measure',
    current_date,
    'FpML Public License 2.0',
    'asset_measures'
);
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
 * FpML Benchmark Rate Dataset Population Script
 *
 * Creates the dataset entry for fpml.benchmark_rate.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Benchmark Rate Dataset
-- =============================================================================

\echo '--- FpML Benchmark Rate Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.benchmark_rate',
    'FpML Standards',
    'Market Data',
    'Reference Data',
    'FPML_BENCHMARK_RATE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Benchmark Rate',
    'FpML Benchmark rates',
    'FPML',
    'Reference data for FpML Benchmark Rate',
    current_date,
    'FpML Public License 2.0',
    'benchmark_rates'
);
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
 * FpML Business Center Dataset Population Script
 *
 * Creates the dataset entry for fpml.business_center.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Business Center Dataset
-- =============================================================================

\echo '--- FpML Business Center Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.business_center',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_BUSINESS_CENTER',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Business Center',
    'The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets.',
    'FPML',
    'Reference data for FpML Business Center',
    current_date,
    'FpML Public License 2.0',
    'business_centres'
);
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
 * FpML Business Process Dataset Population Script
 *
 * Creates the dataset entry for fpml.business_process.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Business Process Dataset
-- =============================================================================

\echo '--- FpML Business Process Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.business_process',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_BUSINESS_PROCESS',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Business Process',
    'Contains a code representing the type of business process a message (e.g. a status request) applies to.',
    'FPML',
    'Reference data for FpML Business Process',
    current_date,
    'FpML Public License 2.0',
    'business_processes'
);
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
 * FpML Cashflow Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.cashflow_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Cashflow Type Dataset
-- =============================================================================

\echo '--- FpML Cashflow Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.cashflow_type',
    'FpML Standards',
    'Trading',
    'Reference Data',
    'FPML_CASHFLOW_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cashflow Type',
    'The type of cash flows associated with OTC derivatives contracts and their lifecycle events.',
    'FPML',
    'Reference data for FpML Cashflow Type',
    current_date,
    'FpML Public License 2.0',
    'cashflow_types'
);
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
 * FpML Cftc Entity Classification Dataset Population Script
 *
 * Creates the dataset entry for fpml.cftc_entity_classification.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Cftc Entity Classification Dataset
-- =============================================================================

\echo '--- FpML Cftc Entity Classification Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.cftc_entity_classification',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cftc Entity Classification',
    'Financial Entity Indicator.',
    'FPML',
    'Reference data for FpML Cftc Entity Classification',
    current_date,
    'FpML Public License 2.0',
    'entity_classifications'
);
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
 * FpML Cftc Organization Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.cftc_organization_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Cftc Organization Type Dataset
-- =============================================================================

\echo '--- FpML Cftc Organization Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.cftc_organization_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_CFTC_ORGANIZATION_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Cftc Organization Type',
    'Indicates whether a counterparty is an entity established pursuant to a U.S. federal law, including CFTC Amendments to Part 45 (2020).',
    'FPML',
    'Reference data for FpML Cftc Organization Type',
    current_date,
    'FpML Public License 2.0',
    'entity_classifications'
);
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
 * FpML Entity Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.entity_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Entity Type Dataset
-- =============================================================================

\echo '--- FpML Entity Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.entity_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_ENTITY_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Entity Type',
    'This specifies the reference entity types corresponding to a list of types defined in the ISDA First to Default documentation.',
    'FPML',
    'Reference data for FpML Entity Type',
    current_date,
    'FpML Public License 2.0',
    'entity_classifications'
);
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
 * FpML Hkma Rewrite Party Relationship Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.hkma_rewrite_party_relationship_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Hkma Rewrite Party Relationship Type Dataset
-- =============================================================================

\echo '--- FpML Hkma Rewrite Party Relationship Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.hkma_rewrite_party_relationship_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Hkma Rewrite Party Relationship Type',
    'Indicates the relationship between two parties as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.',
    'FPML',
    'Reference data for FpML Hkma Rewrite Party Relationship Type',
    current_date,
    'FpML Public License 2.0',
    'party_relationships'
);
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
 * FpML Hkma Rewrite Regulatory Corporate Sector Dataset Population Script
 *
 * Creates the dataset entry for fpml.hkma_rewrite_regulatory_corporate_sector.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Hkma Rewrite Regulatory Corporate Sector Dataset
-- =============================================================================

\echo '--- FpML Hkma Rewrite Regulatory Corporate Sector Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.hkma_rewrite_regulatory_corporate_sector',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Hkma Rewrite Regulatory Corporate Sector',
    'Defines the corporate sector under HKMA (Hong Kong Monetary Authority) Rewrite fields 190 - Nature of Counterparty 1 and 191 - Nature of Counterparty 2.',
    'FPML',
    'Reference data for FpML Hkma Rewrite Regulatory Corporate Sector',
    current_date,
    'FpML Public License 2.0',
    'regulatory_corporate_sectors'
);
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
 * FpML Local Jurisdiction Dataset Population Script
 *
 * Creates the dataset entry for fpml.local_jurisdiction.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Local Jurisdiction Dataset
-- =============================================================================

\echo '--- FpML Local Jurisdiction Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.local_jurisdiction',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_LOCAL_JURISDICTION',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Local Jurisdiction',
    'This overrides the countryScheme. Specifies the Local Jurisdiction that applies to a Transaction, for example for the purposes of defining which Local Taxes will apply.',
    'FPML',
    'Reference data for FpML Local Jurisdiction',
    current_date,
    'FpML Public License 2.0',
    'local_jurisdictions'
);
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
 * FpML Party Relationship Type Dataset Population Script
 *
 * Creates the dataset entry for fpml.party_relationship_type.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Party Relationship Type Dataset
-- =============================================================================

\echo '--- FpML Party Relationship Type Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.party_relationship_type',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Party Relationship Type',
    'A type is containing a code representing how two parties are related, e.g. Affiliated, Intragroup.',
    'FPML',
    'Reference data for FpML Party Relationship Type',
    current_date,
    'FpML Public License 2.0',
    'party_relationships'
);
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
 * FpML Party Role Dataset Population Script
 *
 * Creates the dataset entry for fpml.party_role.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Party Role Dataset
-- =============================================================================

\echo '--- FpML Party Role Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.party_role',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PARTY_ROLE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Party Role',
    'Contains a code representing a related party role. This can be extended to provide custom roles.',
    'FPML',
    'Reference data for FpML Party Role',
    current_date,
    'FpML Public License 2.0',
    'party_roles'
);
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
 * FpML Person Role Dataset Population Script
 *
 * Creates the dataset entry for fpml.person_role.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Person Role Dataset
-- =============================================================================

\echo '--- FpML Person Role Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.person_role',
    'FpML Standards',
    'Parties',
    'Reference Data',
    'FPML_PERSON_ROLE',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Person Role',
    'Indicates the role of a person in a transaction.',
    'FPML',
    'Reference data for FpML Person Role',
    current_date,
    'FpML Public License 2.0',
    'person_roles'
);
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
 * FpML Reporting Regime Dataset Population Script
 *
 * Creates the dataset entry for fpml.reporting_regime.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Reporting Regime Dataset
-- =============================================================================

\echo '--- FpML Reporting Regime Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.reporting_regime',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_REPORTING_REGIME',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Reporting Regime',
    'Contains a code representing a reporting regime under which this transaction may be reported.',
    'FPML',
    'Reference data for FpML Reporting Regime',
    current_date,
    'FpML Public License 2.0',
    'reporting_regimes'
);
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
 * FpML Supervisory Body Dataset Population Script
 *
 * Creates the dataset entry for fpml.supervisory_body.
 * This must be run before populating the artefact table.
 */

set schema 'ores';

-- =============================================================================
-- FpML Supervisory Body Dataset
-- =============================================================================

\echo '--- FpML Supervisory Body Dataset ---'

select ores.upsert_dq_datasets(
    'fpml.supervisory_body',
    'FpML Standards',
    'Regulatory',
    'Reference Data',
    'FPML_SUPERVISORY_BODY',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Supervisory Body',
    'Contains a code representing a supervisory-body that may be supervising this transaction.',
    'FPML',
    'Reference data for FpML Supervisory Body',
    current_date,
    'FpML Public License 2.0',
    'supervisory_bodies'
);
