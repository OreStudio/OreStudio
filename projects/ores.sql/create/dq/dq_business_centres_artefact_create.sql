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
-- The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_business_centres_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "code" text not null,
    "version" integer not null,
    "source" text null,
    "description" text null
    ,"image_id" uuid
    ,"coding_scheme_code" text not null
);

create index if not exists dq_business_centres_artefact_dataset_idx
on ores_dq_business_centres_artefact_tbl (dataset_id);

create index if not exists dq_business_centres_artefact_tenant_idx
on ores_dq_business_centres_artefact_tbl (tenant_id);

create index if not exists dq_business_centres_artefact_code_idx
on ores_dq_business_centres_artefact_tbl (code);

create index if not exists dq_business_centres_artefact_coding_scheme_idx
on ores_dq_business_centres_artefact_tbl (coding_scheme_code);
