/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The dataset wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface Dataset {
    version: number;
    tenant_id: string;
    id: string;
    code: string;
    catalog_name: string | null;
    subject_area_name: string;
    domain_name: string;
    coding_scheme_code: string | null;
    origin_code: string;
    nature_code: string;
    treatment_code: string;
    methodology_id: string | null;
    name: string;
    description: string;
    source_system_id: string;
    business_context: string;
    upstream_derivation_id: string | null;
    lineage_depth: number;
    as_of_date: string;
    ingestion_timestamp: string;
    license_info: string | null;
    artefact_type: string;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
