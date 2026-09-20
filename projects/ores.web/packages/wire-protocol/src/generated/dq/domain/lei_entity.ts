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
 * The LEI entity wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface LeiEntity {
    version: number;
    tenant_id: string;
    lei: string;
    entity_legal_name: string;
    entity_entity_category: string;
    entity_entity_sub_category: string | null;
    entity_entity_status: string;
    entity_legal_form_entity_legal_form_code: string | null;
    entity_legal_form_other_legal_form: string | null;
    entity_legal_jurisdiction: string | null;
    entity_legal_address_first_address_line: string | null;
    entity_legal_address_city: string | null;
    entity_legal_address_region: string | null;
    entity_legal_address_country: string;
    entity_legal_address_postal_code: string | null;
    entity_headquarters_address_first_address_line: string | null;
    entity_headquarters_address_city: string | null;
    entity_headquarters_address_region: string | null;
    entity_headquarters_address_country: string | null;
    entity_headquarters_address_postal_code: string | null;
    entity_entity_creation_date: string | null;
    registration_initial_registration_date: string | null;
    registration_last_update_date: string | null;
    registration_next_renewal_date: string | null;
    registration_registration_status: string | null;
    entity_transliterated_name_1: string | null;
    entity_transliterated_name_1_type: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
