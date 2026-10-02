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
import type { InstrumentIdentity } from './instrument_identity.js';
import type { AuditRecord } from '../../dq/domain/audit_record.js';
/**
 * The credit instrument wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CreditInstrument {
    identity: InstrumentIdentity;
    reference_entity: string;
    currency: string;
    notional: string;
    spread: number;
    recovery_rate: number;
    tenor: string;
    start_date: string;
    maturity_date: string;
    day_count_fraction_code: string;
    payment_frequency_code: string;
    index_name: string;
    index_series: number | null;
    seniority: string;
    restructuring: string;
    description: string;
    option_type: string;
    option_expiry_date: string | null;
    option_strike: number | null;
    linked_asset_code: string;
    tranche_attachment: number | null;
    tranche_detachment: number | null;
    audit: AuditRecord;
}
