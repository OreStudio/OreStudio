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
 * The Equity Accumulator instrument wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface EquityAccumulatorInstrument {
    identity: InstrumentIdentity;
    underlying_name: string;
    currency: string;
    strike: string;
    fixing_amount: string;
    start_date: string;
    expiry_date: string;
    fixing_frequency: string;
    long_short: string;
    knock_out_level: string | null;
    target_amount: string | null;
    target_type: string;
    payoff_type: string;
    description: string;
    audit: AuditRecord;
}
