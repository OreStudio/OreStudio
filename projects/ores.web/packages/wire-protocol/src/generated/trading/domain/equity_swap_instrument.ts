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
 * The equity swap instrument wire shape.
 *
 * Field names are the C++ member names, because they are the keys the codec
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface EquitySwapInstrument {
    identity: InstrumentIdentity;
    underlying_name: string;
    currency: string;
    notional: string;
    return_type: string;
    start_date: string;
    maturity_date: string;
    long_short: string;
    payment_frequency_code: string;
    fixed_rate: string | null;
    floating_index: string | null;
    floating_spread: string | null;
    floating_day_count_fraction: string | null;
    initial_fixed_rate: string | null;
    knock_in_level: string | null;
    bermudan_knock_in: boolean | null;
    accruing_fixed_coupons: boolean | null;
    accumulating_fixed_coupons: boolean | null;
    description: string;
    audit: AuditRecord;
}
