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
 * The FX forward instrument wire shape.
 *
 * Field names are the C++ member names, because they are the keys the codec
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface FxForwardInstrument {
    identity: InstrumentIdentity;
    bought_currency: string;
    bought_amount: string;
    sold_currency: string;
    sold_amount: string;
    value_date: string;
    settlement: string;
    settlement_currency: string | null;
    settlement_fx_index: string | null;
    settlement_date: string | null;
    settlement_payment_lag: string | null;
    settlement_payment_calendar: string | null;
    settlement_payment_convention: string | null;
    far_date: string | null;
    far_bought_amount: string | null;
    far_sold_amount: string | null;
    description: string;
    audit: AuditRecord;
}
