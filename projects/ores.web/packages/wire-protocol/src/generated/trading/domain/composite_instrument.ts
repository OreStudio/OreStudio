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
 * The composite instrument wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CompositeInstrument {
    identity: InstrumentIdentity;
    description: string;
    currency: string | null;
    notional_calculation: string | null;
    notional_override: string | null;
    portfolio_basket: string | null;
    index_quantity: string | null;
    return_payer: string | null;
    return_currency: string | null;
    return_observation_lag: string | null;
    return_observation_convention: string | null;
    return_observation_calendar: string | null;
    return_payment_lag: string | null;
    return_payment_convention: string | null;
    return_payment_calendar: string | null;
    return_initial_price: string | null;
    return_initial_price_currency: string | null;
    return_fx_index: string | null;
    return_pay_underlying_cash_flows_immediately: string | null;
    funding_notional_types: string | null;
    underlying_basket_name: string | null;
    underlying_index_quantity: string | null;
    underlying_price_is_per_unit: string | null;
    audit: AuditRecord;
}
