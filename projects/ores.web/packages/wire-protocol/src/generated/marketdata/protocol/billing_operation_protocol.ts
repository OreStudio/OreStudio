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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief One component of a rate card, the quantity it counted and what it costs.
 */
export interface BillLineView {
    driver: string;
    licence_right: string;
    quantity: number;
    charge_micros: number;
    total_micros: number;
}

/**
 * @brief The bill of a source.
 *
 * A shadow bill is the bill of a simulated source. It has the same lines and the same amounts as a bill
 * of a live source, and nobody pays it.
 */
export interface SourceBillView {
    feed_id: string;
    source_name: string;
    currency: string;
    shadow: boolean;
    lines: BillLineView[];
    total_micros: number;
}

/**
 * @brief The charge of a party for a component of a source.
 */
export interface PartyChargeView {
    tenant_id: string;
    party_id: string;
    feed_id: string;
    driver: string;
    licence_right: string;
    total_micros: number;
}

/**
 * @brief Asks for the bills of a half open period.
 *
 * A period of no length, or one that ends before it starts, is refused.
 */
export interface ComputeBillRequest {
    period_from: string;
    period_to: string;
    source_name: string;
}

/**
 * @brief The source bills, the charge of each party and the part that no party bears.
 */
export interface ComputeBillResponse {
    source_bills: SourceBillView[];
    party_charges: PartyChargeView[];
    unallocated_micros: number;
    success: boolean;
    message: string;
}

export const subjects = {
    compute_bill_request: 'marketdata.v1.ops.compute_bill',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    compute_bill_request: true,
} as const;
