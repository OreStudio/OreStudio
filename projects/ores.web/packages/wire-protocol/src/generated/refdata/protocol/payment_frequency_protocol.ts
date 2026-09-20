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
import type { PaymentFrequency } from '../domain/payment_frequency.js';

export interface GetPaymentFrequenciesRequest {
    offset: number;
    limit: number;
}

export interface GetPaymentFrequenciesResponse {
    payment_frequencies: PaymentFrequency[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePaymentFrequencyRequest {
    data: PaymentFrequency;
}

export interface SavePaymentFrequencyResponse {
    success: boolean;
    message: string;
}

export interface DeletePaymentFrequencyRequest {
    codes: string[];
}

export interface DeletePaymentFrequencyResponse {
    success: boolean;
    message: string;
}

export interface GetPaymentFrequencyHistoryRequest {
    code: string;
}

export interface GetPaymentFrequencyHistoryResponse {
    history: PaymentFrequency[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_payment_frequencies_request: "refdata.v1.payment_frequencies.list",
    save_payment_frequency_request: "refdata.v1.payment_frequencies.save",
    delete_payment_frequency_request: "refdata.v1.payment_frequencies.delete",
    get_payment_frequency_history_request: "refdata.v1.payment_frequencies.history",
} as const;
