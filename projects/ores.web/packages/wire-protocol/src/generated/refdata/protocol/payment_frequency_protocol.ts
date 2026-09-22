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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PaymentFrequencyKey {
    code: string;
}

export interface PaymentFrequencyWrite {
    code: string;
    name: string;
    description: string;
    period_unit: string;
    period_multiplier: number | null;
    display_order: number;
}

export interface PaymentFrequencyChange {
    write: PaymentFrequencyWrite;
    precondition: Precondition;
}

export interface PaymentFrequencyRemoval {
    key: PaymentFrequencyKey;
    precondition: Precondition;
}

export interface PaymentFrequencyLookup {
    key: PaymentFrequencyKey;
    payment_frequency: PaymentFrequency | null;
}

export interface PaymentFrequencyEvent {
    event_id: string;
    key: PaymentFrequencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PaymentFrequencyVersionKey {
    payment_frequency: PaymentFrequencyKey;
    version: number;
}

export interface PaymentFrequencyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPaymentFrequenciesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPaymentFrequenciesResponse {
    result: Result;
    payment_frequencies: PaymentFrequency[];
    total: number;
}

export interface GetPaymentFrequencyRequest {
    key: PaymentFrequencyKey;
}

export interface GetPaymentFrequencyResponse {
    result: Result;
    payment_frequency: PaymentFrequency | null;
}

export interface GetManyPaymentFrequenciesRequest {
    keys: PaymentFrequencyKey[];
}

export interface GetManyPaymentFrequenciesResponse {
    result: Result;
    entries: PaymentFrequencyLookup[];
}

export interface PutPaymentFrequencyRequest {
    change: PaymentFrequencyChange;
    intent: ChangeIntent;
}

export interface PutPaymentFrequencyResponse {
    result: Result;
    payment_frequency: PaymentFrequency;
}

export interface PutManyPaymentFrequenciesRequest {
    changes: PaymentFrequencyChange[];
    intent: ChangeIntent;
}

export interface PutManyPaymentFrequenciesResponse {
    result: Result;
    payment_frequencies: PaymentFrequency[];
}

export interface DeletePaymentFrequencyRequest {
    removal: PaymentFrequencyRemoval;
    intent: ChangeIntent;
}

export interface DeletePaymentFrequencyResponse {
    result: Result;
}

export interface DeleteManyPaymentFrequenciesRequest {
    removals: PaymentFrequencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPaymentFrequenciesResponse {
    result: Result;
}

export interface ListPaymentFrequencyVersionsRequest {
    key: PaymentFrequencyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PaymentFrequencyVersionsFilter | null;
}

export interface ListPaymentFrequencyVersionsResponse {
    result: Result;
    versions: PaymentFrequency[];
    total: number;
}

export interface GetPaymentFrequencyVersionRequest {
    key: PaymentFrequencyVersionKey;
}

export interface GetPaymentFrequencyVersionResponse {
    result: Result;
    version: PaymentFrequency;
}

export const subjects = {
    list_payment_frequencies_request: "refdata.v1.payment_frequencies.list",
    get_payment_frequency_request: "refdata.v1.payment_frequencies.get",
    get_many_payment_frequencies_request: "refdata.v1.payment_frequencies.get_many",
    put_payment_frequency_request: "refdata.v1.payment_frequencies.put",
    put_many_payment_frequencies_request: "refdata.v1.payment_frequencies.put_many",
    delete_payment_frequency_request: "refdata.v1.payment_frequencies.delete",
    delete_many_payment_frequencies_request: "refdata.v1.payment_frequencies.delete_many",
    list_payment_frequency_versions_request: "refdata.v1.payment_frequencies_versions.list",
    get_payment_frequency_version_request: "refdata.v1.payment_frequencies_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_payment_frequencies_request: true,
    get_payment_frequency_request: true,
    get_many_payment_frequencies_request: true,
    put_payment_frequency_request: true,
    put_many_payment_frequencies_request: true,
    delete_payment_frequency_request: true,
    delete_many_payment_frequencies_request: true,
    list_payment_frequency_versions_request: true,
    get_payment_frequency_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.payment_frequencies_events.created",
    updated: "refdata.v1.payment_frequencies_events.updated",
    deleted: "refdata.v1.payment_frequencies_events.deleted",
} as const;
