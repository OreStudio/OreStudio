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
import type { CurrencyPairClassification } from '../domain/currency_pair_classification.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyPairClassificationKey {
    code: string;
}

export interface CurrencyPairClassificationWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CurrencyPairClassificationChange {
    write: CurrencyPairClassificationWrite;
    precondition: Precondition;
}

export interface CurrencyPairClassificationRemoval {
    key: CurrencyPairClassificationKey;
    precondition: Precondition;
}

export interface CurrencyPairClassificationLookup {
    key: CurrencyPairClassificationKey;
    currency_pair_classification: CurrencyPairClassification | null;
}

export interface CurrencyPairClassificationEvent {
    event_id: string;
    key: CurrencyPairClassificationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyPairClassificationVersionKey {
    currency_pair_classification: CurrencyPairClassificationKey;
    version: number;
}

export interface CurrencyPairClassificationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrencyPairClassificationsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurrencyPairClassificationsResponse {
    result: Result;
    classifications: CurrencyPairClassification[];
    total: number;
}

export interface GetCurrencyPairClassificationRequest {
    key: CurrencyPairClassificationKey;
}

export interface GetCurrencyPairClassificationResponse {
    result: Result;
    currency_pair_classification: CurrencyPairClassification | null;
}

export interface GetManyCurrencyPairClassificationsRequest {
    keys: CurrencyPairClassificationKey[];
}

export interface GetManyCurrencyPairClassificationsResponse {
    result: Result;
    entries: CurrencyPairClassificationLookup[];
}

export interface PutCurrencyPairClassificationRequest {
    change: CurrencyPairClassificationChange;
    intent: ChangeIntent;
}

export interface PutCurrencyPairClassificationResponse {
    result: Result;
    currency_pair_classification: CurrencyPairClassification;
}

export interface PutManyCurrencyPairClassificationsRequest {
    changes: CurrencyPairClassificationChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyPairClassificationsResponse {
    result: Result;
    classifications: CurrencyPairClassification[];
}

export interface DeleteCurrencyPairClassificationRequest {
    removal: CurrencyPairClassificationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyPairClassificationResponse {
    result: Result;
}

export interface DeleteManyCurrencyPairClassificationsRequest {
    removals: CurrencyPairClassificationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyPairClassificationsResponse {
    result: Result;
}

export interface ListCurrencyPairClassificationVersionsRequest {
    key: CurrencyPairClassificationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyPairClassificationVersionsFilter | null;
}

export interface ListCurrencyPairClassificationVersionsResponse {
    result: Result;
    versions: CurrencyPairClassification[];
    total: number;
}

export interface GetCurrencyPairClassificationVersionRequest {
    key: CurrencyPairClassificationVersionKey;
}

export interface GetCurrencyPairClassificationVersionResponse {
    result: Result;
    version: CurrencyPairClassification;
}

export const subjects = {
    list_currency_pair_classifications_request: "refdata.v1.currency_pair_classifications.list",
    get_currency_pair_classification_request: "refdata.v1.currency_pair_classifications.get",
    get_many_currency_pair_classifications_request: "refdata.v1.currency_pair_classifications.get_many",
    put_currency_pair_classification_request: "refdata.v1.currency_pair_classifications.put",
    put_many_currency_pair_classifications_request: "refdata.v1.currency_pair_classifications.put_many",
    delete_currency_pair_classification_request: "refdata.v1.currency_pair_classifications.delete",
    delete_many_currency_pair_classifications_request: "refdata.v1.currency_pair_classifications.delete_many",
    list_currency_pair_classification_versions_request: "refdata.v1.currency_pair_classifications_versions.list",
    get_currency_pair_classification_version_request: "refdata.v1.currency_pair_classifications_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_pair_classifications_request: true,
    get_currency_pair_classification_request: true,
    get_many_currency_pair_classifications_request: true,
    put_currency_pair_classification_request: true,
    put_many_currency_pair_classifications_request: true,
    delete_currency_pair_classification_request: true,
    delete_many_currency_pair_classifications_request: true,
    list_currency_pair_classification_versions_request: true,
    get_currency_pair_classification_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currency_pair_classifications_events.created",
    updated: "refdata.v1.currency_pair_classifications_events.updated",
    deleted: "refdata.v1.currency_pair_classifications_events.deleted",
} as const;
