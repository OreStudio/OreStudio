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
import type { CounterpartyContactInformation } from '../domain/counterparty_contact_information.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CounterpartyContactInformationKey {
    contact_type: string;
}

export interface CounterpartyContactInformationWrite {
    id: string;
    counterparty_id: string;
    contact_type: string;
    street_line_1: string;
    street_line_2: string;
    city: string;
    state: string;
    country_code: string;
    postal_code: string;
    phone: string;
    email: string;
    web_page: string;
}

export interface CounterpartyContactInformationChange {
    write: CounterpartyContactInformationWrite;
    precondition: Precondition;
}

export interface CounterpartyContactInformationRemoval {
    key: CounterpartyContactInformationKey;
    precondition: Precondition;
}

export interface CounterpartyContactInformationLookup {
    key: CounterpartyContactInformationKey;
    counterparty_contact_information: CounterpartyContactInformation | null;
}

export interface CounterpartyContactInformationsFilter {
    counterparty_id: string | null;
}

export interface CounterpartyContactInformationEvent {
    event_id: string;
    key: CounterpartyContactInformationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CounterpartyContactInformationVersionKey {
    counterparty_contact_information: CounterpartyContactInformationKey;
    version: number;
}

export interface CounterpartyContactInformationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCounterpartyContactInformationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyContactInformationsFilter | null;
}

export interface ListCounterpartyContactInformationsResponse {
    result: Result;
    counterparty_contact_informations: CounterpartyContactInformation[];
    total: number;
}

export interface GetCounterpartyContactInformationRequest {
    key: CounterpartyContactInformationKey;
}

export interface GetCounterpartyContactInformationResponse {
    result: Result;
    counterparty_contact_information: CounterpartyContactInformation | null;
}

export interface GetManyCounterpartyContactInformationsRequest {
    keys: CounterpartyContactInformationKey[];
}

export interface GetManyCounterpartyContactInformationsResponse {
    result: Result;
    entries: CounterpartyContactInformationLookup[];
}

export interface PutCounterpartyContactInformationRequest {
    change: CounterpartyContactInformationChange;
    intent: ChangeIntent;
}

export interface PutCounterpartyContactInformationResponse {
    result: Result;
    counterparty_contact_information: CounterpartyContactInformation;
}

export interface PutManyCounterpartyContactInformationsRequest {
    changes: CounterpartyContactInformationChange[];
    intent: ChangeIntent;
}

export interface PutManyCounterpartyContactInformationsResponse {
    result: Result;
    counterparty_contact_informations: CounterpartyContactInformation[];
}

export interface DeleteCounterpartyContactInformationRequest {
    removal: CounterpartyContactInformationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCounterpartyContactInformationResponse {
    result: Result;
}

export interface DeleteManyCounterpartyContactInformationsRequest {
    removals: CounterpartyContactInformationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCounterpartyContactInformationsResponse {
    result: Result;
}

export interface ListByCounterpartyIdCounterpartyContactInformationsRequest {
    counterparty_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyContactInformationsFilter | null;
}

export interface ListByCounterpartyIdCounterpartyContactInformationsResponse {
    result: Result;
    counterparty_contact_informations: CounterpartyContactInformation[];
    total: number;
}

export interface ListCounterpartyContactInformationVersionsRequest {
    key: CounterpartyContactInformationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyContactInformationVersionsFilter | null;
}

export interface ListCounterpartyContactInformationVersionsResponse {
    result: Result;
    versions: CounterpartyContactInformation[];
    total: number;
}

export interface GetCounterpartyContactInformationVersionRequest {
    key: CounterpartyContactInformationVersionKey;
}

export interface GetCounterpartyContactInformationVersionResponse {
    result: Result;
    version: CounterpartyContactInformation;
}

export const subjects = {
    list_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.list",
    get_counterparty_contact_information_request: "refdata.v1.counterparty_contact_informations.get",
    get_many_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.get_many",
    put_counterparty_contact_information_request: "refdata.v1.counterparty_contact_informations.put",
    put_many_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.put_many",
    delete_counterparty_contact_information_request: "refdata.v1.counterparty_contact_informations.delete",
    delete_many_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.delete_many",
    list_by_counterparty_id_counterparty_contact_informations_request: "refdata.v1.counterparty_contact_informations.list_by_counterparty_id",
    list_counterparty_contact_information_versions_request: "refdata.v1.counterparty_contact_informations_versions.list",
    get_counterparty_contact_information_version_request: "refdata.v1.counterparty_contact_informations_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_contact_informations_request: true,
    get_counterparty_contact_information_request: true,
    get_many_counterparty_contact_informations_request: true,
    put_counterparty_contact_information_request: true,
    put_many_counterparty_contact_informations_request: true,
    delete_counterparty_contact_information_request: true,
    delete_many_counterparty_contact_informations_request: true,
    list_by_counterparty_id_counterparty_contact_informations_request: true,
    list_counterparty_contact_information_versions_request: true,
    get_counterparty_contact_information_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.counterparty_contact_informations_events.created",
    updated: "refdata.v1.counterparty_contact_informations_events.updated",
    deleted: "refdata.v1.counterparty_contact_informations_events.deleted",
} as const;
