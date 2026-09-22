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
import type { BusinessCentre } from '../domain/business_centre.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BusinessCentreKey {
    code: string;
}

export interface BusinessCentreWrite {
    code: string;
    source: string;
    description: string;
    city_name: string;
    country_alpha2_code: string;
    coding_scheme_code: string;
}

export interface BusinessCentreChange {
    write: BusinessCentreWrite;
    precondition: Precondition;
}

export interface BusinessCentreRemoval {
    key: BusinessCentreKey;
    precondition: Precondition;
}

export interface BusinessCentreLookup {
    key: BusinessCentreKey;
    business_centre: BusinessCentre | null;
}

export interface BusinessCentreEvent {
    event_id: string;
    key: BusinessCentreKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BusinessCentreVersionKey {
    business_centre: BusinessCentreKey;
    version: number;
}

export interface BusinessCentreVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBusinessCentresRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBusinessCentresResponse {
    result: Result;
    centres: BusinessCentre[];
    total: number;
}

export interface GetBusinessCentreRequest {
    key: BusinessCentreKey;
}

export interface GetBusinessCentreResponse {
    result: Result;
    business_centre: BusinessCentre | null;
}

export interface GetManyBusinessCentresRequest {
    keys: BusinessCentreKey[];
}

export interface GetManyBusinessCentresResponse {
    result: Result;
    entries: BusinessCentreLookup[];
}

export interface PutBusinessCentreRequest {
    change: BusinessCentreChange;
    intent: ChangeIntent;
}

export interface PutBusinessCentreResponse {
    result: Result;
    business_centre: BusinessCentre;
}

export interface PutManyBusinessCentresRequest {
    changes: BusinessCentreChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessCentresResponse {
    result: Result;
    centres: BusinessCentre[];
}

export interface DeleteBusinessCentreRequest {
    removal: BusinessCentreRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessCentreResponse {
    result: Result;
}

export interface DeleteManyBusinessCentresRequest {
    removals: BusinessCentreRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessCentresResponse {
    result: Result;
}

export interface ListBusinessCentreVersionsRequest {
    key: BusinessCentreKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessCentreVersionsFilter | null;
}

export interface ListBusinessCentreVersionsResponse {
    result: Result;
    versions: BusinessCentre[];
    total: number;
}

export interface GetBusinessCentreVersionRequest {
    key: BusinessCentreVersionKey;
}

export interface GetBusinessCentreVersionResponse {
    result: Result;
    version: BusinessCentre;
}

export const subjects = {
    list_business_centres_request: "refdata.v1.business_centres.list",
    get_business_centre_request: "refdata.v1.business_centres.get",
    get_many_business_centres_request: "refdata.v1.business_centres.get_many",
    put_business_centre_request: "refdata.v1.business_centres.put",
    put_many_business_centres_request: "refdata.v1.business_centres.put_many",
    delete_business_centre_request: "refdata.v1.business_centres.delete",
    delete_many_business_centres_request: "refdata.v1.business_centres.delete_many",
    list_business_centre_versions_request: "refdata.v1.business_centres_versions.list",
    get_business_centre_version_request: "refdata.v1.business_centres_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_centres_request: true,
    get_business_centre_request: true,
    get_many_business_centres_request: true,
    put_business_centre_request: true,
    put_many_business_centres_request: true,
    delete_business_centre_request: true,
    delete_many_business_centres_request: true,
    list_business_centre_versions_request: true,
    get_business_centre_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.business_centres_events.created",
    updated: "refdata.v1.business_centres_events.updated",
    deleted: "refdata.v1.business_centres_events.deleted",
} as const;
