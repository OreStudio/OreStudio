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
import type { RegulatoryCorporateSector } from '../domain/regulatory_corporate_sector.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RegulatoryCorporateSectorKey {
    code: string;
    coding_scheme_code: string;
}

export interface RegulatoryCorporateSectorWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface RegulatoryCorporateSectorChange {
    write: RegulatoryCorporateSectorWrite;
    precondition: Precondition;
}

export interface RegulatoryCorporateSectorRemoval {
    key: RegulatoryCorporateSectorKey;
    precondition: Precondition;
}

export interface RegulatoryCorporateSectorLookup {
    key: RegulatoryCorporateSectorKey;
    regulatory_corporate_sector: RegulatoryCorporateSector | null;
}

export interface RegulatoryCorporateSectorEvent {
    event_id: string;
    key: RegulatoryCorporateSectorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RegulatoryCorporateSectorVersionKey {
    regulatory_corporate_sector: RegulatoryCorporateSectorKey;
    version: number;
}

export interface RegulatoryCorporateSectorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRegulatoryCorporateSectorsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListRegulatoryCorporateSectorsResponse {
    result: Result;
    sectors: RegulatoryCorporateSector[];
    total: number;
}

export interface GetRegulatoryCorporateSectorRequest {
    key: RegulatoryCorporateSectorKey;
}

export interface GetRegulatoryCorporateSectorResponse {
    result: Result;
    regulatory_corporate_sector: RegulatoryCorporateSector | null;
}

export interface GetManyRegulatoryCorporateSectorsRequest {
    keys: RegulatoryCorporateSectorKey[];
}

export interface GetManyRegulatoryCorporateSectorsResponse {
    result: Result;
    entries: RegulatoryCorporateSectorLookup[];
}

export interface PutRegulatoryCorporateSectorRequest {
    change: RegulatoryCorporateSectorChange;
    intent: ChangeIntent;
}

export interface PutRegulatoryCorporateSectorResponse {
    result: Result;
    regulatory_corporate_sector: RegulatoryCorporateSector | null;
}

export interface PutManyRegulatoryCorporateSectorsRequest {
    changes: RegulatoryCorporateSectorChange[];
    intent: ChangeIntent;
}

export interface PutManyRegulatoryCorporateSectorsResponse {
    result: Result;
    sectors: RegulatoryCorporateSector[];
}

export interface DeleteRegulatoryCorporateSectorRequest {
    removal: RegulatoryCorporateSectorRemoval;
    intent: ChangeIntent;
}

export interface DeleteRegulatoryCorporateSectorResponse {
    result: Result;
}

export interface DeleteManyRegulatoryCorporateSectorsRequest {
    removals: RegulatoryCorporateSectorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRegulatoryCorporateSectorsResponse {
    result: Result;
}

export interface ListRegulatoryCorporateSectorVersionsRequest {
    key: RegulatoryCorporateSectorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RegulatoryCorporateSectorVersionsFilter | null;
}

export interface ListRegulatoryCorporateSectorVersionsResponse {
    result: Result;
    versions: RegulatoryCorporateSector[];
    total: number;
}

export interface GetRegulatoryCorporateSectorVersionRequest {
    key: RegulatoryCorporateSectorVersionKey;
}

export interface GetRegulatoryCorporateSectorVersionResponse {
    result: Result;
    version: RegulatoryCorporateSector | null;
}

export const subjects = {
    list_regulatory_corporate_sectors_request: 'refdata.v1.regulatory_corporate_sectors.list',
    get_regulatory_corporate_sector_request: 'refdata.v1.regulatory_corporate_sectors.get',
    get_many_regulatory_corporate_sectors_request:
        'refdata.v1.regulatory_corporate_sectors.get_many',
    put_regulatory_corporate_sector_request: 'refdata.v1.regulatory_corporate_sectors.put',
    put_many_regulatory_corporate_sectors_request:
        'refdata.v1.regulatory_corporate_sectors.put_many',
    delete_regulatory_corporate_sector_request: 'refdata.v1.regulatory_corporate_sectors.delete',
    delete_many_regulatory_corporate_sectors_request:
        'refdata.v1.regulatory_corporate_sectors.delete_many',
    list_regulatory_corporate_sector_versions_request:
        'refdata.v1.regulatory_corporate_sectors_versions.list',
    get_regulatory_corporate_sector_version_request:
        'refdata.v1.regulatory_corporate_sectors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_regulatory_corporate_sectors_request: true,
    get_regulatory_corporate_sector_request: true,
    get_many_regulatory_corporate_sectors_request: true,
    put_regulatory_corporate_sector_request: true,
    put_many_regulatory_corporate_sectors_request: true,
    delete_regulatory_corporate_sector_request: true,
    delete_many_regulatory_corporate_sectors_request: true,
    list_regulatory_corporate_sector_versions_request: true,
    get_regulatory_corporate_sector_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.regulatory_corporate_sectors_events.created',
    updated: 'refdata.v1.regulatory_corporate_sectors_events.updated',
    deleted: 'refdata.v1.regulatory_corporate_sectors_events.deleted',
} as const;
