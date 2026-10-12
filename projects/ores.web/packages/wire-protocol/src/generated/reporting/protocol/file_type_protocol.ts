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
import type { FileType } from '../domain/file_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface FileTypeKey {
    code: string;
}

export interface FileTypeWrite {
    code: string;
    analytic_type_code: string | null;
    ore_parameter: string;
    kind: string;
    configuration_type_code: string | null;
    name_pattern: string;
    sample_count: number;
    sample_total: number;
    description: string;
    display_order: number;
}

export interface FileTypeChange {
    write: FileTypeWrite;
    precondition: Precondition;
}

export interface FileTypeRemoval {
    key: FileTypeKey;
    precondition: Precondition;
}

export interface FileTypeLookup {
    key: FileTypeKey;
    file_type: FileType | null;
}

export interface FileTypesFilter {
    analytic_type_code: string | null | null;
    configuration_type_code: string | null | null;
    code_one_of: string[] | null;
    analytic_type_code_one_of: string[] | null;
    configuration_type_code_one_of: string[] | null;
}

export interface FileTypeEvent {
    event_id: string;
    key: FileTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FileTypeVersionKey {
    file_type: FileTypeKey;
    version: number;
}

export interface FileTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFileTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FileTypesFilter | null;
    as_of: string | null;
}

export interface ListFileTypesResponse {
    result: Result;
    types: FileType[];
    total: number;
}

export interface GetFileTypeRequest {
    key: FileTypeKey;
}

export interface GetFileTypeResponse {
    result: Result;
    file_type: FileType | null;
}

export interface GetManyFileTypesRequest {
    keys: FileTypeKey[];
}

export interface GetManyFileTypesResponse {
    result: Result;
    entries: FileTypeLookup[];
}

export interface PutFileTypeRequest {
    change: FileTypeChange;
    intent: ChangeIntent;
}

export interface PutFileTypeResponse {
    result: Result;
    file_type: FileType | null;
}

export interface PutManyFileTypesRequest {
    changes: FileTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyFileTypesResponse {
    result: Result;
    types: FileType[];
}

export interface DeleteFileTypeRequest {
    removal: FileTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteFileTypeResponse {
    result: Result;
}

export interface DeleteManyFileTypesRequest {
    removals: FileTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFileTypesResponse {
    result: Result;
}

export interface ListByAnalyticTypeCodeFileTypesRequest {
    analytic_type_code: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: FileTypesFilter | null;
}

export interface ListByAnalyticTypeCodeFileTypesResponse {
    result: Result;
    types: FileType[];
    total: number;
}

export interface ListByConfigurationTypeCodeFileTypesRequest {
    configuration_type_code: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: FileTypesFilter | null;
}

export interface ListByConfigurationTypeCodeFileTypesResponse {
    result: Result;
    types: FileType[];
    total: number;
}

export interface ListFileTypeVersionsRequest {
    key: FileTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FileTypeVersionsFilter | null;
}

export interface ListFileTypeVersionsResponse {
    result: Result;
    versions: FileType[];
    total: number;
}

export interface GetFileTypeVersionRequest {
    key: FileTypeVersionKey;
}

export interface GetFileTypeVersionResponse {
    result: Result;
    version: FileType | null;
}

export const subjects = {
    list_file_types_request: 'reporting.v1.file_types.list',
    get_file_type_request: 'reporting.v1.file_types.get',
    get_many_file_types_request: 'reporting.v1.file_types.get_many',
    put_file_type_request: 'reporting.v1.file_types.put',
    put_many_file_types_request: 'reporting.v1.file_types.put_many',
    delete_file_type_request: 'reporting.v1.file_types.delete',
    delete_many_file_types_request: 'reporting.v1.file_types.delete_many',
    list_by_analytic_type_code_file_types_request:
        'reporting.v1.file_types.list_by_analytic_type_code',
    list_by_configuration_type_code_file_types_request:
        'reporting.v1.file_types.list_by_configuration_type_code',
    list_file_type_versions_request: 'reporting.v1.file_types_versions.list',
    get_file_type_version_request: 'reporting.v1.file_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_file_types_request: true,
    get_file_type_request: true,
    get_many_file_types_request: true,
    put_file_type_request: true,
    put_many_file_types_request: true,
    delete_file_type_request: true,
    delete_many_file_types_request: true,
    list_by_analytic_type_code_file_types_request: true,
    list_by_configuration_type_code_file_types_request: true,
    list_file_type_versions_request: true,
    get_file_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.file_types_events.created',
    updated: 'reporting.v1.file_types_events.updated',
    deleted: 'reporting.v1.file_types_events.deleted',
} as const;
