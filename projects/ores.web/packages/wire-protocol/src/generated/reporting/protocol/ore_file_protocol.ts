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
import type { OreFile } from '../domain/ore_file.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface OreFileKey {
    code: string;
}

export interface OreFileWrite {
    code: string;
    name: string;
    category_code: string;
    configuration_type_code: string | null;
    guide_section: string | null;
    origin: string;
    format: string;
    schema_ore_file_code: string | null;
    typical_filename: string;
    filename_regex: string;
    description: string;
    display_order: number;
}

export interface OreFileChange {
    write: OreFileWrite;
    precondition: Precondition;
}

export interface OreFileRemoval {
    key: OreFileKey;
    precondition: Precondition;
}

export interface OreFileLookup {
    key: OreFileKey;
    ore_file: OreFile | null;
}

export interface OreFilesFilter {
    category_code: string | null;
    configuration_type_code: string | null | null;
    schema_ore_file_code: string | null | null;
    code_one_of: string[] | null;
    category_code_one_of: string[] | null;
    configuration_type_code_one_of: string[] | null;
    schema_ore_file_code_one_of: string[] | null;
}

export interface OreFileEvent {
    event_id: string;
    key: OreFileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreFilesVersionKey {
    ore_file: OreFileKey;
    version: number;
}

export interface OreFilesVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreFilesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreFilesFilter | null;
    as_of: string | null;
}

export interface ListOreFilesResponse {
    result: Result;
    files: OreFile[];
    total: number;
}

export interface GetOreFileRequest {
    key: OreFileKey;
}

export interface GetOreFileResponse {
    result: Result;
    ore_file: OreFile | null;
}

export interface GetManyOreFilesRequest {
    keys: OreFileKey[];
}

export interface GetManyOreFilesResponse {
    result: Result;
    entries: OreFileLookup[];
}

export interface PutOreFileRequest {
    change: OreFileChange;
    intent: ChangeIntent;
}

export interface PutOreFileResponse {
    result: Result;
    ore_file: OreFile | null;
}

export interface PutManyOreFilesRequest {
    changes: OreFileChange[];
    intent: ChangeIntent;
}

export interface PutManyOreFilesResponse {
    result: Result;
    files: OreFile[];
}

export interface DeleteOreFileRequest {
    removal: OreFileRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreFileResponse {
    result: Result;
}

export interface DeleteManyOreFilesRequest {
    removals: OreFileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreFilesResponse {
    result: Result;
}

export interface ListByCategoryCodeOreFilesRequest {
    category_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFilesFilter | null;
}

export interface ListByCategoryCodeOreFilesResponse {
    result: Result;
    files: OreFile[];
    total: number;
}

export interface ListByConfigurationTypeCodeOreFilesRequest {
    configuration_type_code: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFilesFilter | null;
}

export interface ListByConfigurationTypeCodeOreFilesResponse {
    result: Result;
    files: OreFile[];
    total: number;
}

export interface ListBySchemaOreFileCodeOreFilesRequest {
    schema_ore_file_code: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFilesFilter | null;
}

export interface ListBySchemaOreFileCodeOreFilesResponse {
    result: Result;
    files: OreFile[];
    total: number;
}

export interface ListOreFilesVersionsRequest {
    key: OreFileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFilesVersionsFilter | null;
}

export interface ListOreFilesVersionsResponse {
    result: Result;
    versions: OreFile[];
    total: number;
}

export interface GetOreFilesVersionRequest {
    key: OreFilesVersionKey;
}

export interface GetOreFilesVersionResponse {
    result: Result;
    version: OreFile | null;
}

export const subjects = {
    list_ore_files_request: 'reporting.v1.ore_files.list',
    get_ore_file_request: 'reporting.v1.ore_files.get',
    get_many_ore_files_request: 'reporting.v1.ore_files.get_many',
    put_ore_file_request: 'reporting.v1.ore_files.put',
    put_many_ore_files_request: 'reporting.v1.ore_files.put_many',
    delete_ore_file_request: 'reporting.v1.ore_files.delete',
    delete_many_ore_files_request: 'reporting.v1.ore_files.delete_many',
    list_by_category_code_ore_files_request: 'reporting.v1.ore_files.list_by_category_code',
    list_by_configuration_type_code_ore_files_request:
        'reporting.v1.ore_files.list_by_configuration_type_code',
    list_by_schema_ore_file_code_ore_files_request:
        'reporting.v1.ore_files.list_by_schema_ore_file_code',
    list_ore_files_versions_request: 'reporting.v1.ore_files_versions.list',
    get_ore_files_version_request: 'reporting.v1.ore_files_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_files_request: true,
    get_ore_file_request: true,
    get_many_ore_files_request: true,
    put_ore_file_request: true,
    put_many_ore_files_request: true,
    delete_ore_file_request: true,
    delete_many_ore_files_request: true,
    list_by_category_code_ore_files_request: true,
    list_by_configuration_type_code_ore_files_request: true,
    list_by_schema_ore_file_code_ore_files_request: true,
    list_ore_files_versions_request: true,
    get_ore_files_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.events.ore_files.created',
    updated: 'reporting.v1.events.ore_files.updated',
    deleted: 'reporting.v1.events.ore_files.deleted',
} as const;
