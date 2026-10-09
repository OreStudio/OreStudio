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
import type { Folder } from '../domain/folder.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FolderKey {
    id: string;
}

export interface FolderWrite {
    id: string;
    parent_id: string | null;
    name: string;
    kind: string;
    collection_id: string | null;
}

export interface FolderChange {
    write: FolderWrite;
    precondition: Precondition;
}

export interface FolderRemoval {
    key: FolderKey;
    precondition: Precondition;
}

export interface FolderLookup {
    key: FolderKey;
    folder: Folder | null;
}

export interface FoldersFilter {
    id_one_of: string[] | null;
}

export interface FolderEvent {
    event_id: string;
    key: FolderKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FolderVersionKey {
    folder: FolderKey;
    version: number;
}

export interface FolderVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFoldersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FoldersFilter | null;
    as_of: string | null;
}

export interface ListFoldersResponse {
    result: Result;
    folders: Folder[];
    total: number;
}

export interface GetFolderRequest {
    key: FolderKey;
}

export interface GetFolderResponse {
    result: Result;
    folder: Folder | null;
}

export interface GetManyFoldersRequest {
    keys: FolderKey[];
}

export interface GetManyFoldersResponse {
    result: Result;
    entries: FolderLookup[];
}

export interface PutFolderRequest {
    change: FolderChange;
    intent: ChangeIntent;
}

export interface PutFolderResponse {
    result: Result;
    folder: Folder | null;
}

export interface PutManyFoldersRequest {
    changes: FolderChange[];
    intent: ChangeIntent;
}

export interface PutManyFoldersResponse {
    result: Result;
    folders: Folder[];
}

export interface DeleteFolderRequest {
    removal: FolderRemoval;
    intent: ChangeIntent;
}

export interface DeleteFolderResponse {
    result: Result;
}

export interface DeleteManyFoldersRequest {
    removals: FolderRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFoldersResponse {
    result: Result;
}

export interface ListFolderVersionsRequest {
    key: FolderKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FolderVersionsFilter | null;
}

export interface ListFolderVersionsResponse {
    result: Result;
    versions: Folder[];
    total: number;
}

export interface GetFolderVersionRequest {
    key: FolderVersionKey;
}

export interface GetFolderVersionResponse {
    result: Result;
    version: Folder | null;
}

export const subjects = {
    list_folders_request: 'synthetic.v1.folders.list',
    get_folder_request: 'synthetic.v1.folders.get',
    get_many_folders_request: 'synthetic.v1.folders.get_many',
    put_folder_request: 'synthetic.v1.folders.put',
    put_many_folders_request: 'synthetic.v1.folders.put_many',
    delete_folder_request: 'synthetic.v1.folders.delete',
    delete_many_folders_request: 'synthetic.v1.folders.delete_many',
    list_folder_versions_request: 'synthetic.v1.folders_versions.list',
    get_folder_version_request: 'synthetic.v1.folders_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_folders_request: true,
    get_folder_request: true,
    get_many_folders_request: true,
    put_folder_request: true,
    put_many_folders_request: true,
    delete_folder_request: true,
    delete_many_folders_request: true,
    list_folder_versions_request: true,
    get_folder_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.folders_events.created',
    updated: 'synthetic.v1.folders_events.updated',
    deleted: 'synthetic.v1.folders_events.deleted',
} as const;
