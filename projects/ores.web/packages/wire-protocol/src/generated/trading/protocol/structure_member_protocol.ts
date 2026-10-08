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
import type { StructureMember } from '../domain/structure_member.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StructureMemberKey {
    trade_id: string;
}

export interface StructureMemberWrite {
    trade_id: string;
    structure_id: string;
    role: string;
    sequence_number: number;
    counterparty_id: string;
}

export interface StructureMemberChange {
    write: StructureMemberWrite;
    precondition: Precondition;
}

export interface StructureMemberRemoval {
    key: StructureMemberKey;
    precondition: Precondition;
}

export interface StructureMemberLookup {
    key: StructureMemberKey;
    structure_member: StructureMember | null;
}

export interface StructureMembersFilter {
    trade_id_one_of: string[] | null;
}

export interface StructureMemberEvent {
    event_id: string;
    key: StructureMemberKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureMemberVersionKey {
    structure_member: StructureMemberKey;
    version: number;
}

export interface StructureMemberVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructureMembersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StructureMembersFilter | null;
    as_of: string | null;
}

export interface ListStructureMembersResponse {
    result: Result;
    members: StructureMember[];
    total: number;
}

export interface GetStructureMemberRequest {
    key: StructureMemberKey;
}

export interface GetStructureMemberResponse {
    result: Result;
    structure_member: StructureMember | null;
}

export interface GetManyStructureMembersRequest {
    keys: StructureMemberKey[];
}

export interface GetManyStructureMembersResponse {
    result: Result;
    entries: StructureMemberLookup[];
}

export interface PutStructureMemberRequest {
    change: StructureMemberChange;
    intent: ChangeIntent;
}

export interface PutStructureMemberResponse {
    result: Result;
    structure_member: StructureMember | null;
}

export interface PutManyStructureMembersRequest {
    changes: StructureMemberChange[];
    intent: ChangeIntent;
}

export interface PutManyStructureMembersResponse {
    result: Result;
    members: StructureMember[];
}

export interface DeleteStructureMemberRequest {
    removal: StructureMemberRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureMemberResponse {
    result: Result;
}

export interface DeleteManyStructureMembersRequest {
    removals: StructureMemberRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructureMembersResponse {
    result: Result;
}

export interface ListStructureMemberVersionsRequest {
    key: StructureMemberKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureMemberVersionsFilter | null;
}

export interface ListStructureMemberVersionsResponse {
    result: Result;
    versions: StructureMember[];
    total: number;
}

export interface GetStructureMemberVersionRequest {
    key: StructureMemberVersionKey;
}

export interface GetStructureMemberVersionResponse {
    result: Result;
    version: StructureMember | null;
}

export const subjects = {
    list_structure_members_request: 'trading.v1.structure_members.list',
    get_structure_member_request: 'trading.v1.structure_members.get',
    get_many_structure_members_request: 'trading.v1.structure_members.get_many',
    put_structure_member_request: 'trading.v1.structure_members.put',
    put_many_structure_members_request: 'trading.v1.structure_members.put_many',
    delete_structure_member_request: 'trading.v1.structure_members.delete',
    delete_many_structure_members_request: 'trading.v1.structure_members.delete_many',
    list_structure_member_versions_request: 'trading.v1.structure_members_versions.list',
    get_structure_member_version_request: 'trading.v1.structure_members_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structure_members_request: true,
    get_structure_member_request: true,
    get_many_structure_members_request: true,
    put_structure_member_request: true,
    put_many_structure_members_request: true,
    delete_structure_member_request: true,
    delete_many_structure_members_request: true,
    list_structure_member_versions_request: true,
    get_structure_member_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structure_members_events.created',
    updated: 'trading.v1.structure_members_events.updated',
    deleted: 'trading.v1.structure_members_events.deleted',
} as const;
