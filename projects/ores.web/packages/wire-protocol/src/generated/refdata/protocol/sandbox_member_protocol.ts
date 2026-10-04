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
import type { SandboxMember } from '../domain/sandbox_member.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SandboxMemberKey {
    id: string;
}

export interface SandboxMemberWrite {
    id: string;
    sandbox_id: string;
    account_id: string;
}

export interface SandboxMemberChange {
    write: SandboxMemberWrite;
    precondition: Precondition;
}

export interface SandboxMemberRemoval {
    key: SandboxMemberKey;
    precondition: Precondition;
}

export interface SandboxMemberLookup {
    key: SandboxMemberKey;
    sandbox_member: SandboxMember | null;
}

export interface SandboxMembersFilter {
    sandbox_id: string | null;
    account_id: string | null;
    id_one_of: string[] | null;
    sandbox_id_one_of: string[] | null;
    account_id_one_of: string[] | null;
}

export interface SandboxMemberEvent {
    event_id: string;
    key: SandboxMemberKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SandboxMemberVersionKey {
    sandbox_member: SandboxMemberKey;
    version: number;
}

export interface SandboxMemberVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSandboxMembersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxMembersFilter | null;
}

export interface ListSandboxMembersResponse {
    result: Result;
    sandbox_members: SandboxMember[];
    total: number;
}

export interface GetSandboxMemberRequest {
    key: SandboxMemberKey;
}

export interface GetSandboxMemberResponse {
    result: Result;
    sandbox_member: SandboxMember | null;
}

export interface GetManySandboxMembersRequest {
    keys: SandboxMemberKey[];
}

export interface GetManySandboxMembersResponse {
    result: Result;
    entries: SandboxMemberLookup[];
}

export interface PutSandboxMemberRequest {
    change: SandboxMemberChange;
    intent: ChangeIntent;
}

export interface PutSandboxMemberResponse {
    result: Result;
    sandbox_member: SandboxMember | null;
}

export interface PutManySandboxMembersRequest {
    changes: SandboxMemberChange[];
    intent: ChangeIntent;
}

export interface PutManySandboxMembersResponse {
    result: Result;
    sandbox_members: SandboxMember[];
}

export interface DeleteSandboxMemberRequest {
    removal: SandboxMemberRemoval;
    intent: ChangeIntent;
}

export interface DeleteSandboxMemberResponse {
    result: Result;
}

export interface DeleteManySandboxMembersRequest {
    removals: SandboxMemberRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySandboxMembersResponse {
    result: Result;
}

export interface ListBySandboxIdSandboxMembersRequest {
    sandbox_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxMembersFilter | null;
}

export interface ListBySandboxIdSandboxMembersResponse {
    result: Result;
    sandbox_members: SandboxMember[];
    total: number;
}

export interface ListByAccountIdSandboxMembersRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxMembersFilter | null;
}

export interface ListByAccountIdSandboxMembersResponse {
    result: Result;
    sandbox_members: SandboxMember[];
    total: number;
}

export interface ListSandboxMemberVersionsRequest {
    key: SandboxMemberKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxMemberVersionsFilter | null;
}

export interface ListSandboxMemberVersionsResponse {
    result: Result;
    versions: SandboxMember[];
    total: number;
}

export interface GetSandboxMemberVersionRequest {
    key: SandboxMemberVersionKey;
}

export interface GetSandboxMemberVersionResponse {
    result: Result;
    version: SandboxMember | null;
}

export const subjects = {
    list_sandbox_members_request: 'refdata.v1.sandbox_members.list',
    get_sandbox_member_request: 'refdata.v1.sandbox_members.get',
    get_many_sandbox_members_request: 'refdata.v1.sandbox_members.get_many',
    put_sandbox_member_request: 'refdata.v1.sandbox_members.put',
    put_many_sandbox_members_request: 'refdata.v1.sandbox_members.put_many',
    delete_sandbox_member_request: 'refdata.v1.sandbox_members.delete',
    delete_many_sandbox_members_request: 'refdata.v1.sandbox_members.delete_many',
    list_by_sandbox_id_sandbox_members_request: 'refdata.v1.sandbox_members.list_by_sandbox_id',
    list_by_account_id_sandbox_members_request: 'refdata.v1.sandbox_members.list_by_account_id',
    list_sandbox_member_versions_request: 'refdata.v1.sandbox_members_versions.list',
    get_sandbox_member_version_request: 'refdata.v1.sandbox_members_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sandbox_members_request: true,
    get_sandbox_member_request: true,
    get_many_sandbox_members_request: true,
    put_sandbox_member_request: true,
    put_many_sandbox_members_request: true,
    delete_sandbox_member_request: true,
    delete_many_sandbox_members_request: true,
    list_by_sandbox_id_sandbox_members_request: true,
    list_by_account_id_sandbox_members_request: true,
    list_sandbox_member_versions_request: true,
    get_sandbox_member_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.sandbox_members_events.created',
    updated: 'refdata.v1.sandbox_members_events.updated',
    deleted: 'refdata.v1.sandbox_members_events.deleted',
} as const;
