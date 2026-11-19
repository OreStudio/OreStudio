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
import type { CboBasketMember } from '../domain/cbo_basket_member.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CboBasketMemberKey {
    trade_id: string;
    sequence_number: number;
}

export interface CboBasketMemberWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    sub_trade_id: string;
    security_id: string;
    bond_notional: string;
    counterparty: string | null;
}

export interface CboBasketMemberChange {
    write: CboBasketMemberWrite;
    precondition: Precondition;
}

export interface CboBasketMemberRemoval {
    key: CboBasketMemberKey;
    precondition: Precondition;
}

export interface CboBasketMemberLookup {
    key: CboBasketMemberKey;
    cbo_basket_member: CboBasketMember | null;
}

export interface CboBasketMembersFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface CboBasketMemberEvent {
    event_id: string;
    key: CboBasketMemberKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CboBasketMemberVersionKey {
    cbo_basket_member: CboBasketMemberKey;
    version: number;
}

export interface CboBasketMemberVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCboBasketMembersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CboBasketMembersFilter | null;
    as_of: string | null;
}

export interface ListCboBasketMembersResponse {
    result: Result;
    cbo_basket_members: CboBasketMember[];
    total: number;
}

export interface GetCboBasketMemberRequest {
    key: CboBasketMemberKey;
}

export interface GetCboBasketMemberResponse {
    result: Result;
    cbo_basket_member: CboBasketMember | null;
}

export interface GetManyCboBasketMembersRequest {
    keys: CboBasketMemberKey[];
}

export interface GetManyCboBasketMembersResponse {
    result: Result;
    entries: CboBasketMemberLookup[];
}

export interface PutCboBasketMemberRequest {
    change: CboBasketMemberChange;
    intent: ChangeIntent;
}

export interface PutCboBasketMemberResponse {
    result: Result;
    cbo_basket_member: CboBasketMember | null;
}

export interface PutManyCboBasketMembersRequest {
    changes: CboBasketMemberChange[];
    intent: ChangeIntent;
}

export interface PutManyCboBasketMembersResponse {
    result: Result;
    cbo_basket_members: CboBasketMember[];
}

export interface DeleteCboBasketMemberRequest {
    removal: CboBasketMemberRemoval;
    intent: ChangeIntent;
}

export interface DeleteCboBasketMemberResponse {
    result: Result;
}

export interface DeleteManyCboBasketMembersRequest {
    removals: CboBasketMemberRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCboBasketMembersResponse {
    result: Result;
}

export interface ListByTradeIdCboBasketMembersRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CboBasketMembersFilter | null;
}

export interface ListByTradeIdCboBasketMembersResponse {
    result: Result;
    cbo_basket_members: CboBasketMember[];
    total: number;
}

export interface ListCboBasketMemberVersionsRequest {
    key: CboBasketMemberKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CboBasketMemberVersionsFilter | null;
}

export interface ListCboBasketMemberVersionsResponse {
    result: Result;
    versions: CboBasketMember[];
    total: number;
}

export interface GetCboBasketMemberVersionRequest {
    key: CboBasketMemberVersionKey;
}

export interface GetCboBasketMemberVersionResponse {
    result: Result;
    version: CboBasketMember | null;
}

export const subjects = {
    list_cbo_basket_members_request: 'trading.v1.cbo_basket_members.list',
    get_cbo_basket_member_request: 'trading.v1.cbo_basket_members.get',
    get_many_cbo_basket_members_request: 'trading.v1.cbo_basket_members.get_many',
    put_cbo_basket_member_request: 'trading.v1.cbo_basket_members.put',
    put_many_cbo_basket_members_request: 'trading.v1.cbo_basket_members.put_many',
    delete_cbo_basket_member_request: 'trading.v1.cbo_basket_members.delete',
    delete_many_cbo_basket_members_request: 'trading.v1.cbo_basket_members.delete_many',
    list_by_trade_id_cbo_basket_members_request: 'trading.v1.cbo_basket_members.list_by_trade_id',
    list_cbo_basket_member_versions_request: 'trading.v1.cbo_basket_members_versions.list',
    get_cbo_basket_member_version_request: 'trading.v1.cbo_basket_members_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cbo_basket_members_request: true,
    get_cbo_basket_member_request: true,
    get_many_cbo_basket_members_request: true,
    put_cbo_basket_member_request: true,
    put_many_cbo_basket_members_request: true,
    delete_cbo_basket_member_request: true,
    delete_many_cbo_basket_members_request: true,
    list_by_trade_id_cbo_basket_members_request: true,
    list_cbo_basket_member_versions_request: true,
    get_cbo_basket_member_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.cbo_basket_members.created',
    updated: 'trading.v1.events.cbo_basket_members.updated',
    deleted: 'trading.v1.events.cbo_basket_members.deleted',
} as const;
