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
import type { TradePartyRole } from '../domain/trade_party_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradePartyRoleKey {
    trade_id: string;
    role: string;
}

export interface TradePartyRoleWrite {
    trade_id: string;
    role: string;
    counterparty_id: string;
}

export interface TradePartyRoleChange {
    write: TradePartyRoleWrite;
    precondition: Precondition;
}

export interface TradePartyRoleRemoval {
    key: TradePartyRoleKey;
    precondition: Precondition;
}

export interface TradePartyRoleLookup {
    key: TradePartyRoleKey;
    trade_party_role: TradePartyRole | null;
}

export interface TradePartyRoleEvent {
    event_id: string;
    key: TradePartyRoleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradePartyRoleVersionKey {
    trade_party_role: TradePartyRoleKey;
    version: number;
}

export interface TradePartyRoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradePartyRolesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTradePartyRolesResponse {
    result: Result;
    roles: TradePartyRole[];
    total: number;
}

export interface GetTradePartyRoleRequest {
    key: TradePartyRoleKey;
}

export interface GetTradePartyRoleResponse {
    result: Result;
    trade_party_role: TradePartyRole | null;
}

export interface GetManyTradePartyRolesRequest {
    keys: TradePartyRoleKey[];
}

export interface GetManyTradePartyRolesResponse {
    result: Result;
    entries: TradePartyRoleLookup[];
}

export interface PutTradePartyRoleRequest {
    change: TradePartyRoleChange;
    intent: ChangeIntent;
}

export interface PutTradePartyRoleResponse {
    result: Result;
    trade_party_role: TradePartyRole | null;
}

export interface PutManyTradePartyRolesRequest {
    changes: TradePartyRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyTradePartyRolesResponse {
    result: Result;
    roles: TradePartyRole[];
}

export interface DeleteTradePartyRoleRequest {
    removal: TradePartyRoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradePartyRoleResponse {
    result: Result;
}

export interface DeleteManyTradePartyRolesRequest {
    removals: TradePartyRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradePartyRolesResponse {
    result: Result;
}

export interface ListTradePartyRoleVersionsRequest {
    key: TradePartyRoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradePartyRoleVersionsFilter | null;
}

export interface ListTradePartyRoleVersionsResponse {
    result: Result;
    versions: TradePartyRole[];
    total: number;
}

export interface GetTradePartyRoleVersionRequest {
    key: TradePartyRoleVersionKey;
}

export interface GetTradePartyRoleVersionResponse {
    result: Result;
    version: TradePartyRole | null;
}

export const subjects = {
    list_trade_party_roles_request: 'trading.v1.trade_party_roles.list',
    get_trade_party_role_request: 'trading.v1.trade_party_roles.get',
    get_many_trade_party_roles_request: 'trading.v1.trade_party_roles.get_many',
    put_trade_party_role_request: 'trading.v1.trade_party_roles.put',
    put_many_trade_party_roles_request: 'trading.v1.trade_party_roles.put_many',
    delete_trade_party_role_request: 'trading.v1.trade_party_roles.delete',
    delete_many_trade_party_roles_request: 'trading.v1.trade_party_roles.delete_many',
    list_trade_party_role_versions_request: 'trading.v1.trade_party_roles_versions.list',
    get_trade_party_role_version_request: 'trading.v1.trade_party_roles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_party_roles_request: true,
    get_trade_party_role_request: true,
    get_many_trade_party_roles_request: true,
    put_trade_party_role_request: true,
    put_many_trade_party_roles_request: true,
    delete_trade_party_role_request: true,
    delete_many_trade_party_roles_request: true,
    list_trade_party_role_versions_request: true,
    get_trade_party_role_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_party_roles_events.created',
    updated: 'trading.v1.trade_party_roles_events.updated',
    deleted: 'trading.v1.trade_party_roles_events.deleted',
} as const;
