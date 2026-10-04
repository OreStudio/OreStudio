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
import type { CounterpartyAlias } from '../domain/counterparty_alias.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CounterpartyAliasKey {
    id_value: string;
}

export interface CounterpartyAliasLookup {
    key: CounterpartyAliasKey;
    counterparty_alias: CounterpartyAlias | null;
}

export interface CounterpartyAliasesFilter {
    id_value_one_of: string[] | null;
}

export interface CounterpartyAliasEvent {
    event_id: string;
    key: CounterpartyAliasKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCounterpartyAliasesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CounterpartyAliasesFilter | null;
}

export interface ListCounterpartyAliasesResponse {
    result: Result;
    counterparty_aliases: CounterpartyAlias[];
    total: number;
}

export interface GetCounterpartyAliasRequest {
    key: CounterpartyAliasKey;
}

export interface GetCounterpartyAliasResponse {
    result: Result;
    counterparty_alias: CounterpartyAlias | null;
}

export interface GetManyCounterpartyAliasesRequest {
    keys: CounterpartyAliasKey[];
}

export interface GetManyCounterpartyAliasesResponse {
    result: Result;
    entries: CounterpartyAliasLookup[];
}

export const subjects = {
    list_counterparty_aliases_request: 'dq.v1.counterparty_aliases.list',
    get_counterparty_alias_request: 'dq.v1.counterparty_aliases.get',
    get_many_counterparty_aliases_request: 'dq.v1.counterparty_aliases.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_aliases_request: true,
    get_counterparty_alias_request: true,
    get_many_counterparty_aliases_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.counterparty_aliases_events.created',
    updated: 'dq.v1.counterparty_aliases_events.updated',
    deleted: 'dq.v1.counterparty_aliases_events.deleted',
} as const;
