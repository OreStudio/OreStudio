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
import type { TradeGenerationCounterparty } from '../domain/trade_generation_counterparty.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeGenerationCounterpartyKey {
    id: string;
}

export interface TradeGenerationCounterpartyWrite {
    id: string;
    config_id: string;
    counterparty_id: string;
}

export interface TradeGenerationCounterpartyChange {
    write: TradeGenerationCounterpartyWrite;
    precondition: Precondition;
}

export interface TradeGenerationCounterpartyRemoval {
    key: TradeGenerationCounterpartyKey;
    precondition: Precondition;
}

export interface TradeGenerationCounterpartyLookup {
    key: TradeGenerationCounterpartyKey;
    trade_generation_counterparty: TradeGenerationCounterparty | null;
}

export interface TradeGenerationCounterpartiesFilter {
    config_id: string | null;
    id_one_of: string[] | null;
    config_id_one_of: string[] | null;
}

export interface TradeGenerationCounterpartyEvent {
    event_id: string;
    key: TradeGenerationCounterpartyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeGenerationCounterpartyVersionKey {
    trade_generation_counterparty: TradeGenerationCounterpartyKey;
    version: number;
}

export interface TradeGenerationCounterpartyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeGenerationCounterpartiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationCounterpartiesFilter | null;
    as_of: string | null;
}

export interface ListTradeGenerationCounterpartiesResponse {
    result: Result;
    trade_generation_counterparties: TradeGenerationCounterparty[];
    total: number;
}

export interface GetTradeGenerationCounterpartyRequest {
    key: TradeGenerationCounterpartyKey;
}

export interface GetTradeGenerationCounterpartyResponse {
    result: Result;
    trade_generation_counterparty: TradeGenerationCounterparty | null;
}

export interface GetManyTradeGenerationCounterpartiesRequest {
    keys: TradeGenerationCounterpartyKey[];
}

export interface GetManyTradeGenerationCounterpartiesResponse {
    result: Result;
    entries: TradeGenerationCounterpartyLookup[];
}

export interface PutTradeGenerationCounterpartyRequest {
    change: TradeGenerationCounterpartyChange;
    intent: ChangeIntent;
}

export interface PutTradeGenerationCounterpartyResponse {
    result: Result;
    trade_generation_counterparty: TradeGenerationCounterparty | null;
}

export interface PutManyTradeGenerationCounterpartiesRequest {
    changes: TradeGenerationCounterpartyChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeGenerationCounterpartiesResponse {
    result: Result;
    trade_generation_counterparties: TradeGenerationCounterparty[];
}

export interface DeleteTradeGenerationCounterpartyRequest {
    removal: TradeGenerationCounterpartyRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeGenerationCounterpartyResponse {
    result: Result;
}

export interface DeleteManyTradeGenerationCounterpartiesRequest {
    removals: TradeGenerationCounterpartyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeGenerationCounterpartiesResponse {
    result: Result;
}

export interface ListTradeGenerationCounterpartyVersionsRequest {
    key: TradeGenerationCounterpartyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationCounterpartyVersionsFilter | null;
}

export interface ListTradeGenerationCounterpartyVersionsResponse {
    result: Result;
    versions: TradeGenerationCounterparty[];
    total: number;
}

export interface GetTradeGenerationCounterpartyVersionRequest {
    key: TradeGenerationCounterpartyVersionKey;
}

export interface GetTradeGenerationCounterpartyVersionResponse {
    result: Result;
    version: TradeGenerationCounterparty | null;
}

export const subjects = {
    list_trade_generation_counterparties_request:
        'synthetic.v1.trade_generation_counterparties.list',
    get_trade_generation_counterparty_request: 'synthetic.v1.trade_generation_counterparties.get',
    get_many_trade_generation_counterparties_request:
        'synthetic.v1.trade_generation_counterparties.get_many',
    put_trade_generation_counterparty_request: 'synthetic.v1.trade_generation_counterparties.put',
    put_many_trade_generation_counterparties_request:
        'synthetic.v1.trade_generation_counterparties.put_many',
    delete_trade_generation_counterparty_request:
        'synthetic.v1.trade_generation_counterparties.delete',
    delete_many_trade_generation_counterparties_request:
        'synthetic.v1.trade_generation_counterparties.delete_many',
    list_trade_generation_counterparty_versions_request:
        'synthetic.v1.trade_generation_counterparties_versions.list',
    get_trade_generation_counterparty_version_request:
        'synthetic.v1.trade_generation_counterparties_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_generation_counterparties_request: true,
    get_trade_generation_counterparty_request: true,
    get_many_trade_generation_counterparties_request: true,
    put_trade_generation_counterparty_request: true,
    put_many_trade_generation_counterparties_request: true,
    delete_trade_generation_counterparty_request: true,
    delete_many_trade_generation_counterparties_request: true,
    list_trade_generation_counterparty_versions_request: true,
    get_trade_generation_counterparty_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.trade_generation_counterparties_events.created',
    updated: 'synthetic.v1.trade_generation_counterparties_events.updated',
    deleted: 'synthetic.v1.trade_generation_counterparties_events.deleted',
} as const;
