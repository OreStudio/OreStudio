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
import type { StoppedFeedNode } from '../domain/stopped_feed_node.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StoppedFeedNodeKey {
    node_name: string;
}

export interface StoppedFeedNodeWrite {
    node_name: string;
    kind: string;
    provider_code: string;
}

export interface StoppedFeedNodeChange {
    write: StoppedFeedNodeWrite;
    precondition: Precondition;
}

export interface StoppedFeedNodeRemoval {
    key: StoppedFeedNodeKey;
    precondition: Precondition;
}

export interface StoppedFeedNodeLookup {
    key: StoppedFeedNodeKey;
    stopped_feed_node: StoppedFeedNode | null;
}

export interface StoppedFeedNodesFilter {
    node_name_one_of: string[] | null;
}

export interface StoppedFeedNodeEvent {
    event_id: string;
    key: StoppedFeedNodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StoppedFeedNodeVersionKey {
    stopped_feed_node: StoppedFeedNodeKey;
    version: number;
}

export interface StoppedFeedNodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStoppedFeedNodesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StoppedFeedNodesFilter | null;
    as_of: string | null;
}

export interface ListStoppedFeedNodesResponse {
    result: Result;
    stopped_nodes: StoppedFeedNode[];
    total: number;
}

export interface GetStoppedFeedNodeRequest {
    key: StoppedFeedNodeKey;
}

export interface GetStoppedFeedNodeResponse {
    result: Result;
    stopped_feed_node: StoppedFeedNode | null;
}

export interface GetManyStoppedFeedNodesRequest {
    keys: StoppedFeedNodeKey[];
}

export interface GetManyStoppedFeedNodesResponse {
    result: Result;
    entries: StoppedFeedNodeLookup[];
}

export interface PutStoppedFeedNodeRequest {
    change: StoppedFeedNodeChange;
    intent: ChangeIntent;
}

export interface PutStoppedFeedNodeResponse {
    result: Result;
    stopped_feed_node: StoppedFeedNode | null;
}

export interface PutManyStoppedFeedNodesRequest {
    changes: StoppedFeedNodeChange[];
    intent: ChangeIntent;
}

export interface PutManyStoppedFeedNodesResponse {
    result: Result;
    stopped_nodes: StoppedFeedNode[];
}

export interface DeleteStoppedFeedNodeRequest {
    removal: StoppedFeedNodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteStoppedFeedNodeResponse {
    result: Result;
}

export interface DeleteManyStoppedFeedNodesRequest {
    removals: StoppedFeedNodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStoppedFeedNodesResponse {
    result: Result;
}

export interface ListStoppedFeedNodeVersionsRequest {
    key: StoppedFeedNodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StoppedFeedNodeVersionsFilter | null;
}

export interface ListStoppedFeedNodeVersionsResponse {
    result: Result;
    versions: StoppedFeedNode[];
    total: number;
}

export interface GetStoppedFeedNodeVersionRequest {
    key: StoppedFeedNodeVersionKey;
}

export interface GetStoppedFeedNodeVersionResponse {
    result: Result;
    version: StoppedFeedNode | null;
}

export const subjects = {
    list_stopped_feed_nodes_request: 'synthetic.v1.stopped_feed_nodes.list',
    get_stopped_feed_node_request: 'synthetic.v1.stopped_feed_nodes.get',
    get_many_stopped_feed_nodes_request: 'synthetic.v1.stopped_feed_nodes.get_many',
    put_stopped_feed_node_request: 'synthetic.v1.stopped_feed_nodes.put',
    put_many_stopped_feed_nodes_request: 'synthetic.v1.stopped_feed_nodes.put_many',
    delete_stopped_feed_node_request: 'synthetic.v1.stopped_feed_nodes.delete',
    delete_many_stopped_feed_nodes_request: 'synthetic.v1.stopped_feed_nodes.delete_many',
    list_stopped_feed_node_versions_request: 'synthetic.v1.stopped_feed_nodes_versions.list',
    get_stopped_feed_node_version_request: 'synthetic.v1.stopped_feed_nodes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stopped_feed_nodes_request: true,
    get_stopped_feed_node_request: true,
    get_many_stopped_feed_nodes_request: true,
    put_stopped_feed_node_request: true,
    put_many_stopped_feed_nodes_request: true,
    delete_stopped_feed_node_request: true,
    delete_many_stopped_feed_nodes_request: true,
    list_stopped_feed_node_versions_request: true,
    get_stopped_feed_node_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.stopped_feed_nodes.created',
    updated: 'synthetic.v1.events.stopped_feed_nodes.updated',
    deleted: 'synthetic.v1.events.stopped_feed_nodes.deleted',
} as const;
