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
import type { Session } from '../domain/session.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SessionKey {
    code: string;
}

export interface SessionWrite {
    code: string;
    name: string;
    supplier_code: string;
    adapter_code: string;
    environment: string;
    endpoint: string;
    credential_ref: string;
    settings: string;
    enabled: boolean;
    description: string;
}

export interface SessionChange {
    write: SessionWrite;
    precondition: Precondition;
}

export interface SessionRemoval {
    key: SessionKey;
    precondition: Precondition;
}

export interface SessionLookup {
    key: SessionKey;
    session: Session | null;
}

export interface SessionsFilter {
    supplier_code: string | null;
    adapter_code: string | null;
    environment: string | null;
    code_one_of: string[] | null;
    supplier_code_one_of: string[] | null;
    adapter_code_one_of: string[] | null;
    environment_one_of: string[] | null;
    search: string | null;
}

export interface SessionEvent {
    event_id: string;
    key: SessionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SessionVersionKey {
    session: SessionKey;
    version: number;
}

export interface SessionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSessionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SessionsFilter | null;
    as_of: string | null;
}

export interface ListSessionsResponse {
    result: Result;
    sessions: Session[];
    total: number;
}

export interface GetSessionRequest {
    key: SessionKey;
}

export interface GetSessionResponse {
    result: Result;
    session: Session | null;
}

export interface GetManySessionsRequest {
    keys: SessionKey[];
}

export interface GetManySessionsResponse {
    result: Result;
    entries: SessionLookup[];
}

export interface PutSessionRequest {
    change: SessionChange;
    intent: ChangeIntent;
}

export interface PutSessionResponse {
    result: Result;
    session: Session | null;
}

export interface PutManySessionsRequest {
    changes: SessionChange[];
    intent: ChangeIntent;
}

export interface PutManySessionsResponse {
    result: Result;
    sessions: Session[];
}

export interface DeleteSessionRequest {
    removal: SessionRemoval;
    intent: ChangeIntent;
}

export interface DeleteSessionResponse {
    result: Result;
}

export interface DeleteManySessionsRequest {
    removals: SessionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySessionsResponse {
    result: Result;
}

export interface ListSessionVersionsRequest {
    key: SessionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SessionVersionsFilter | null;
}

export interface ListSessionVersionsResponse {
    result: Result;
    versions: Session[];
    total: number;
}

export interface GetSessionVersionRequest {
    key: SessionVersionKey;
}

export interface GetSessionVersionResponse {
    result: Result;
    version: Session | null;
}

export const subjects = {
    list_sessions_request: 'marketdata.v1.sessions.list',
    get_session_request: 'marketdata.v1.sessions.get',
    get_many_sessions_request: 'marketdata.v1.sessions.get_many',
    put_session_request: 'marketdata.v1.sessions.put',
    put_many_sessions_request: 'marketdata.v1.sessions.put_many',
    delete_session_request: 'marketdata.v1.sessions.delete',
    delete_many_sessions_request: 'marketdata.v1.sessions.delete_many',
    list_session_versions_request: 'marketdata.v1.sessions_versions.list',
    get_session_version_request: 'marketdata.v1.sessions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sessions_request: true,
    get_session_request: true,
    get_many_sessions_request: true,
    put_session_request: true,
    put_many_sessions_request: true,
    delete_session_request: true,
    delete_many_sessions_request: true,
    list_session_versions_request: true,
    get_session_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.sessions_events.created',
    updated: 'marketdata.v1.sessions_events.updated',
    deleted: 'marketdata.v1.sessions_events.deleted',
} as const;
