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
    id: string;
}

export interface SessionWrite {
    id: string;
    start_time: string;
    account_id: string;
    end_time: string;
    client_ip: string;
    client_identifier: string;
    client_version_major: number;
    client_version_minor: number;
    bytes_sent: number;
    bytes_received: number;
    country_code: string;
    protocol: string;
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

export interface SessionEvent {
    event_id: string;
    key: SessionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListSessionsRequest {
    offset: number;
    limit: number;
    order: Order;
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
    session: Session;
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

export const subjects = {
    list_sessions_request: "iam.v1.sessions.list",
    get_session_request: "iam.v1.sessions.get",
    get_many_sessions_request: "iam.v1.sessions.get_many",
    put_session_request: "iam.v1.sessions.put",
    put_many_sessions_request: "iam.v1.sessions.put_many",
    delete_session_request: "iam.v1.sessions.delete",
    delete_many_sessions_request: "iam.v1.sessions.delete_many",
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
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.sessions_events.created",
    updated: "iam.v1.sessions_events.updated",
    deleted: "iam.v1.sessions_events.deleted",
} as const;
