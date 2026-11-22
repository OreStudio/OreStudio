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
import type { KnowledgePage } from '../domain/knowledge_page.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface KnowledgePageKey {
    id: string;
}

export interface KnowledgePageLookup {
    key: KnowledgePageKey;
    knowledge_page: KnowledgePage | null;
}

export interface KnowledgePagesFilter {
    id_one_of: string[] | null;
}

export interface KnowledgePageEvent {
    event_id: string;
    key: KnowledgePageKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface KnowledgePageVersionKey {
    knowledge_page: KnowledgePageKey;
    version: number;
}

export interface KnowledgePageVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListKnowledgePagesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: KnowledgePagesFilter | null;
    as_of: string | null;
}

export interface ListKnowledgePagesResponse {
    result: Result;
    pages: KnowledgePage[];
    total: number;
}

export interface GetKnowledgePageRequest {
    key: KnowledgePageKey;
}

export interface GetKnowledgePageResponse {
    result: Result;
    knowledge_page: KnowledgePage | null;
}

export interface GetManyKnowledgePagesRequest {
    keys: KnowledgePageKey[];
}

export interface GetManyKnowledgePagesResponse {
    result: Result;
    entries: KnowledgePageLookup[];
}

export interface ListKnowledgePageVersionsRequest {
    key: KnowledgePageKey;
    offset: number;
    limit: number;
    order: Order;
    filter: KnowledgePageVersionsFilter | null;
}

export interface ListKnowledgePageVersionsResponse {
    result: Result;
    versions: KnowledgePage[];
    total: number;
}

export interface GetKnowledgePageVersionRequest {
    key: KnowledgePageVersionKey;
}

export interface GetKnowledgePageVersionResponse {
    result: Result;
    version: KnowledgePage | null;
}

export const subjects = {
    list_knowledge_pages_request: 'orgmode.v1.knowledge_pages.list',
    get_knowledge_page_request: 'orgmode.v1.knowledge_pages.get',
    get_many_knowledge_pages_request: 'orgmode.v1.knowledge_pages.get_many',
    list_knowledge_page_versions_request: 'orgmode.v1.knowledge_pages_versions.list',
    get_knowledge_page_version_request: 'orgmode.v1.knowledge_pages_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_knowledge_pages_request: true,
    get_knowledge_page_request: true,
    get_many_knowledge_pages_request: true,
    list_knowledge_page_versions_request: true,
    get_knowledge_page_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'orgmode.v1.events.knowledge_pages.created',
    updated: 'orgmode.v1.events.knowledge_pages.updated',
    deleted: 'orgmode.v1.events.knowledge_pages.deleted',
} as const;
