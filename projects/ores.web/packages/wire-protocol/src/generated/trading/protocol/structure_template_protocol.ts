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
import type { StructureTemplate } from '../domain/structure_template.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StructureTemplateKey {
    code: string;
}

export interface StructureTemplateWrite {
    code: string;
    description: string;
    kind: string;
}

export interface StructureTemplateChange {
    write: StructureTemplateWrite;
    precondition: Precondition;
}

export interface StructureTemplateRemoval {
    key: StructureTemplateKey;
    precondition: Precondition;
}

export interface StructureTemplateLookup {
    key: StructureTemplateKey;
    structure_template: StructureTemplate | null;
}

export interface StructureTemplatesFilter {
    code_one_of: string[] | null;
}

export interface StructureTemplateEvent {
    event_id: string;
    key: StructureTemplateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureTemplateVersionKey {
    structure_template: StructureTemplateKey;
    version: number;
}

export interface StructureTemplateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructureTemplatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StructureTemplatesFilter | null;
    as_of: string | null;
}

export interface ListStructureTemplatesResponse {
    result: Result;
    structure_templates: StructureTemplate[];
    total: number;
}

export interface GetStructureTemplateRequest {
    key: StructureTemplateKey;
}

export interface GetStructureTemplateResponse {
    result: Result;
    structure_template: StructureTemplate | null;
}

export interface GetManyStructureTemplatesRequest {
    keys: StructureTemplateKey[];
}

export interface GetManyStructureTemplatesResponse {
    result: Result;
    entries: StructureTemplateLookup[];
}

export interface PutStructureTemplateRequest {
    change: StructureTemplateChange;
    intent: ChangeIntent;
}

export interface PutStructureTemplateResponse {
    result: Result;
    structure_template: StructureTemplate | null;
}

export interface PutManyStructureTemplatesRequest {
    changes: StructureTemplateChange[];
    intent: ChangeIntent;
}

export interface PutManyStructureTemplatesResponse {
    result: Result;
    structure_templates: StructureTemplate[];
}

export interface DeleteStructureTemplateRequest {
    removal: StructureTemplateRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureTemplateResponse {
    result: Result;
}

export interface DeleteManyStructureTemplatesRequest {
    removals: StructureTemplateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructureTemplatesResponse {
    result: Result;
}

export interface ListStructureTemplateVersionsRequest {
    key: StructureTemplateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureTemplateVersionsFilter | null;
}

export interface ListStructureTemplateVersionsResponse {
    result: Result;
    versions: StructureTemplate[];
    total: number;
}

export interface GetStructureTemplateVersionRequest {
    key: StructureTemplateVersionKey;
}

export interface GetStructureTemplateVersionResponse {
    result: Result;
    version: StructureTemplate | null;
}

export const subjects = {
    list_structure_templates_request: 'trading.v1.structure_templates.list',
    get_structure_template_request: 'trading.v1.structure_templates.get',
    get_many_structure_templates_request: 'trading.v1.structure_templates.get_many',
    put_structure_template_request: 'trading.v1.structure_templates.put',
    put_many_structure_templates_request: 'trading.v1.structure_templates.put_many',
    delete_structure_template_request: 'trading.v1.structure_templates.delete',
    delete_many_structure_templates_request: 'trading.v1.structure_templates.delete_many',
    list_structure_template_versions_request: 'trading.v1.structure_templates_versions.list',
    get_structure_template_version_request: 'trading.v1.structure_templates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structure_templates_request: true,
    get_structure_template_request: true,
    get_many_structure_templates_request: true,
    put_structure_template_request: true,
    put_many_structure_templates_request: true,
    delete_structure_template_request: true,
    delete_many_structure_templates_request: true,
    list_structure_template_versions_request: true,
    get_structure_template_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structure_templates_events.created',
    updated: 'trading.v1.structure_templates_events.updated',
    deleted: 'trading.v1.structure_templates_events.deleted',
} as const;
