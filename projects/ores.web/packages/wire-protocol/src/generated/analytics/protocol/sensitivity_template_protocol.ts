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
import type { SensitivityTemplate } from '../domain/sensitivity_template.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SensitivityTemplateKey {
    code: string;
}

export interface SensitivityTemplateWrite {
    code: string;
    asset_class: string;
    method: string;
    method_code: string;
    description: string;
}

export interface SensitivityTemplateChange {
    write: SensitivityTemplateWrite;
    precondition: Precondition;
}

export interface SensitivityTemplateRemoval {
    key: SensitivityTemplateKey;
    precondition: Precondition;
}

export interface SensitivityTemplateLookup {
    key: SensitivityTemplateKey;
    sensitivity_template: SensitivityTemplate | null;
}

export interface SensitivityTemplatesFilter {
    code_one_of: string[] | null;
    search: string | null;
}

export interface SensitivityTemplateEvent {
    event_id: string;
    key: SensitivityTemplateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SensitivityTemplateVersionKey {
    sensitivity_template: SensitivityTemplateKey;
    version: number;
}

export interface SensitivityTemplateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSensitivityTemplatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityTemplatesFilter | null;
    as_of: string | null;
}

export interface ListSensitivityTemplatesResponse {
    result: Result;
    templates: SensitivityTemplate[];
    total: number;
}

export interface GetSensitivityTemplateRequest {
    key: SensitivityTemplateKey;
}

export interface GetSensitivityTemplateResponse {
    result: Result;
    sensitivity_template: SensitivityTemplate | null;
}

export interface GetManySensitivityTemplatesRequest {
    keys: SensitivityTemplateKey[];
}

export interface GetManySensitivityTemplatesResponse {
    result: Result;
    entries: SensitivityTemplateLookup[];
}

export interface PutSensitivityTemplateRequest {
    change: SensitivityTemplateChange;
    intent: ChangeIntent;
}

export interface PutSensitivityTemplateResponse {
    result: Result;
    sensitivity_template: SensitivityTemplate | null;
}

export interface PutManySensitivityTemplatesRequest {
    changes: SensitivityTemplateChange[];
    intent: ChangeIntent;
}

export interface PutManySensitivityTemplatesResponse {
    result: Result;
    templates: SensitivityTemplate[];
}

export interface DeleteSensitivityTemplateRequest {
    removal: SensitivityTemplateRemoval;
    intent: ChangeIntent;
}

export interface DeleteSensitivityTemplateResponse {
    result: Result;
}

export interface DeleteManySensitivityTemplatesRequest {
    removals: SensitivityTemplateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySensitivityTemplatesResponse {
    result: Result;
}

export interface ListSensitivityTemplateVersionsRequest {
    key: SensitivityTemplateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityTemplateVersionsFilter | null;
}

export interface ListSensitivityTemplateVersionsResponse {
    result: Result;
    versions: SensitivityTemplate[];
    total: number;
}

export interface GetSensitivityTemplateVersionRequest {
    key: SensitivityTemplateVersionKey;
}

export interface GetSensitivityTemplateVersionResponse {
    result: Result;
    version: SensitivityTemplate | null;
}

export const subjects = {
    list_sensitivity_templates_request: 'analytics.v1.sensitivity_templates.list',
    get_sensitivity_template_request: 'analytics.v1.sensitivity_templates.get',
    get_many_sensitivity_templates_request: 'analytics.v1.sensitivity_templates.get_many',
    put_sensitivity_template_request: 'analytics.v1.sensitivity_templates.put',
    put_many_sensitivity_templates_request: 'analytics.v1.sensitivity_templates.put_many',
    delete_sensitivity_template_request: 'analytics.v1.sensitivity_templates.delete',
    delete_many_sensitivity_templates_request: 'analytics.v1.sensitivity_templates.delete_many',
    list_sensitivity_template_versions_request: 'analytics.v1.sensitivity_templates_versions.list',
    get_sensitivity_template_version_request: 'analytics.v1.sensitivity_templates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sensitivity_templates_request: true,
    get_sensitivity_template_request: true,
    get_many_sensitivity_templates_request: true,
    put_sensitivity_template_request: true,
    put_many_sensitivity_templates_request: true,
    delete_sensitivity_template_request: true,
    delete_many_sensitivity_templates_request: true,
    list_sensitivity_template_versions_request: true,
    get_sensitivity_template_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.sensitivity_templates_events.created',
    updated: 'analytics.v1.sensitivity_templates_events.updated',
    deleted: 'analytics.v1.sensitivity_templates_events.deleted',
} as const;
