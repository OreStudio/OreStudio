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
import type { ScriptVariant } from '../domain/script_variant.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ScriptVariantKey {
    script_name: string;
    purpose: string;
}

export interface ScriptVariantWrite {
    script_name: string;
    purpose: string;
    code: string;
    npv_variable: string;
}

export interface ScriptVariantChange {
    write: ScriptVariantWrite;
    precondition: Precondition;
}

export interface ScriptVariantRemoval {
    key: ScriptVariantKey;
    precondition: Precondition;
}

export interface ScriptVariantLookup {
    key: ScriptVariantKey;
    script_variant: ScriptVariant | null;
}

export interface ScriptVariantEvent {
    event_id: string;
    key: ScriptVariantKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ScriptVariantVersionKey {
    script_variant: ScriptVariantKey;
    version: number;
}

export interface ScriptVariantVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListScriptVariantsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListScriptVariantsResponse {
    result: Result;
    variants: ScriptVariant[];
    total: number;
}

export interface GetScriptVariantRequest {
    key: ScriptVariantKey;
}

export interface GetScriptVariantResponse {
    result: Result;
    script_variant: ScriptVariant | null;
}

export interface GetManyScriptVariantsRequest {
    keys: ScriptVariantKey[];
}

export interface GetManyScriptVariantsResponse {
    result: Result;
    entries: ScriptVariantLookup[];
}

export interface PutScriptVariantRequest {
    change: ScriptVariantChange;
    intent: ChangeIntent;
}

export interface PutScriptVariantResponse {
    result: Result;
    script_variant: ScriptVariant | null;
}

export interface PutManyScriptVariantsRequest {
    changes: ScriptVariantChange[];
    intent: ChangeIntent;
}

export interface PutManyScriptVariantsResponse {
    result: Result;
    variants: ScriptVariant[];
}

export interface DeleteScriptVariantRequest {
    removal: ScriptVariantRemoval;
    intent: ChangeIntent;
}

export interface DeleteScriptVariantResponse {
    result: Result;
}

export interface DeleteManyScriptVariantsRequest {
    removals: ScriptVariantRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyScriptVariantsResponse {
    result: Result;
}

export interface ListScriptVariantVersionsRequest {
    key: ScriptVariantKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ScriptVariantVersionsFilter | null;
}

export interface ListScriptVariantVersionsResponse {
    result: Result;
    versions: ScriptVariant[];
    total: number;
}

export interface GetScriptVariantVersionRequest {
    key: ScriptVariantVersionKey;
}

export interface GetScriptVariantVersionResponse {
    result: Result;
    version: ScriptVariant | null;
}

export const subjects = {
    list_script_variants_request: 'trading.v1.script_variants.list',
    get_script_variant_request: 'trading.v1.script_variants.get',
    get_many_script_variants_request: 'trading.v1.script_variants.get_many',
    put_script_variant_request: 'trading.v1.script_variants.put',
    put_many_script_variants_request: 'trading.v1.script_variants.put_many',
    delete_script_variant_request: 'trading.v1.script_variants.delete',
    delete_many_script_variants_request: 'trading.v1.script_variants.delete_many',
    list_script_variant_versions_request: 'trading.v1.script_variants_versions.list',
    get_script_variant_version_request: 'trading.v1.script_variants_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_script_variants_request: true,
    get_script_variant_request: true,
    get_many_script_variants_request: true,
    put_script_variant_request: true,
    put_many_script_variants_request: true,
    delete_script_variant_request: true,
    delete_many_script_variants_request: true,
    list_script_variant_versions_request: true,
    get_script_variant_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.script_variants.created',
    updated: 'trading.v1.events.script_variants.updated',
    deleted: 'trading.v1.events.script_variants.deleted',
} as const;
