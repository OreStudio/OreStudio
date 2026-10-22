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
import type { PricingPairSetting } from '../domain/pricing_pair_setting.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PricingPairSettingKey {
    code: string;
}

export interface PricingPairSettingWrite {
    code: string;
    model: string;
    engine: string;
    pricing_engine_type_code: string;
    scope: string;
    name: string;
    kind: string;
    usual_value: string;
    options: string;
    is_required: boolean;
    note: string;
}

export interface PricingPairSettingChange {
    write: PricingPairSettingWrite;
    precondition: Precondition;
}

export interface PricingPairSettingRemoval {
    key: PricingPairSettingKey;
    precondition: Precondition;
}

export interface PricingPairSettingLookup {
    key: PricingPairSettingKey;
    pricing_pair_setting: PricingPairSetting | null;
}

export interface PricingPairSettingsFilter {
    code_one_of: string[] | null;
}

export interface PricingPairSettingEvent {
    event_id: string;
    key: PricingPairSettingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PricingPairSettingVersionKey {
    pricing_pair_setting: PricingPairSettingKey;
    version: number;
}

export interface PricingPairSettingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPricingPairSettingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PricingPairSettingsFilter | null;
    as_of: string | null;
}

export interface ListPricingPairSettingsResponse {
    result: Result;
    settings: PricingPairSetting[];
    total: number;
}

export interface GetPricingPairSettingRequest {
    key: PricingPairSettingKey;
}

export interface GetPricingPairSettingResponse {
    result: Result;
    pricing_pair_setting: PricingPairSetting | null;
}

export interface GetManyPricingPairSettingsRequest {
    keys: PricingPairSettingKey[];
}

export interface GetManyPricingPairSettingsResponse {
    result: Result;
    entries: PricingPairSettingLookup[];
}

export interface PutPricingPairSettingRequest {
    change: PricingPairSettingChange;
    intent: ChangeIntent;
}

export interface PutPricingPairSettingResponse {
    result: Result;
    pricing_pair_setting: PricingPairSetting | null;
}

export interface PutManyPricingPairSettingsRequest {
    changes: PricingPairSettingChange[];
    intent: ChangeIntent;
}

export interface PutManyPricingPairSettingsResponse {
    result: Result;
    settings: PricingPairSetting[];
}

export interface DeletePricingPairSettingRequest {
    removal: PricingPairSettingRemoval;
    intent: ChangeIntent;
}

export interface DeletePricingPairSettingResponse {
    result: Result;
}

export interface DeleteManyPricingPairSettingsRequest {
    removals: PricingPairSettingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPricingPairSettingsResponse {
    result: Result;
}

export interface ListPricingPairSettingVersionsRequest {
    key: PricingPairSettingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PricingPairSettingVersionsFilter | null;
}

export interface ListPricingPairSettingVersionsResponse {
    result: Result;
    versions: PricingPairSetting[];
    total: number;
}

export interface GetPricingPairSettingVersionRequest {
    key: PricingPairSettingVersionKey;
}

export interface GetPricingPairSettingVersionResponse {
    result: Result;
    version: PricingPairSetting | null;
}

export const subjects = {
    list_pricing_pair_settings_request: 'analytics.v1.pricing_pair_settings.list',
    get_pricing_pair_setting_request: 'analytics.v1.pricing_pair_settings.get',
    get_many_pricing_pair_settings_request: 'analytics.v1.pricing_pair_settings.get_many',
    put_pricing_pair_setting_request: 'analytics.v1.pricing_pair_settings.put',
    put_many_pricing_pair_settings_request: 'analytics.v1.pricing_pair_settings.put_many',
    delete_pricing_pair_setting_request: 'analytics.v1.pricing_pair_settings.delete',
    delete_many_pricing_pair_settings_request: 'analytics.v1.pricing_pair_settings.delete_many',
    list_pricing_pair_setting_versions_request: 'analytics.v1.pricing_pair_settings_versions.list',
    get_pricing_pair_setting_version_request: 'analytics.v1.pricing_pair_settings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_pricing_pair_settings_request: true,
    get_pricing_pair_setting_request: true,
    get_many_pricing_pair_settings_request: true,
    put_pricing_pair_setting_request: true,
    put_many_pricing_pair_settings_request: true,
    delete_pricing_pair_setting_request: true,
    delete_many_pricing_pair_settings_request: true,
    list_pricing_pair_setting_versions_request: true,
    get_pricing_pair_setting_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.pricing_pair_settings_events.created',
    updated: 'analytics.v1.pricing_pair_settings_events.updated',
    deleted: 'analytics.v1.pricing_pair_settings_events.deleted',
} as const;
