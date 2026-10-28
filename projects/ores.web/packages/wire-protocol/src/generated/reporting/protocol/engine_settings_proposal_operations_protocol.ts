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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief One setting of a group beside the value in force.
 *
 * The values are text whatever the setting's kind, because a screen draws them
 * and does not compute with them. An empty before or after is the engine
 * default for that setting.
 */
export interface EngineSettingsValueChange {
    column_name: string;
    ore_name: string;
    before: string;
    after: string;
    changed: boolean;
}

/**
 * @brief One proposed write to a group, as a decider reads it: the group, the
 * configuration it belongs to, the version the maker read, and each setting
 * beside the value in force.
 */
export interface EngineSettingsChangeLine {
    line_no: number;
    group_code: string;
    engine_settings_id: string;
    base_version: number;
    applied: boolean;
    fields: EngineSettingsValueChange[];
}

/**
 * @brief One value a person proposes for one setting of a group.
 *
 * The value is the setting's own words: true or false for a flag, its digits
 * for a whole, and one of the words its setting names for a choice. An empty
 * value unsets the setting, which is the engine default.
 */
export interface EngineSettingsValue {
    setting_code: string;
    text_value: string;
}

/**
 * @brief Holds a proposed change to a group in an approval request.
 *
 * A configuration that no report binds is a draft, and its groups change in
 * place, so a proposal here is refused and the caller writes the group
 * directly. A group that changes no numbers is refused the same way, because
 * the execution group is operational and lands directly.
 *
 * The proposal names the parts its group needs: the kind names one part for
 * each number-changing group, and the request takes the part of the group the
 * proposal writes.
 */
export interface RaiseEngineSettingsChangeRequest {
    engine_settings_id: string;
    group_code: string;
    /**
     * @brief The version of the group's row the person read, or zero for a group
     * the configuration had not set.
     */
    base_version: number;
    /**
     * @brief The values proposed for the group, one per setting it holds.
     *
     * Every setting of the group travels, so the proposal states the whole group
     * and the apply writes it whole. A setting the person left alone travels with
     * the value in force, which is what makes the proposal a proposal of the group
     * rather than of the settings that changed.
     */
    values: EngineSettingsValue[];
    /**
     * @brief Why the person asks, in their words.
     */
    reason: string;
}

export interface RaiseEngineSettingsChangeResponse {
    result: Result;
    /**
     * @brief The request the proposal was raised as, empty when none was.
     */
    request_id: string;
}

/**
 * @brief Reads the groups a request proposes to change, each beside the value
 * in force.
 *
 * Answered to the person who raised the request, to whoever may decide its
 * kind, and to whoever may read the tenant's requests. Answered as not found
 * otherwise, as opening the request is.
 */
export interface GetEngineSettingsChangesRequest {
    request_id: string;
}

export interface GetEngineSettingsChangesResponse {
    result: Result;
    /**
     * @brief The proposed groups, in the order the proposal holds them.
     */
    changes: EngineSettingsChangeLine[];
}

/**
 * @brief Reads the proposal against the version in force and writes nothing.
 *
 * The group may have moved since the proposal was raised, so the checks a
 * decider reads are read now. Answered to the same callers as the lines are.
 */
export interface RecheckEngineSettingsChangesRequest {
    request_id: string;
}

export interface RecheckEngineSettingsChangesResponse {
    result: Result;
    changes: EngineSettingsChangeLine[];
    /**
     * @brief One refusal for each group, in the same order as the changes, empty
     * when the apply would stand.
     */
    refusals: string[];
    part_codes: string[];
}

export const subjects = {
    raise_engine_settings_change_request: 'reporting.v1.ops.raise_engine_settings_change',
    get_engine_settings_changes_request: 'reporting.v1.ops.get_engine_settings_changes',
    recheck_engine_settings_changes_request: 'reporting.v1.ops.recheck_engine_settings_changes',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    raise_engine_settings_change_request: true,
    get_engine_settings_changes_request: true,
    recheck_engine_settings_changes_request: true,
} as const;
