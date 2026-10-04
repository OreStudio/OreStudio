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
import type { StressTestScenario } from '../domain/stress_test_scenario.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StressTestScenarioKey {
    id: string;
}

export interface StressTestScenarioWrite {
    id: string;
    stress_test_library_id: string;
    name: string;
    date: string | null;
    position: number;
}

export interface StressTestScenarioChange {
    write: StressTestScenarioWrite;
    precondition: Precondition;
}

export interface StressTestScenarioRemoval {
    key: StressTestScenarioKey;
    precondition: Precondition;
}

export interface StressTestScenarioLookup {
    key: StressTestScenarioKey;
    stress_test_scenario: StressTestScenario | null;
}

export interface StressTestScenariosFilter {
    id_one_of: string[] | null;
}

export interface StressTestScenarioEvent {
    event_id: string;
    key: StressTestScenarioKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StressTestScenarioVersionKey {
    stress_test_scenario: StressTestScenarioKey;
    version: number;
}

export interface StressTestScenarioVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStressTestScenariosRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestScenariosFilter | null;
}

export interface ListStressTestScenariosResponse {
    result: Result;
    stress_test_scenarios: StressTestScenario[];
    total: number;
}

export interface GetStressTestScenarioRequest {
    key: StressTestScenarioKey;
}

export interface GetStressTestScenarioResponse {
    result: Result;
    stress_test_scenario: StressTestScenario | null;
}

export interface GetManyStressTestScenariosRequest {
    keys: StressTestScenarioKey[];
}

export interface GetManyStressTestScenariosResponse {
    result: Result;
    entries: StressTestScenarioLookup[];
}

export interface PutStressTestScenarioRequest {
    change: StressTestScenarioChange;
    intent: ChangeIntent;
}

export interface PutStressTestScenarioResponse {
    result: Result;
    stress_test_scenario: StressTestScenario | null;
}

export interface PutManyStressTestScenariosRequest {
    changes: StressTestScenarioChange[];
    intent: ChangeIntent;
}

export interface PutManyStressTestScenariosResponse {
    result: Result;
    stress_test_scenarios: StressTestScenario[];
}

export interface DeleteStressTestScenarioRequest {
    removal: StressTestScenarioRemoval;
    intent: ChangeIntent;
}

export interface DeleteStressTestScenarioResponse {
    result: Result;
}

export interface DeleteManyStressTestScenariosRequest {
    removals: StressTestScenarioRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStressTestScenariosResponse {
    result: Result;
}

export interface ListStressTestScenarioVersionsRequest {
    key: StressTestScenarioKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestScenarioVersionsFilter | null;
}

export interface ListStressTestScenarioVersionsResponse {
    result: Result;
    versions: StressTestScenario[];
    total: number;
}

export interface GetStressTestScenarioVersionRequest {
    key: StressTestScenarioVersionKey;
}

export interface GetStressTestScenarioVersionResponse {
    result: Result;
    version: StressTestScenario | null;
}

export const subjects = {
    list_stress_test_scenarios_request: 'analytics.v1.stress_test_scenarios.list',
    get_stress_test_scenario_request: 'analytics.v1.stress_test_scenarios.get',
    get_many_stress_test_scenarios_request: 'analytics.v1.stress_test_scenarios.get_many',
    put_stress_test_scenario_request: 'analytics.v1.stress_test_scenarios.put',
    put_many_stress_test_scenarios_request: 'analytics.v1.stress_test_scenarios.put_many',
    delete_stress_test_scenario_request: 'analytics.v1.stress_test_scenarios.delete',
    delete_many_stress_test_scenarios_request: 'analytics.v1.stress_test_scenarios.delete_many',
    list_stress_test_scenario_versions_request: 'analytics.v1.stress_test_scenarios_versions.list',
    get_stress_test_scenario_version_request: 'analytics.v1.stress_test_scenarios_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stress_test_scenarios_request: true,
    get_stress_test_scenario_request: true,
    get_many_stress_test_scenarios_request: true,
    put_stress_test_scenario_request: true,
    put_many_stress_test_scenarios_request: true,
    delete_stress_test_scenario_request: true,
    delete_many_stress_test_scenarios_request: true,
    list_stress_test_scenario_versions_request: true,
    get_stress_test_scenario_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.stress_test_scenarios_events.created',
    updated: 'analytics.v1.stress_test_scenarios_events.updated',
    deleted: 'analytics.v1.stress_test_scenarios_events.deleted',
} as const;
