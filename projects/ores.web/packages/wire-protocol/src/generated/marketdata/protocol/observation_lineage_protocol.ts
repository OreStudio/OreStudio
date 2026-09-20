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
import type { ObservationLineage } from '../domain/observation_lineage.js';

export interface GetObservationLineagesRequest {
    offset: number;
    limit: number;
}

export interface GetObservationLineagesResponse {
    observation_lineages: ObservationLineage[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveObservationLineageRequest {
    data: ObservationLineage;
}

export interface SaveObservationLineageResponse {
    success: boolean;
    message: string;
}

export interface DeleteObservationLineageRequest {
    ids: string[];
}

export interface DeleteObservationLineageResponse {
    success: boolean;
    message: string;
}

export interface GetObservationLineageHistoryRequest {
    id: string;
}

export interface GetObservationLineageHistoryResponse {
    history: ObservationLineage[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_observation_lineages_request: "marketdata.v1.observation_lineages.list",
    save_observation_lineage_request: "marketdata.v1.observation_lineages.save",
    delete_observation_lineage_request: "marketdata.v1.observation_lineages.delete",
    get_observation_lineage_history_request: "marketdata.v1.observation_lineages.history",
} as const;
