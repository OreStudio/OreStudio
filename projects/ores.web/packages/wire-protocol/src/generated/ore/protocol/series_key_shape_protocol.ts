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
import type { SeriesKeyShape } from '../domain/series_key_shape.js';

export interface GetSeriesKeyShapesRequest {
    offset: number;
    limit: number;
}

export interface GetSeriesKeyShapesResponse {
    shapes: SeriesKeyShape[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveSeriesKeyShapeRequest {
    data: SeriesKeyShape;
}

export interface SaveSeriesKeyShapeResponse {
    success: boolean;
    message: string;
}

export interface DeleteSeriesKeyShapeRequest {
    series_types: string[];
}

export interface DeleteSeriesKeyShapeResponse {
    success: boolean;
    message: string;
}

export interface GetSeriesKeyShapeHistoryRequest {
    series_type: string;
}

export interface GetSeriesKeyShapeHistoryResponse {
    history: SeriesKeyShape[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_series_key_shapes_request: "ore.v1.series_key_shapes.list",
    save_series_key_shape_request: "ore.v1.series_key_shapes.save",
    delete_series_key_shape_request: "ore.v1.series_key_shapes.delete",
    get_series_key_shape_history_request: "ore.v1.series_key_shapes.history",
} as const;
