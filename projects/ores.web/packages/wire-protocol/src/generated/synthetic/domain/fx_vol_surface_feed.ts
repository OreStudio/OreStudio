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
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The FX volatility surface feed wire shape.
 *
 * Field names are the C++ member names, because they are the keys the codec
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface FxVolSurfaceFeed {
    version: number;
    tenant_id: string;
    id: string;
    party_id: string;
    config_id: string;
    base_currency_code: string;
    quote_currency_code: string;
    source_name: string;
    ticks_per_hour: number;
    enabled: boolean;
    auto_start: boolean;
    folder_id: string | null;
    asset_class_code: string;
    arrival_mode: string;
    branching_ratio: number;
    decay: number;
    sensitivity: number;
    skew_reversion: number;
    skew_volatility: number;
    curvature_reversion: number;
    curvature_volatility: number;
    spot_variance_correlation: number;
    spot_skew_correlation: number;
    segment_correlation: number;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
