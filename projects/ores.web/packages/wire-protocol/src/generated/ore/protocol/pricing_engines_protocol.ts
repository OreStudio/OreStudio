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
import type { OreDocument } from '../../../generated/ore/protocol/ore_document_protocol.js';

/**
 * @brief One reason the export is refused, named so a client can act on it.
 *
 * A refusal reports every reason it found, not the first, so a person fixes
 * them in one pass.
 */
export interface PricingEnginesExportProblem {
    /**
     * @brief Which rule the export could not follow: configuration_missing,
     * document_missing, file_name_missing or write_failed.
     */
    code: string;
    message: string;
}

/**
 * @brief Writes one pricing engine configuration as an ORE pricing engines
 * document.
 *
 * The configuration is named by the reporting configuration it fills, which is
 * what analytics' own read finds a document by.
 */
export interface ExportPricingEnginesRequest {
    configuration_id: string;
}

/**
 * @brief The document, one file, and how many products it holds.
 */
export interface ExportPricingEnginesResponse {
    success: boolean;
    message: string;
    count: number;
    files: OreDocument[];
    problems: PricingEnginesExportProblem[];
}

export const subjects = {
    export_pricing_engines_request: 'ore.v1.ops.export_pricing_engines',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    export_pricing_engines_request: true,
} as const;
