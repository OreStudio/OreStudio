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
import type { ReportAnalytic } from '../domain/report_analytic.js';
import type { ReportMarketBinding } from '../domain/report_market_binding.js';
import type { ReportRunSetup } from '../domain/report_run_setup.js';

/**
 * @brief One parameter of an analytic, by the name the run document uses.
 *
 * The stored parameter holds a definition id and a value, and the id is a
 * database concern. The document is one step earlier, where the parameter is
 * still the name ORE wrote, so the name rides beside the value and the store
 * resolves it.
 */
export interface RunParameter {
    name: string;
    value: string;
    position: number;
}

/**
 * @brief An analytic of the run and the parameters it sets.
 *
 * The active flag is a parameter in ORE's schema and a column on the analytic,
 * so it lives on the analytic; the remaining parameters stay beside it, in the
 * order the document wrote them.
 */
export interface RunAnalytic {
    analytic: ReportAnalytic;
    parameters: RunParameter[];
}

/**
 * @brief One ORE run document as reporting stores it against a report
 * definition: the setup, the analytics with their parameters, and the market
 * bindings.
 */
export interface RunDocument {
    setup: ReportRunSetup;
    analytics: RunAnalytic[];
    market_bindings: ReportMarketBinding[];
}

/**
 * @brief Stores a run document against a report definition.
 *
 * Refused when the definition already holds one, or when an analytic sets a
 * parameter no definition describes.
 */
export interface SaveRunDocumentRequest {
    report_definition_id: string;
    document: RunDocument;
}

/**
 * @brief Whether the save succeeded.
 */
export interface SaveRunDocumentResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Creates a configuration of a type and binds it to the definition in
 * that type's slot.
 */
export interface BindConfigurationRequest {
    report_definition_id: string;
    configuration_type_code: string;
    name: string;
}

/**
 * @brief The configuration the binding created.
 */
export interface BindConfigurationResponse {
    success: boolean;
    message: string;
    configuration_id: string;
}

/**
 * @brief Reads a definition's run document and its bindings.
 */
export interface GetRunDocumentRequest {
    report_definition_id: string;
}

/**
 * @brief The run document, the slots it fills, and the party that owns it.
 */
export interface GetRunDocumentResponse {
    success: boolean;
    message: string;
    document: RunDocument;
    bindings: string[];
    /** The party that owns the definition, whose configuration documents the run reads. */
    party_id: string;
}

/**
 * @brief Deletes a definition's run document, its bindings and the
 * configuration rows they name.
 *
 * A run import's compensation sends it, to undo a save.
 */
export interface DeleteRunDocumentRequest {
    report_definition_id: string;
}

/**
 * @brief Whether the delete succeeded.
 */
export interface DeleteRunDocumentResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    save_run_document_request: 'reporting.v1.run_documents.save',
    bind_configuration_request: 'reporting.v1.run_documents.bind',
    get_run_document_request: 'reporting.v1.run_documents.get',
    delete_run_document_request: 'reporting.v1.run_documents.delete',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    save_run_document_request: true,
    bind_configuration_request: true,
    get_run_document_request: true,
    delete_run_document_request: true,
} as const;
