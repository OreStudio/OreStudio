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
/**
 * @brief One file of an ORE input directory.
 *
 * The name is the one the run document gives the file; the run document
 * itself is ore.xml.
 */
export interface RunInputFile {
    name: string;
    content: string;
}

/**
 * @brief A configuration document an owner stored, by the configuration it
 * fills, so an undo can delete it.
 */
export interface SavedDocument {
    configuration_type_code: string;
    configuration_id: string;
}

/**
 * @brief Starts importing an ORE input directory into a report definition.
 *
 * The definition must hold no run document yet, and the session must act for
 * the party that owns it.
 */
export interface ImportRunConfigurationRequest {
    report_definition_id: string;
    /** Prefixes each configuration the import creates, as name/file. */
    name: string;
    files: RunInputFile[];
}

/**
 * @brief The workflow running the import, whose status the caller follows.
 */
export interface ImportRunConfigurationResponse {
    success: boolean;
    message: string;
    correlation_id: string;
    workflow_instance_id: string;
}

/**
 * @brief The import's execute step, which the workflow engine sends.
 *
 * Carries the caller's token so the step stores each document as the caller.
 */
export interface RunConfigurationImportExecuteRequest {
    report_definition_id: string;
    name: string;
    files: RunInputFile[];
    correlation_id: string;
    /** The caller's JWT, which the step delegates to the owners. */
    bearer_token: string;
}

/**
 * @brief What the execute step stored, travelling as the step's result.
 *
 * It names what was stored, so a compensation can delete it.
 */
export interface RunConfigurationImportExecuteResult {
    success: boolean;
    message: string;
    /** The files stored, run document first. */
    stored: string[];
    /** Input files that hold no configuration the owners keep. */
    not_stored: string[];
    /** World conventions the tenant already held, left unchanged. */
    world_conventions_kept: string[];
    /** FX conventions, which have no store yet. */
    fx_conventions_skipped: string[];
    report_definition_id: string;
    /** Whether reporting stored the run document, which the undo deletes. */
    run_document_saved: boolean;
    /** The documents the owners stored, which the undo deletes. */
    saved_documents: SavedDocument[];
}

/**
 * @brief Deletes what an import stored.
 *
 * The workflow engine sends it as the import's compensation. Deleting what is
 * already gone succeeds, so it can run twice.
 */
export interface RunConfigurationImportRollbackRequest {
    correlation_id: string;
    /** The caller's JWT, which the rollback delegates to the owners. */
    bearer_token: string;
    report_definition_id: string;
    /** Whether reporting stored the run document, which the undo deletes. */
    run_document_saved: boolean;
    /** The documents the owners stored, which the undo deletes. */
    saved_documents: SavedDocument[];
}

/**
 * @brief Rebuilds a report definition's ORE input directory from the owners.
 */
export interface ExportRunConfigurationRequest {
    report_definition_id: string;
}

/**
 * @brief The run document and every configuration document the definition binds.
 */
export interface ExportRunConfigurationResponse {
    success: boolean;
    message: string;
    files: RunInputFile[];
}

export const subjects = {
    import_run_configuration_request: 'ore.v1.run_configuration.import',
    run_configuration_import_execute_request: 'ore.v1.run_configuration.import.execute',
    run_configuration_import_rollback_request: 'ore.v1.run_configuration.import.rollback',
    export_run_configuration_request: 'ore.v1.run_configuration.export',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    import_run_configuration_request: true,
    run_configuration_import_execute_request: true,
    run_configuration_import_rollback_request: true,
    export_run_configuration_request: true,
} as const;
