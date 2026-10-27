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
     * @brief Which rule the export could not follow: document_unreadable,
     * file_name_missing or write_failed.
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

/**
 * @brief One product the file holds, as the preview states it.
 *
 * A person compares the file against the configurations they hold by the type,
 * the model and the engine, so the preview names them rather than counting
 * them alone.
 */
export interface PricingEnginesImportProduct {
    type_code: string;
    model: string;
    engine: string;
    /** How many settings this product carries, of both scopes. */
    parameters: number;
}

/**
 * @brief One row of the difference between the file and the target: added,
 * changed or removed, and in words what differs.
 *
 * =key= is the row's identity within its configuration, and it is what the
 * import's choices name, so a person acts on a row rather than on a position in
 * a list they cannot see twice.
 */
export interface PricingEnginesImportRow {
    /** The row's identity, such as =Swap#0= or =Swap#0/engine/DiscountFactor=. */
    key: string;
    /** One of product, parameter. */
    entity: string;
    /** One of added, changed, removed. */
    change: string;
    /** The product the row belongs to, or the configuration for a global setting. */
    owner: string;
    /** The setting's name, empty for a product. */
    name: string;
    what: string;
}

/**
 * @brief Something in the file the system does not know: a product type with no
 * vocabulary row, an element the tables cannot hold, or a row the file states
 * twice.
 */
export interface PricingEnginesImportIssue {
    /** One of unknown_type, dropped, duplicate. */
    kind: string;
    subject: string;
    detail: string;
}

/**
 * @brief Parses a pricing engines document and says what importing it would do.
 *
 * Nothing is written. A file of another kind is refused, and the refusal names
 * the root element found. The target is optional: a file read against no
 * target is a file for a configuration that does not exist yet, so every row it
 * holds is an addition.
 */
export interface PreviewPricingEnginesImportRequest {
    content: string;
    /** The reporting configuration to compare against, or empty for none. */
    target_configuration_id: string;
}

/**
 * @brief What the file holds, what the system does not know, and how it differs
 * from the target.
 */
export interface PreviewPricingEnginesImportResponse {
    success: boolean;
    message: string;
    root_element: string;
    in_file_products: number;
    in_file_parameters: number;
    products: PricingEnginesImportProduct[];
    /** The names of the root-level settings the file states. */
    global_parameters: string[];
    /** The configuration the file was compared against, or empty for none. */
    target_name: string;
    /** Rows the target holds with the file's values. */
    held: number;
    added: number;
    changed: number;
    /** Rows the target holds and the file omits. */
    removed: number;
    rows: PricingEnginesImportRow[];
    issues: PricingEnginesImportIssue[];
}

/**
 * @brief Writes a pricing engines document with the person's choices.
 *
 * The target is an existing configuration, named by the reporting
 * configuration it fills, or a new one, named here. A new configuration is
 * registered in reporting first and removed again when the document is
 * refused, so a refused import leaves nothing behind.
 *
 * A removed row the choices do not name is dropped, which is the file's
 * reading; the screen names every removed row it keeps, because a row a file
 * omits is usually omitted by accident.
 */
export interface ImportPricingEnginesRequest {
    content: string;
    /** The reporting configuration to write into, or empty to make a new one. */
    target_configuration_id: string;
    new_configuration_name: string;
    new_configuration_variant: string;
    /** The keys of the removed rows the person chose to keep. */
    removals_kept: string[];
    reason_code: string;
    commentary: string;
}

/**
 * @brief A row the import could not write, and the owner's reason.
 */
export interface PricingEnginesImportError {
    key: string;
    message: string;
}

/**
 * @brief What the import wrote, kept and could not write.
 */
export interface ImportPricingEnginesResponse {
    success: boolean;
    message: string;
    /** The pricing engine configuration the import wrote, as the screen names it. */
    configuration_id: string;
    configuration_name: string;
    added: number;
    changed: number;
    deleted: number;
    kept: number;
    errors: PricingEnginesImportError[];
}

export const subjects = {
    export_pricing_engines_request: 'ore.v1.ops.export_pricing_engines',
    preview_pricing_engines_import_request: 'ore.v1.ops.preview_pricing_engines_import',
    import_pricing_engines_request: 'ore.v1.ops.import_pricing_engines',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    export_pricing_engines_request: true,
    preview_pricing_engines_import_request: true,
    import_pricing_engines_request: true,
} as const;
