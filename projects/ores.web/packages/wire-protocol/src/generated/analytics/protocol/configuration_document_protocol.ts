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
import type { PricingModelConfig } from '../domain/pricing_model_config.js';
import type { PricingModelProduct } from '../domain/pricing_model_product.js';
import type { PricingModelProductParameter } from '../domain/pricing_model_product_parameter.js';
import type { TodaysMarketCollection } from '../domain/todays_market_collection.js';
import type { TodaysMarketConfig } from '../domain/todays_market_config.js';
import type { TodaysMarketConfiguration } from '../domain/todays_market_configuration.js';
import type { TodaysMarketConfigurationBinding } from '../domain/todays_market_configuration_binding.js';
import type { TodaysMarketEntry } from '../domain/todays_market_entry.js';

/**
 * @brief One ORE pricing engines document as the rows analytics stores.
 */
export interface PricingEnginesDocument {
    config: PricingModelConfig;
    products: PricingModelProduct[];
    parameters: PricingModelProductParameter[];
}

/**
 * @brief One ORE today's market document as the rows analytics stores.
 */
export interface TodaysMarketDocument {
    config: TodaysMarketConfig;
    collections: TodaysMarketCollection[];
    entries: TodaysMarketEntry[];
    configurations: TodaysMarketConfiguration[];
    bindings: TodaysMarketConfigurationBinding[];
}

/**
 * @brief Stores a pricing engines document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
export interface SavePricingEnginesDocumentRequest {
    document: PricingEnginesDocument;
}

/**
 * @brief The id of the header the save stored.
 */
export interface SavePricingEnginesDocumentResponse {
    success: boolean;
    message: string;
    id: string;
}

/**
 * @brief Reads a pricing engines document by the reporting configuration it fills.
 */
export interface GetPricingEnginesDocumentRequest {
    configuration_id: string;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read. */
    party_id: string;
}

/**
 * @brief A pricing engines document, when one fills the configuration.
 */
export interface GetPricingEnginesDocumentResponse {
    success: boolean;
    message: string;
    document: PricingEnginesDocument;
}

/**
 * @brief Deletes a pricing engines document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
export interface DeletePricingEnginesDocumentRequest {
    configuration_id: string;
}

/**
 * @brief Whether the delete succeeded.
 */
export interface DeletePricingEnginesDocumentResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Stores a today's market document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
export interface SaveTodaysMarketDocumentRequest {
    document: TodaysMarketDocument;
}

/**
 * @brief The id of the header the save stored.
 */
export interface SaveTodaysMarketDocumentResponse {
    success: boolean;
    message: string;
    id: string;
}

/**
 * @brief Reads a today's market document by the reporting configuration it fills.
 */
export interface GetTodaysMarketDocumentRequest {
    configuration_id: string;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read. */
    party_id: string;
}

/**
 * @brief A today's market document, when one fills the configuration.
 */
export interface GetTodaysMarketDocumentResponse {
    success: boolean;
    message: string;
    document: TodaysMarketDocument;
}

/**
 * @brief Deletes a today's market document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
export interface DeleteTodaysMarketDocumentRequest {
    configuration_id: string;
}

/**
 * @brief Whether the delete succeeded.
 */
export interface DeleteTodaysMarketDocumentResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    save_pricing_engines_document_request: 'analytics.v1.pricing_engines_documents.save',
    get_pricing_engines_document_request: 'analytics.v1.pricing_engines_documents.get',
    delete_pricing_engines_document_request: 'analytics.v1.pricing_engines_documents.delete',
    save_todays_market_document_request: 'analytics.v1.todays_market_documents.save',
    get_todays_market_document_request: 'analytics.v1.todays_market_documents.get',
    delete_todays_market_document_request: 'analytics.v1.todays_market_documents.delete',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    save_pricing_engines_document_request: true,
    get_pricing_engines_document_request: true,
    delete_pricing_engines_document_request: true,
    save_todays_market_document_request: true,
    get_todays_market_document_request: true,
    delete_todays_market_document_request: true,
} as const;
