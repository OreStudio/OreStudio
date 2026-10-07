/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ANALYTICS_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP

#include "ores.analytics.api/domain/pricing_model_config.hpp"
#include "ores.analytics.api/domain/pricing_model_product.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp"
#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/domain/todays_market_config.hpp"
#include "ores.analytics.api/domain/todays_market_configuration.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding.hpp"
#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::analytics::messaging {

/**
 * @brief One ORE pricing engines document as the rows analytics stores.
 */
struct pricing_engines_document {
    ores::analytics::domain::pricing_model_config config;
    std::vector<ores::analytics::domain::pricing_model_product> products;
    std::vector<ores::analytics::domain::pricing_model_product_parameter> parameters;
};

/**
 * @brief One ORE today's market document as the rows analytics stores.
 */
struct todays_market_document {
    ores::analytics::domain::todays_market_config config;
    std::vector<ores::analytics::domain::todays_market_collection> collections;
    std::vector<ores::analytics::domain::todays_market_entry> entries;
    std::vector<ores::analytics::domain::todays_market_configuration> configurations;
    std::vector<ores::analytics::domain::todays_market_configuration_binding> bindings;
};

/**
 * @brief Stores a pricing engines document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_pricing_engines_document_request {
    using response_type = struct save_pricing_engines_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_engines_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    pricing_engines_document document;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_pricing_engines_document_response {
    bool success = false;
    std::string message;
    std::string id;
};

/**
 * @brief Reads a pricing engines document by the reporting configuration it fills.
 */
struct get_pricing_engines_document_request {
    using response_type = struct get_pricing_engines_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.pricing_engines_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;
};

/**
 * @brief A pricing engines document, when one fills the configuration.
 */
struct get_pricing_engines_document_response {
    bool success = false;
    std::string message;
    pricing_engines_document document;
};

/**
 * @brief Deletes a pricing engines document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
struct delete_pricing_engines_document_request {
    using response_type = struct delete_pricing_engines_document_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.pricing_engines_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_pricing_engines_document_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Stores a today's market document.
 *
 * The session must act for a party, which owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_todays_market_document_request {
    using response_type = struct save_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_document document;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_todays_market_document_response {
    bool success = false;
    std::string message;
    std::string id;
};

/**
 * @brief Reads a today's market document by the reporting configuration it fills.
 */
struct get_todays_market_document_request {
    using response_type = struct get_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;
};

/**
 * @brief A today's market document, when one fills the configuration.
 */
struct get_todays_market_document_response {
    bool success = false;
    std::string message;
    todays_market_document document;
};

/**
 * @brief Deletes a today's market document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save. Deleting a
 * configuration no document fills succeeds, so a compensation can run twice.
 */
struct delete_todays_market_document_request {
    using response_type = struct delete_todays_market_document_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_documents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_id;
};

/**
 * @brief Whether the delete succeeded.
 */
struct delete_todays_market_document_response {
    bool success = false;
    std::string message;
};

}

#endif
