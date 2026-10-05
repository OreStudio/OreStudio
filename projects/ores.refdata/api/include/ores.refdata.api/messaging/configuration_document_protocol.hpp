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
#ifndef ORES_REFDATA_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CONFIGURATION_DOCUMENT_PROTOCOL_HPP

#include "ores.refdata.api/domain/conventions_document.hpp"
#include "ores.refdata.api/domain/curve_configuration_document.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Stores a curve configuration document.
 *
 * The session's party owns every row. The header carries the reporting
 * configuration it fills, which is what a get finds it by.
 */
struct save_curve_configuration_document_request {
    using response_type = struct save_curve_configuration_document_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_configuration_documents.save";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    domain::curve_configuration_document document;
};

/**
 * @brief The id of the header the save stored.
 */
struct save_curve_configuration_document_response {
    bool success = false;
    std::string message;
    std::string id;
};

/**
 * @brief Reads a curve configuration document by the reporting configuration it fills.
 */
struct get_curve_configuration_document_request {
    using response_type = struct get_curve_configuration_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_configuration_documents.get";
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
 * @brief A curve configuration document, when one fills the configuration.
 */
struct get_curve_configuration_document_response {
    bool success = false;
    std::string message;
    domain::curve_configuration_document document;
};

/**
 * @brief Deletes a curve configuration document that fills a reporting configuration.
 *
 * A run import's compensation sends it, to undo a save.
 */
struct delete_curve_configuration_document_request {
    using response_type = struct delete_curve_configuration_document_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_configuration_documents.delete";
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
struct delete_curve_configuration_document_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Stores a conventions document.
 *
 * The instrument conventions belong to the session's party and replace any
 * it holds under the same id. A world convention the tenant lacks is added;
 * one it holds is left as it is.
 */
struct save_conventions_document_request {
    using response_type = struct save_conventions_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_documents.save";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    domain::conventions_document document;
};

/**
 * @brief What the save did not store, and why.
 */
struct save_conventions_document_response {
    bool success = false;
    std::string message;
    /** World conventions the tenant already held, left unchanged. */
    std::vector<std::string> world_kept;
    /** FX conventions, by ORE id, which have no store yet. */
    std::vector<std::string> fx_skipped;
};

/**
 * @brief Reads every convention the party sees: its instrument conventions
 * and the tenant's world conventions.
 */
struct get_conventions_document_request {
    using response_type = struct get_conventions_document_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_documents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** When the session acts for no party, as a workflow step's does, the party whose rows to read.
     */
    std::string party_id;
};

/**
 * @brief The conventions the party sees.
 */
struct get_conventions_document_response {
    bool success = false;
    std::string message;
    domain::conventions_document document;
};

}

#endif
