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
#ifndef ORES_REFDATA_API_MESSAGING_CONVENTIONS_IMPORT_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CONVENTIONS_IMPORT_OPERATIONS_PROTOCOL_HPP

#include "ores.refdata.api/domain/conventions_import.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief What a commit did with one staged family.
 */
struct conventions_import_family_outcome {
    std::string family;
    int sequence = 0;
    bool applied = false;
    std::string message;
};

/**
 * @brief Opens a staging session for a conventions bundle.
 *
 * The caller supplies the id, so a retried begin names the same import. The
 * service stamps the session's party on the row.
 */
struct begin_conventions_import_request {
    using response_type = struct begin_conventions_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.begin_conventions_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid id;
    std::optional<std::string> source;
    std::optional<int> families_expected;
    std::optional<std::chrono::system_clock::time_point> expires_at;
    /**
     * Refused when set: the service states the state, never the caller.
     */
    std::optional<std::string> state;
};

/**
 * @brief The import that is open for staging.
 */
struct begin_conventions_import_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::conventions_import import_row;
};

/**
 * @brief Stages one family's payload under an open import.
 *
 * The payload is the family's own generated value type, serialised as JSON.
 */
struct stage_conventions_family_request {
    using response_type = struct stage_conventions_family_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.stage_conventions_family";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid import_id;
    std::string family;
    int sequence = 0;
    std::string payload;
    std::optional<std::string> checksum;
};

/**
 * @brief Whether the stage wrote its row.
 */
struct stage_conventions_family_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Applies every staged family of one import in one transaction.
 */
struct commit_conventions_import_request {
    using response_type = struct commit_conventions_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.commit_conventions_import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid import_id;
};

/**
 * @brief What the commit stored, and what it did not.
 *
 * The world conventions the tenant already held are kept, as the document save
 * keeps them. The family outcomes list one entry per applied staged family.
 */
struct commit_conventions_import_response {
    bool success = false;
    std::string message;
    std::vector<std::string> world_kept;
    std::vector<std::string> fx_skipped;
    std::vector<conventions_import_family_outcome> families;
};

}

#endif
