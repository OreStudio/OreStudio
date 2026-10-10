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
#ifndef ORES_REFDATA_API_MESSAGING_BOOK_PROPOSAL_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BOOK_PROPOSAL_OPERATIONS_PROTOCOL_HPP

#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones
 * the line changes, which is what the policy reads to name the parts.
 */
struct book_line_outcome {
    int line_no;
    std::string operation;
    boost::uuids::uuid entity_id;
    std::vector<std::string> columns;
    std::string refusal;
};

/**
 * @brief Runs each proposed line through the real write and writes nothing.
 *
 * A put line needs the permission to write books, a delete line the permission
 * to delete them. The lines are checked in order inside one transaction that is
 * never committed.
 */
struct preview_book_changes_request {
    using response_type = struct preview_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.preview_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The proposed writes. The request, line number and part of a line are
     * not read: the server numbers the lines and the policy names the parts.
     */
    std::vector<ores::refdata::domain::book_change> lines;
};

struct preview_book_changes_response {
    ores::utility::domain::result result;
    std::vector<book_line_outcome> lines;
    /**
     * @brief The parts the policy needs for the lines together. Empty when no
     * policy row gates them.
     */
    std::vector<std::string> part_codes;
};

/**
 * @brief Holds the proposed lines in an approval request.
 *
 * Previews first. A refused line, or lines no policy row gates, raise nothing.
 */
struct raise_book_changes_request {
    using response_type = struct raise_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.raise_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;
    std::vector<ores::refdata::domain::book_change> lines;
};

struct raise_book_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The id of the request as raised, when the outcome is ok. The inbox
     * operations read the request itself.
     */
    std::optional<boost::uuids::uuid> request_id;
    std::vector<book_line_outcome> lines;
    std::vector<std::string> part_codes;
};

}

#endif
