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
#ifndef ORES_DQ_API_MESSAGING_BADGE_DEFINITION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_DEFINITION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Badge Definition Messages
// ============================================================================

/**
 * @brief Request to retrieve all badge definitions.
 */
struct get_badge_definitions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_badge_definitions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_badge_definitions_request& v);

/**
 * @brief Response containing all badge definitions.
 */
struct get_badge_definitions_response final {
    std::vector<domain::badge_definition> definitions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_badge_definitions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_badge_definitions_response& v);

/**
 * @brief Request to save a badge definition (create or update).
 */
struct save_badge_definition_request final {
    domain::badge_definition definition;

    std::vector<std::byte> serialize() const;
    static std::expected<save_badge_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_badge_definition_request& v);

/**
 * @brief Response confirming badge definition save operation.
 */
struct save_badge_definition_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_badge_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_badge_definition_response& v);

/**
 * @brief Result for a single badge definition deletion.
 */
struct delete_badge_definition_result final {
    std::string code;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_result& v);

/**
 * @brief Request to delete one or more badge definitions.
 */
struct delete_badge_definition_request final {
    std::vector<std::string> codes;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_badge_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_request& v);

/**
 * @brief Response confirming badge definition deletion(s).
 */
struct delete_badge_definition_response final {
    std::vector<delete_badge_definition_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_badge_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_response& v);

/**
 * @brief Request to retrieve version history for a badge definition.
 */
struct get_badge_definition_history_request final {
    std::string code;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_badge_definition_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_badge_definition_history_request& v);

/**
 * @brief Response containing badge definition version history.
 */
struct get_badge_definition_history_response final {
    bool success;
    std::string message;
    std::vector<domain::badge_definition> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_badge_definition_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_badge_definition_history_response& v);

}

#endif
