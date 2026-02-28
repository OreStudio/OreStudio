/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_VARIABILITY_MESSAGING_FEATURE_FLAGS_PROTOCOL_HPP
#define ORES_VARIABILITY_MESSAGING_FEATURE_FLAGS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/save_result.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.variability/domain/feature_flags.hpp"

namespace ores::variability::messaging {

/**
 * @brief Request to retrieve all feature flags.
 */
struct get_feature_flags_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_feature_flags_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_feature_flags_request& v);

/**
 * @brief Response containing all feature flags.
 */
struct get_feature_flags_response final {
    std::vector<domain::feature_flags> feature_flags;

    std::vector<std::byte> serialize() const;
    static std::expected<get_feature_flags_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_feature_flags_response& v);

/**
 * @brief Request to save (create or update) one or more feature flags.
 *
 * Includes change tracking fields for audit trail.
 */
struct save_feature_flag_request final {
    std::vector<domain::feature_flags> flags;

    static save_feature_flag_request from(domain::feature_flags flag);
    static save_feature_flag_request from(std::vector<domain::feature_flags> flags);

    std::vector<std::byte> serialize() const;
    static std::expected<save_feature_flag_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_feature_flag_request& v);

/**
 * @brief Response to save feature flag request.
 */
struct save_feature_flag_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_feature_flag_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_feature_flag_response& v);

/**
 * @brief Request to delete a feature flag by name.
 */
struct delete_feature_flag_request final {
    std::string name;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_feature_flag_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_feature_flag_request& v);

/**
 * @brief Response to delete feature flag request.
 */
struct delete_feature_flag_response final {
    bool success = false;
    std::string error_message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_feature_flag_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_feature_flag_response& v);

/**
 * @brief Request to retrieve feature flag history by name.
 */
struct get_feature_flag_history_request final {
    std::string name;

    std::vector<std::byte> serialize() const;
    static std::expected<get_feature_flag_history_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_feature_flag_history_request& v);

/**
 * @brief Response containing feature flag history.
 */
struct get_feature_flag_history_response final {
    bool success = false;
    std::string message;
    std::vector<domain::feature_flags> history;  // Ordered newest-first

    std::vector<std::byte> serialize() const;
    static std::expected<get_feature_flag_history_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_feature_flag_history_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_feature_flags_request.
 */
template<>
struct message_traits<variability::messaging::get_feature_flags_request> {
    using request_type = variability::messaging::get_feature_flags_request;
    using response_type = variability::messaging::get_feature_flags_response;
    static constexpr message_type request_message_type =
        message_type::get_feature_flags_request;
};

/**
 * @brief Message traits specialization for save_feature_flag_request.
 */
template<>
struct message_traits<variability::messaging::save_feature_flag_request> {
    using request_type = variability::messaging::save_feature_flag_request;
    using response_type = variability::messaging::save_feature_flag_response;
    static constexpr message_type request_message_type =
        message_type::save_feature_flag_request;
};

/**
 * @brief Message traits specialization for delete_feature_flag_request.
 */
template<>
struct message_traits<variability::messaging::delete_feature_flag_request> {
    using request_type = variability::messaging::delete_feature_flag_request;
    using response_type = variability::messaging::delete_feature_flag_response;
    static constexpr message_type request_message_type =
        message_type::delete_feature_flag_request;
};

/**
 * @brief Message traits specialization for get_feature_flag_history_request.
 */
template<>
struct message_traits<variability::messaging::get_feature_flag_history_request> {
    using request_type = variability::messaging::get_feature_flag_history_request;
    using response_type = variability::messaging::get_feature_flag_history_response;
    static constexpr message_type request_message_type =
        message_type::get_feature_flag_history_request;
};

}

#endif
