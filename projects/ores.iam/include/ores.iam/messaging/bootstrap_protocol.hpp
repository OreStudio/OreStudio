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
#ifndef ORES_IAM_MESSAGING_BOOTSTRAP_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_BOOTSTRAP_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to create the initial administrator account.
 *
 * This endpoint is only available during bootstrap mode (when no admin accounts exist).
 * It must be called from localhost only.
 */
struct create_initial_admin_request final {
    std::string username;
    std::string password;
    std::string email;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 2 bytes: username length
     * - N bytes: username (UTF-8)
     * - 2 bytes: password length
     * - N bytes: password (UTF-8)
     * - 2 bytes: email length
     * - N bytes: email (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<create_initial_admin_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_initial_admin_request& v);

/**
 * @brief Response from creating the initial administrator account.
 */
struct create_initial_admin_response final {
    bool success;
    std::string error_message;
    boost::uuids::uuid account_id;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: success (boolean)
     * - 2 bytes: error_message length
     * - N bytes: error_message (UTF-8)
     * - 16 bytes: account_id (UUID)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<create_initial_admin_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const create_initial_admin_response& v);

/**
 * @brief Request to check the bootstrap mode status.
 *
 * This endpoint is always available and returns whether the system is in
 * bootstrap mode (waiting for initial admin) or secure mode (admin exists).
 */
struct bootstrap_status_request final {
    /**
     * @brief Serialize request to bytes.
     *
     * Format: Empty (no data)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<bootstrap_status_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const bootstrap_status_request& v);

/**
 * @brief Response containing the bootstrap mode status.
 */
struct bootstrap_status_response final {
    bool is_in_bootstrap_mode;
    std::string message;

    /**
     * @brief Serialize response to bytes.
     *
     * Format:
     * - 1 byte: is_in_bootstrap_mode (boolean)
     * - 2 bytes: message length
     * - N bytes: message (UTF-8)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize response from bytes.
     */
    static std::expected<bootstrap_status_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const bootstrap_status_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for create_initial_admin_request.
 */
template<>
struct message_traits<iam::messaging::create_initial_admin_request> {
    using request_type = iam::messaging::create_initial_admin_request;
    using response_type = iam::messaging::create_initial_admin_response;
    static constexpr message_type request_message_type =
        message_type::create_initial_admin_request;
};

/**
 * @brief Message traits specialization for bootstrap_status_request.
 */
template<>
struct message_traits<iam::messaging::bootstrap_status_request> {
    using request_type = iam::messaging::bootstrap_status_request;
    using response_type = iam::messaging::bootstrap_status_response;
    static constexpr message_type request_message_type =
        message_type::bootstrap_status_request;
};

}

#endif
