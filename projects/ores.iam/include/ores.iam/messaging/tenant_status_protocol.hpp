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
#ifndef ORES_IAM_MESSAGING_TENANT_STATUS_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_TENANT_STATUS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/tenant_status.hpp"

namespace ores::iam::messaging {

// ============================================================================
// Tenant Status Messages
// ============================================================================

/**
 * @brief Request to retrieve all tenant statuses.
 */
struct get_tenant_statuses_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_statuses_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_statuses_request& v);

/**
 * @brief Response containing all tenant statuses.
 */
struct get_tenant_statuses_response final {
    std::vector<domain::tenant_status> statuses;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_statuses_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_statuses_response& v);

/**
 * @brief Request to save a tenant status (create or update).
 */
struct save_tenant_status_request final {
    domain::tenant_status status;

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_status_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_status_request& v);

/**
 * @brief Response confirming tenant status save operation.
 */
struct save_tenant_status_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_tenant_status_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_tenant_status_response& v);

/**
 * @brief Result for a single tenant status deletion.
 */
struct delete_tenant_status_result final {
    std::string status;  ///< Text primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_status_result& v);

/**
 * @brief Request to delete one or more tenant statuses.
 */
struct delete_tenant_status_request final {
    std::vector<std::string> statuses;  ///< Text primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_status_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_status_request& v);

/**
 * @brief Response confirming tenant status deletion(s).
 */
struct delete_tenant_status_response final {
    std::vector<delete_tenant_status_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_tenant_status_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_tenant_status_response& v);

/**
 * @brief Request to retrieve version history for a tenant status.
 */
struct get_tenant_status_history_request final {
    std::string status;  ///< Text primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_status_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_status_history_request& v);

/**
 * @brief Response containing tenant status version history.
 */
struct get_tenant_status_history_response final {
    bool success;
    std::string message;
    std::vector<domain::tenant_status> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_tenant_status_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_tenant_status_history_response& v);

}

namespace ores::comms::messaging {

// Tenant Status traits
template<>
struct message_traits<iam::messaging::get_tenant_statuses_request> {
    using request_type = iam::messaging::get_tenant_statuses_request;
    using response_type = iam::messaging::get_tenant_statuses_response;
    static constexpr message_type request_message_type =
        message_type::get_tenant_statuses_request;
};

template<>
struct message_traits<iam::messaging::save_tenant_status_request> {
    using request_type = iam::messaging::save_tenant_status_request;
    using response_type = iam::messaging::save_tenant_status_response;
    static constexpr message_type request_message_type =
        message_type::save_tenant_status_request;
};

template<>
struct message_traits<iam::messaging::delete_tenant_status_request> {
    using request_type = iam::messaging::delete_tenant_status_request;
    using response_type = iam::messaging::delete_tenant_status_response;
    static constexpr message_type request_message_type =
        message_type::delete_tenant_status_request;
};

template<>
struct message_traits<iam::messaging::get_tenant_status_history_request> {
    using request_type = iam::messaging::get_tenant_status_history_request;
    using response_type = iam::messaging::get_tenant_status_history_response;
    static constexpr message_type request_message_type =
        message_type::get_tenant_status_history_request;
};

}

#endif
