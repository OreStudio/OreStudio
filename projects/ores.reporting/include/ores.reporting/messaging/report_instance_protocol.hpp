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
#ifndef ORES_REPORTING_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.reporting/domain/report_instance.hpp"

namespace ores::reporting::messaging {

// ============================================================================
// Report Instance Messages
// ============================================================================

/**
 * @brief Request to retrieve all report instances.
 */
struct get_report_instances_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_report_instances_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_instances_request& v);

/**
 * @brief Response containing all report instances.
 */
struct get_report_instances_response final {
    std::vector<domain::report_instance> instances;

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_instances_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_instances_response& v);

/**
 * @brief Request to save a report instance (create or update).
 */
struct save_report_instance_request final {
    domain::report_instance instance;

    std::vector<std::byte> serialize() const;
    static std::expected<save_report_instance_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_report_instance_request& v);

/**
 * @brief Response confirming report instance save operation.
 */
struct save_report_instance_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_report_instance_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_report_instance_response& v);

/**
 * @brief Result for a single report instance deletion.
 */
struct delete_report_instance_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_report_instance_result& v);

/**
 * @brief Request to delete one or more report instances.
 */
struct delete_report_instance_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_report_instance_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_report_instance_request& v);

/**
 * @brief Response confirming report instance deletion(s).
 */
struct delete_report_instance_response final {
    std::vector<delete_report_instance_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_report_instance_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_report_instance_response& v);

/**
 * @brief Request to retrieve version history for a report instance.
 */
struct get_report_instance_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_instance_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_instance_history_request& v);

/**
 * @brief Response containing report instance version history.
 */
struct get_report_instance_history_response final {
    bool success;
    std::string message;
    std::vector<domain::report_instance> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_instance_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_instance_history_response& v);

}

namespace ores::comms::messaging {

// Report Instance traits
template<>
struct message_traits<reporting::messaging::get_report_instances_request> {
    using request_type = reporting::messaging::get_report_instances_request;
    using response_type = reporting::messaging::get_report_instances_response;
    static constexpr message_type request_message_type =
        message_type::get_report_instances_request;
};

template<>
struct message_traits<reporting::messaging::save_report_instance_request> {
    using request_type = reporting::messaging::save_report_instance_request;
    using response_type = reporting::messaging::save_report_instance_response;
    static constexpr message_type request_message_type =
        message_type::save_report_instance_request;
};

template<>
struct message_traits<reporting::messaging::delete_report_instance_request> {
    using request_type = reporting::messaging::delete_report_instance_request;
    using response_type = reporting::messaging::delete_report_instance_response;
    static constexpr message_type request_message_type =
        message_type::delete_report_instance_request;
};

template<>
struct message_traits<reporting::messaging::get_report_instance_history_request> {
    using request_type = reporting::messaging::get_report_instance_history_request;
    using response_type = reporting::messaging::get_report_instance_history_response;
    static constexpr message_type request_message_type =
        message_type::get_report_instance_history_request;
};

}

#endif
