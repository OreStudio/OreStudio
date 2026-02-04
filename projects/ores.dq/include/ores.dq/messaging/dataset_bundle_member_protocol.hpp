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
#ifndef ORES_DQ_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATASET_BUNDLE_MEMBER_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/dataset_bundle_member.hpp"

namespace ores::dq::messaging {

// ============================================================================
// Dataset Bundle Member Messages
// ============================================================================

/**
 * @brief Request to retrieve all dataset bundle members.
 */
struct get_dataset_bundle_members_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_members_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_request& v);

/**
 * @brief Response containing all dataset bundle members.
 */
struct get_dataset_bundle_members_response final {
    std::vector<domain::dataset_bundle_member> members;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_members_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_response& v);

/**
 * @brief Request to retrieve dataset bundle members for a specific bundle.
 */
struct get_dataset_bundle_members_by_bundle_request final {
    std::string bundle_code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_members_by_bundle_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_by_bundle_request& v);

/**
 * @brief Response containing dataset bundle members for a bundle.
 */
struct get_dataset_bundle_members_by_bundle_response final {
    std::vector<domain::dataset_bundle_member> members;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_bundle_members_by_bundle_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_bundle_members_by_bundle_response& v);

/**
 * @brief Request to save a dataset bundle member (create or update).
 */
struct save_dataset_bundle_member_request final {
    domain::dataset_bundle_member member;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_bundle_member_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_member_request& v);

/**
 * @brief Response confirming dataset bundle member save operation.
 */
struct save_dataset_bundle_member_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_dataset_bundle_member_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_dataset_bundle_member_response& v);

/**
 * @brief Result for a single dataset bundle member deletion.
 */
struct delete_dataset_bundle_member_result final {
    std::string bundle_code;
    std::string dataset_code;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_result& v);

/**
 * @brief Key identifying a dataset bundle member for deletion.
 */
struct dataset_bundle_member_key final {
    std::string bundle_code;
    std::string dataset_code;
};

std::ostream& operator<<(std::ostream& s, const dataset_bundle_member_key& v);

/**
 * @brief Request to delete one or more dataset bundle members.
 */
struct delete_dataset_bundle_member_request final {
    std::vector<dataset_bundle_member_key> keys;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_bundle_member_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_request& v);

/**
 * @brief Response confirming dataset bundle member deletion(s).
 */
struct delete_dataset_bundle_member_response final {
    std::vector<delete_dataset_bundle_member_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_dataset_bundle_member_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_dataset_bundle_member_response& v);

}

namespace ores::comms::messaging {

// Dataset Bundle Member traits
template<>
struct message_traits<dq::messaging::get_dataset_bundle_members_request> {
    using request_type = dq::messaging::get_dataset_bundle_members_request;
    using response_type = dq::messaging::get_dataset_bundle_members_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_bundle_members_request;
};

template<>
struct message_traits<dq::messaging::get_dataset_bundle_members_by_bundle_request> {
    using request_type = dq::messaging::get_dataset_bundle_members_by_bundle_request;
    using response_type = dq::messaging::get_dataset_bundle_members_by_bundle_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_bundle_members_by_bundle_request;
};

template<>
struct message_traits<dq::messaging::save_dataset_bundle_member_request> {
    using request_type = dq::messaging::save_dataset_bundle_member_request;
    using response_type = dq::messaging::save_dataset_bundle_member_response;
    static constexpr message_type request_message_type =
        message_type::save_dataset_bundle_member_request;
};

template<>
struct message_traits<dq::messaging::delete_dataset_bundle_member_request> {
    using request_type = dq::messaging::delete_dataset_bundle_member_request;
    using response_type = dq::messaging::delete_dataset_bundle_member_response;
    static constexpr message_type request_message_type =
        message_type::delete_dataset_bundle_member_request;
};

}

#endif
