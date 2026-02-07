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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.iam/domain/account_party.hpp"

namespace ores::iam::messaging {

// ============================================================================
// Account Party Messages
// ============================================================================

/**
 * @brief Request to retrieve all account parties.
 */
struct get_account_parties_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_account_parties_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_parties_request& v);

/**
 * @brief Response containing all account parties.
 */
struct get_account_parties_response final {
    std::vector<domain::account_party> account_parties;

    std::vector<std::byte> serialize() const;
    static std::expected<get_account_parties_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_parties_response& v);

/**
 * @brief Request to retrieve account parties for a specific account.
 */
struct get_account_parties_by_account_request final {
    boost::uuids::uuid account_id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_account_parties_by_account_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_parties_by_account_request& v);

/**
 * @brief Response containing account parties for a account.
 */
struct get_account_parties_by_account_response final {
    std::vector<domain::account_party> account_parties;

    std::vector<std::byte> serialize() const;
    static std::expected<get_account_parties_by_account_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_account_parties_by_account_response& v);

/**
 * @brief Request to save a account party (create or update).
 */
struct save_account_party_request final {
    domain::account_party account_party;

    std::vector<std::byte> serialize() const;
    static std::expected<save_account_party_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_account_party_request& v);

/**
 * @brief Response confirming account party save operation.
 */
struct save_account_party_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_account_party_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_account_party_response& v);

/**
 * @brief Result for a single account party deletion.
 */
struct delete_account_party_result final {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_account_party_result& v);

/**
 * @brief Key identifying a account party for deletion.
 */
struct account_party_key final {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
};

std::ostream& operator<<(std::ostream& s, const account_party_key& v);

/**
 * @brief Request to delete one or more account parties.
 */
struct delete_account_party_request final {
    std::vector<account_party_key> keys;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_account_party_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_account_party_request& v);

/**
 * @brief Response confirming account party deletion(s).
 */
struct delete_account_party_response final {
    std::vector<delete_account_party_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_account_party_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_account_party_response& v);

}

namespace ores::comms::messaging {

// Account Party traits
template<>
struct message_traits<iam::messaging::get_account_parties_request> {
    using request_type = iam::messaging::get_account_parties_request;
    using response_type = iam::messaging::get_account_parties_response;
    static constexpr message_type request_message_type =
        message_type::get_account_parties_request;
};

template<>
struct message_traits<iam::messaging::get_account_parties_by_account_request> {
    using request_type = iam::messaging::get_account_parties_by_account_request;
    using response_type = iam::messaging::get_account_parties_by_account_response;
    static constexpr message_type request_message_type =
        message_type::get_account_parties_by_account_request;
};

template<>
struct message_traits<iam::messaging::save_account_party_request> {
    using request_type = iam::messaging::save_account_party_request;
    using response_type = iam::messaging::save_account_party_response;
    static constexpr message_type request_message_type =
        message_type::save_account_party_request;
};

template<>
struct message_traits<iam::messaging::delete_account_party_request> {
    using request_type = iam::messaging::delete_account_party_request;
    using response_type = iam::messaging::delete_account_party_response;
    static constexpr message_type request_message_type =
        message_type::delete_account_party_request;
};

}

#endif
