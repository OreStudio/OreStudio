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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_PARTY_PROTOCOL_HPP

#include "ores.iam.api/domain/account_party.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct account_party_key {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
};

struct account_party_write {
    boost::uuids::uuid account_id;
    boost::uuids::uuid party_id;
};

struct account_party_change {
    account_party_write write;
    ores::utility::domain::precondition precondition;
};

struct account_party_removal {
    account_party_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct account_party_lookup {
    account_party_key key;
    std::optional<ores::iam::domain::account_party> account_party;
};

struct account_parties_filter {
    std::optional<boost::uuids::uuid> account_id;
};

struct list_account_parties_request {
    using response_type = struct list_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_parties_filter> filter;
};

struct list_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
    std::uint64_t total;
};

struct get_account_party_request {
    using response_type = struct get_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_key key;
};

struct get_account_party_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_party> account_party;
};

struct get_many_account_parties_request {
    using response_type = struct get_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_key> keys;
};

struct get_many_account_parties_response {
    ores::utility::domain::result result;
    std::vector<account_party_lookup> entries;
};

struct put_account_party_request {
    using response_type = struct put_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_change change;
    ores::utility::domain::change_intent intent;
};

struct put_account_party_response {
    ores::utility::domain::result result;
    ores::iam::domain::account_party account_party;
};

struct put_many_account_parties_request {
    using response_type = struct put_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
};

struct delete_account_party_request {
    using response_type = struct delete_account_party_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_party_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_account_party_response {
    ores::utility::domain::result result;
};

struct delete_many_account_parties_request {
    using response_type = struct delete_many_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_party_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_account_parties_response {
    ores::utility::domain::result result;
};

struct list_by_account_id_account_parties_request {
    using response_type = struct list_by_account_id_account_parties_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_parties.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_parties_filter> filter;
};

struct list_by_account_id_account_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_party> account_parties;
    std::uint64_t total;
};

}

#endif
