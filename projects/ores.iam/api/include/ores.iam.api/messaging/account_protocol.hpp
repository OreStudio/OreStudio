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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_PROTOCOL_HPP

#include "ores.iam.api/domain/account.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct account_key {
    boost::uuids::uuid id;
};

struct account_lookup {
    account_key key;
    std::optional<ores::iam::domain::account> account;
};

struct account_event {
    boost::uuids::uuid event_id;
    account_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct account_version_key {
    account_key account;
    std::uint32_t version;
};

struct account_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_accounts_request {
    using response_type = struct list_accounts_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.list";
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
};

struct list_accounts_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account> accounts;
    std::uint64_t total;
};

struct get_account_request {
    using response_type = struct get_account_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_key key;
};

struct get_account_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account> account;
};

struct get_many_accounts_request {
    using response_type = struct get_many_accounts_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_key> keys;
};

struct get_many_accounts_response {
    ores::utility::domain::result result;
    std::vector<account_lookup> entries;
};

struct list_account_versions_request {
    using response_type = struct list_account_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_versions_filter> filter;
};

struct list_account_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account> versions;
    std::uint64_t total;
};

struct get_account_version_request {
    using response_type = struct get_account_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.accounts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_version_key key;
};

struct get_account_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::account version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace account_event_subjects {
inline constexpr std::string_view created = "iam.v1.accounts_events.created";
inline constexpr std::string_view updated = "iam.v1.accounts_events.updated";
inline constexpr std::string_view deleted = "iam.v1.accounts_events.deleted";
}

}

#endif
