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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_CREDENTIAL_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_CREDENTIAL_PROTOCOL_HPP

#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct account_credential_key {
    boost::uuids::uuid id;
};

struct account_credential_lookup {
    account_credential_key key;
    std::optional<ores::iam::domain::account_credential> account_credential;
};

struct account_credentials_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
};

struct account_credential_event {
    boost::uuids::uuid event_id;
    account_credential_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct account_credential_version_key {
    account_credential_key account_credential;
    std::uint32_t version;
};

struct account_credential_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_account_credentials_request {
    using response_type = struct list_account_credentials_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_credentials.list";
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
    std::optional<account_credentials_filter> filter;
    std::optional<std::string> as_of;
};

struct list_account_credentials_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_credential> account_credentials;
    std::uint64_t total;
};

struct get_account_credential_request {
    using response_type = struct get_account_credential_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_credentials.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_credential_key key;
};

struct get_account_credential_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_credential> account_credential;
};

struct get_many_account_credentials_request {
    using response_type = struct get_many_account_credentials_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_credentials.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_credential_key> keys;
};

struct get_many_account_credentials_response {
    ores::utility::domain::result result;
    std::vector<account_credential_lookup> entries;
};

struct list_by_account_id_account_credentials_request {
    using response_type = struct list_by_account_id_account_credentials_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_credentials.list_by_account_id";
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
    std::optional<account_credentials_filter> filter;
};

struct list_by_account_id_account_credentials_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_credential> account_credentials;
    std::uint64_t total;
};

struct list_account_credential_versions_request {
    using response_type = struct list_account_credential_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_credentials_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_credential_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_credential_versions_filter> filter;
};

struct list_account_credential_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_credential> versions;
    std::uint64_t total;
};

struct get_account_credential_version_request {
    using response_type = struct get_account_credential_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_credentials_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_credential_version_key key;
};

struct get_account_credential_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_credential> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace account_credential_event_subjects {
inline constexpr std::string_view created = "iam.v1.account_credentials_events.created";
inline constexpr std::string_view updated = "iam.v1.account_credentials_events.updated";
inline constexpr std::string_view deleted = "iam.v1.account_credentials_events.deleted";
}

}

#endif
