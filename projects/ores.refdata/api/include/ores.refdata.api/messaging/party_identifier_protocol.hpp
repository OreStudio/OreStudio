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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_IDENTIFIER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_IDENTIFIER_PROTOCOL_HPP

#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct party_identifier_key {
    boost::uuids::uuid id;
};

struct party_identifier_write {
    boost::uuids::uuid id;
    std::string id_scheme;
    std::string id_value;
    std::string description;
};

struct party_identifier_change {
    party_identifier_write write;
    ores::utility::domain::precondition precondition;
};

struct party_identifier_removal {
    party_identifier_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct party_identifier_lookup {
    party_identifier_key key;
    std::optional<ores::refdata::domain::party_identifier> party_identifier;
};

struct party_identifiers_filter {
    std::optional<boost::uuids::uuid> party_id;
};

struct party_identifier_event {
    boost::uuids::uuid event_id;
    party_identifier_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct party_identifier_version_key {
    party_identifier_key party_identifier;
    std::uint32_t version;
};

struct party_identifier_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_party_identifiers_request {
    using response_type = struct list_party_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.list";
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
    std::optional<party_identifiers_filter> filter;
};

struct list_party_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_identifier> party_identifiers;
    std::uint64_t total;
};

struct get_party_identifier_request {
    using response_type = struct get_party_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_identifier_key key;
};

struct get_party_identifier_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party_identifier> party_identifier;
};

struct get_many_party_identifiers_request {
    using response_type = struct get_many_party_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_identifier_key> keys;
};

struct get_many_party_identifiers_response {
    ores::utility::domain::result result;
    std::vector<party_identifier_lookup> entries;
};

struct put_party_identifier_request {
    using response_type = struct put_party_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_identifier_change change;
    ores::utility::domain::change_intent intent;
};

struct put_party_identifier_response {
    ores::utility::domain::result result;
    ores::refdata::domain::party_identifier party_identifier;
};

struct put_many_party_identifiers_request {
    using response_type = struct put_many_party_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_identifier_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_party_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_identifier> party_identifiers;
};

struct delete_party_identifier_request {
    using response_type = struct delete_party_identifier_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_identifier_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_party_identifier_response {
    ores::utility::domain::result result;
};

struct delete_many_party_identifiers_request {
    using response_type = struct delete_many_party_identifiers_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_identifier_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_party_identifiers_response {
    ores::utility::domain::result result;
};

struct list_by_party_id_party_identifiers_request {
    using response_type = struct list_by_party_id_party_identifiers_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.party_identifiers.list_by_party_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid party_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_identifiers_filter> filter;
};

struct list_by_party_id_party_identifiers_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_identifier> party_identifiers;
    std::uint64_t total;
};

struct list_party_identifier_versions_request {
    using response_type = struct list_party_identifier_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_identifier_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_identifier_versions_filter> filter;
};

struct list_party_identifier_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party_identifier> versions;
    std::uint64_t total;
};

struct get_party_identifier_version_request {
    using response_type = struct get_party_identifier_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.party_identifiers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_identifier_version_key key;
};

struct get_party_identifier_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::party_identifier version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace party_identifier_event_subjects {
inline constexpr std::string_view created = "refdata.v1.party_identifiers_events.created";
inline constexpr std::string_view updated = "refdata.v1.party_identifiers_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.party_identifiers_events.deleted";
}

}

#endif
