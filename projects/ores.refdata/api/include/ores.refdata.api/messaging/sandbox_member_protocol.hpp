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
#ifndef ORES_REFDATA_API_MESSAGING_SANDBOX_MEMBER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_SANDBOX_MEMBER_PROTOCOL_HPP

#include "ores.refdata.api/domain/sandbox_member.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct sandbox_member_key {
    boost::uuids::uuid id;
};

struct sandbox_member_write {
    boost::uuids::uuid id;
    boost::uuids::uuid sandbox_id;
    boost::uuids::uuid account_id;
};

struct sandbox_member_change {
    sandbox_member_write write;
    ores::utility::domain::precondition precondition;
};

struct sandbox_member_removal {
    sandbox_member_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct sandbox_member_lookup {
    sandbox_member_key key;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct sandbox_members_filter {
    std::optional<boost::uuids::uuid> sandbox_id;
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> sandbox_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
};

struct sandbox_member_event {
    boost::uuids::uuid event_id;
    sandbox_member_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct sandbox_member_version_key {
    sandbox_member_key sandbox_member;
    std::uint32_t version;
};

struct sandbox_member_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_sandbox_members_request {
    using response_type = struct list_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.list";
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
    std::optional<sandbox_members_filter> filter;
    std::optional<std::string> as_of;
};

struct list_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct get_sandbox_member_request {
    using response_type = struct get_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_key key;
};

struct get_sandbox_member_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct get_many_sandbox_members_request {
    using response_type = struct get_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_key> keys;
};

struct get_many_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<sandbox_member_lookup> entries;
};

struct put_sandbox_member_request {
    using response_type = struct put_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_change change;
    ores::utility::domain::change_intent intent;
};

struct put_sandbox_member_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct put_many_sandbox_members_request {
    using response_type = struct put_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
};

struct delete_sandbox_member_request {
    using response_type = struct delete_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_sandbox_member_response {
    ores::utility::domain::result result;
};

struct delete_many_sandbox_members_request {
    using response_type = struct delete_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_sandbox_members_response {
    ores::utility::domain::result result;
};

struct list_by_sandbox_id_sandbox_members_request {
    using response_type = struct list_by_sandbox_id_sandbox_members_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.sandbox_members.list_by_sandbox_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid sandbox_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_members_filter> filter;
};

struct list_by_sandbox_id_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct list_by_account_id_sandbox_members_request {
    using response_type = struct list_by_account_id_sandbox_members_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.sandbox_members.list_by_account_id";
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
    std::optional<sandbox_members_filter> filter;
};

struct list_by_account_id_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct list_sandbox_member_versions_request {
    using response_type = struct list_sandbox_member_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_member_versions_filter> filter;
};

struct list_sandbox_member_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> versions;
    std::uint64_t total;
};

struct get_sandbox_member_version_request {
    using response_type = struct get_sandbox_member_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_version_key key;
};

struct get_sandbox_member_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace sandbox_member_event_subjects {
inline constexpr std::string_view created = "refdata.v1.sandbox_members_events.created";
inline constexpr std::string_view updated = "refdata.v1.sandbox_members_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.sandbox_members_events.deleted";
}

}

#endif
