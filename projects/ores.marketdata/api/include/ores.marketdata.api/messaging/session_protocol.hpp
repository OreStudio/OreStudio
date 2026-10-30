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
#ifndef ORES_MARKETDATA_API_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_SESSION_PROTOCOL_HPP

#include "ores.marketdata.api/domain/session.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct session_key {
    std::string code;
};

struct session_write {
    std::string code;
    std::string name;
    std::string supplier_code;
    std::string adapter_code;
    std::string environment;
    std::string endpoint;
    std::string credential_ref;
    std::string settings;
    bool enabled;
    std::string description;
};

struct session_change {
    session_write write;
    ores::utility::domain::precondition precondition;
};

struct session_removal {
    session_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct session_lookup {
    session_key key;
    std::optional<ores::marketdata::domain::session> session;
};

struct sessions_filter {
    std::optional<std::string> supplier_code;
    std::optional<std::string> adapter_code;
    std::optional<std::string> environment;
    std::optional<std::vector<std::string>> code_one_of;
    std::optional<std::vector<std::string>> supplier_code_one_of;
    std::optional<std::vector<std::string>> adapter_code_one_of;
    std::optional<std::vector<std::string>> environment_one_of;
    std::optional<std::string> search;
};

struct session_event {
    boost::uuids::uuid event_id;
    session_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct session_version_key {
    session_key session;
    std::uint32_t version;
};

struct session_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_sessions_request {
    using response_type = struct list_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.list";
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
    std::optional<sessions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> sessions;
    std::uint64_t total;
};

struct get_session_request {
    using response_type = struct get_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_key key;
};

struct get_session_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> session;
};

struct get_many_sessions_request {
    using response_type = struct get_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_key> keys;
};

struct get_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<session_lookup> entries;
};

struct put_session_request {
    using response_type = struct put_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_change change;
    ores::utility::domain::change_intent intent;
};

struct put_session_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> session;
};

struct put_many_sessions_request {
    using response_type = struct put_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_sessions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> sessions;
};

struct delete_session_request {
    using response_type = struct delete_session_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_session_response {
    ores::utility::domain::result result;
};

struct delete_many_sessions_request {
    using response_type = struct delete_many_sessions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<session_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_sessions_response {
    ores::utility::domain::result result;
};

struct list_session_versions_request {
    using response_type = struct list_session_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<session_versions_filter> filter;
};

struct list_session_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::session> versions;
    std::uint64_t total;
};

struct get_session_version_request {
    using response_type = struct get_session_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.sessions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    session_version_key key;
};

struct get_session_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::session> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace session_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.sessions_events.created";
inline constexpr std::string_view updated = "marketdata.v1.sessions_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.sessions_events.deleted";
}

}

#endif
