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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_ERROR_SCOPE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_ERROR_SCOPE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_error_scope.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_error_scope_key {
    std::string code;
};

struct report_error_scope_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct report_error_scope_change {
    report_error_scope_write write;
    ores::utility::domain::precondition precondition;
};

struct report_error_scope_removal {
    report_error_scope_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_error_scope_lookup {
    report_error_scope_key key;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct report_error_scopes_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct report_error_scope_event {
    boost::uuids::uuid event_id;
    report_error_scope_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_error_scope_version_key {
    report_error_scope_key report_error_scope;
    std::uint32_t version;
};

struct report_error_scope_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_error_scopes_request {
    using response_type = struct list_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.list";
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
    std::optional<report_error_scopes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> scopes;
    std::uint64_t total;
};

struct get_report_error_scope_request {
    using response_type = struct get_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_key key;
};

struct get_report_error_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct get_many_report_error_scopes_request {
    using response_type = struct get_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_key> keys;
};

struct get_many_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<report_error_scope_lookup> entries;
};

struct put_report_error_scope_request {
    using response_type = struct put_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_error_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> report_error_scope;
};

struct put_many_report_error_scopes_request {
    using response_type = struct put_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_error_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> scopes;
};

struct delete_report_error_scope_request {
    using response_type = struct delete_report_error_scope_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_error_scope_response {
    ores::utility::domain::result result;
};

struct delete_many_report_error_scopes_request {
    using response_type = struct delete_many_report_error_scopes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_scopes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_scope_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_error_scopes_response {
    ores::utility::domain::result result;
};

struct list_report_error_scope_versions_request {
    using response_type = struct list_report_error_scope_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_scopes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_error_scope_versions_filter> filter;
};

struct list_report_error_scope_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_scope> versions;
    std::uint64_t total;
};

struct get_report_error_scope_version_request {
    using response_type = struct get_report_error_scope_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_scopes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_scope_version_key key;
};

struct get_report_error_scope_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_scope> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_error_scope_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_error_scopes_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_error_scopes_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_error_scopes_events.deleted";
}

}

#endif
