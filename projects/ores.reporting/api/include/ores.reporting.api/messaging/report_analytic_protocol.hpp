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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_ANALYTIC_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_ANALYTIC_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_analytic.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_analytic_key {
    boost::uuids::uuid id;
};

struct report_analytic_write {
    boost::uuids::uuid id;
    boost::uuids::uuid report_definition_id;
    std::string analytic_type_code;
    int display_order;
    std::string active;
};

struct report_analytic_change {
    report_analytic_write write;
    ores::utility::domain::precondition precondition;
};

struct report_analytic_removal {
    report_analytic_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_analytic_lookup {
    report_analytic_key key;
    std::optional<ores::reporting::domain::report_analytic> report_analytic;
};

struct report_analytics_filter {
    std::optional<std::string> analytic_type_code;
};

struct report_analytic_event {
    boost::uuids::uuid event_id;
    report_analytic_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_analytic_version_key {
    report_analytic_key report_analytic;
    std::uint32_t version;
};

struct report_analytic_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_analytics_request {
    using response_type = struct list_report_analytics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.list";
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
    std::optional<report_analytics_filter> filter;
};

struct list_report_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_analytic> analytics;
    std::uint64_t total;
};

struct get_report_analytic_request {
    using response_type = struct get_report_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_analytic_key key;
};

struct get_report_analytic_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_analytic> report_analytic;
};

struct get_many_report_analytics_request {
    using response_type = struct get_many_report_analytics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_analytic_key> keys;
};

struct get_many_report_analytics_response {
    ores::utility::domain::result result;
    std::vector<report_analytic_lookup> entries;
};

struct put_report_analytic_request {
    using response_type = struct put_report_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_analytic_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_analytic_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_analytic> report_analytic;
};

struct put_many_report_analytics_request {
    using response_type = struct put_many_report_analytics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_analytic_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_analytic> analytics;
};

struct delete_report_analytic_request {
    using response_type = struct delete_report_analytic_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_analytic_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_analytic_response {
    ores::utility::domain::result result;
};

struct delete_many_report_analytics_request {
    using response_type = struct delete_many_report_analytics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_analytic_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_analytics_response {
    ores::utility::domain::result result;
};

struct list_by_analytic_type_code_report_analytics_request {
    using response_type = struct list_by_analytic_type_code_report_analytics_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_analytics.list_by_analytic_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string analytic_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_analytics_filter> filter;
};

struct list_by_analytic_type_code_report_analytics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_analytic> analytics;
    std::uint64_t total;
};

struct list_report_analytic_versions_request {
    using response_type = struct list_report_analytic_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_analytic_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_analytic_versions_filter> filter;
};

struct list_report_analytic_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_analytic> versions;
    std::uint64_t total;
};

struct get_report_analytic_version_request {
    using response_type = struct get_report_analytic_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_analytics_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_analytic_version_key key;
};

struct get_report_analytic_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_analytic> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_analytic_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_analytics_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_analytics_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_analytics_events.deleted";
}

}

#endif
