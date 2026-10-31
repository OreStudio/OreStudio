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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_METRIC_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_METRIC_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_metric.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_metric_key {
    boost::uuids::uuid id;
};

struct report_metric_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    std::string family;
    std::string subject_type;
    std::string subject_id;
    std::chrono::system_clock::time_point as_of;
    std::string measure;
    boost::uuids::uuid report_definition_id;
    std::string currency;
    std::string scenario;
    std::string factor;
    std::string shift;
    std::string qualifiers;
    double value;
    std::string source;
};

struct report_metric_change {
    report_metric_write write;
    ores::utility::domain::precondition precondition;
};

struct report_metric_removal {
    report_metric_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_metric_lookup {
    report_metric_key key;
    std::optional<ores::reporting::domain::report_metric> report_metric;
};

struct report_metrics_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> family;
    std::optional<std::string> subject_id;
    std::optional<std::string> measure;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;
    std::optional<std::vector<std::string>> measure_one_of;
};

struct report_metric_event {
    boost::uuids::uuid event_id;
    report_metric_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_report_metrics_request {
    using response_type = struct list_report_metrics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.list";
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
    std::optional<report_metrics_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_metric> metrics;
    std::uint64_t total;
};

struct get_report_metric_request {
    using response_type = struct get_report_metric_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_metric_key key;
};

struct get_report_metric_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_metric> report_metric;
};

struct get_many_report_metrics_request {
    using response_type = struct get_many_report_metrics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_metric_key> keys;
};

struct get_many_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<report_metric_lookup> entries;
};

struct put_report_metric_request {
    using response_type = struct put_report_metric_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_metric_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_metric_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_metric> report_metric;
};

struct put_many_report_metrics_request {
    using response_type = struct put_many_report_metrics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_metric_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_metric> metrics;
};

struct delete_report_metric_request {
    using response_type = struct delete_report_metric_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_metric_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_metric_response {
    ores::utility::domain::result result;
};

struct delete_many_report_metrics_request {
    using response_type = struct delete_many_report_metrics_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_metrics.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_metric_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_metrics_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_metric_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_metrics_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_metrics_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_metrics_events.deleted";
}

}

#endif
