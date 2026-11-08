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

#include "ores.reporting.api/domain/report_metric_codec.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::reporting::messaging {

struct report_metric_key {
    boost::uuids::uuid id;

    friend bool operator==(const report_metric_key&, const report_metric_key&) = default;
};

struct report_metric_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    boost::uuids::uuid result_id;
    std::chrono::system_clock::time_point as_of;
    std::string measure;
    std::string source;
    int line{};
    boost::uuids::uuid report_definition_id;
    std::string family;
    std::string subject_type;
    std::string subject_id;
    std::string currency;
    std::string scenario;
    std::string factor;
    std::string qualifiers;
    double value{};

    friend bool operator==(const report_metric_write&, const report_metric_write&) = default;
};

struct report_metric_change {
    report_metric_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const report_metric_change&, const report_metric_change&) = default;
};

struct report_metric_removal {
    report_metric_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const report_metric_removal&, const report_metric_removal&) = default;
};

struct report_metric_lookup {
    report_metric_key key;
    std::optional<ores::reporting::domain::report_metric> report_metric;

    friend bool operator==(const report_metric_lookup&, const report_metric_lookup&) = default;
};

struct report_metrics_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> measure;
    std::optional<std::string> family;
    std::optional<std::string> subject_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> measure_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;

    friend bool operator==(const report_metrics_filter&, const report_metrics_filter&) = default;
};

struct report_metric_event {
    boost::uuids::uuid event_id;
    report_metric_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const report_metric_event&, const report_metric_event&) = default;
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

    friend bool operator==(const list_report_metrics_request&,
                           const list_report_metrics_request&) = default;
};

struct list_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_metric> metrics;
    std::uint64_t total{};

    friend bool operator==(const list_report_metrics_response&,
                           const list_report_metrics_response&) = default;
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

    friend bool operator==(const get_report_metric_request&,
                           const get_report_metric_request&) = default;
};

struct get_report_metric_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_metric> report_metric;

    friend bool operator==(const get_report_metric_response&,
                           const get_report_metric_response&) = default;
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

    friend bool operator==(const get_many_report_metrics_request&,
                           const get_many_report_metrics_request&) = default;
};

struct get_many_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<report_metric_lookup> entries;

    friend bool operator==(const get_many_report_metrics_response&,
                           const get_many_report_metrics_response&) = default;
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

    friend bool operator==(const put_report_metric_request&,
                           const put_report_metric_request&) = default;
};

struct put_report_metric_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_metric> report_metric;

    friend bool operator==(const put_report_metric_response&,
                           const put_report_metric_response&) = default;
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

    friend bool operator==(const put_many_report_metrics_request&,
                           const put_many_report_metrics_request&) = default;
};

struct put_many_report_metrics_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_metric> metrics;

    friend bool operator==(const put_many_report_metrics_response&,
                           const put_many_report_metrics_response&) = default;
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

    friend bool operator==(const delete_report_metric_request&,
                           const delete_report_metric_request&) = default;
};

struct delete_report_metric_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_report_metric_response&,
                           const delete_report_metric_response&) = default;
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

    friend bool operator==(const delete_many_report_metrics_request&,
                           const delete_many_report_metrics_request&) = default;
};

struct delete_many_report_metrics_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_report_metrics_response&,
                           const delete_many_report_metrics_response&) = default;
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
inline constexpr std::string_view created = "reporting.v1.events.report_metrics.created";
inline constexpr std::string_view updated = "reporting.v1.events.report_metrics.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.report_metrics.deleted";
}
/**
 * @brief The members of report_metric_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_key>) {
    return std::tuple{
        utility::codec::field{"id", &report_metric_key::id},
    };
}

/**
 * @brief The members of report_metric_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_write>) {
    return std::tuple{
        utility::codec::field{"id", &report_metric_write::id},
        utility::codec::field{"party_id", &report_metric_write::party_id},
        utility::codec::field{"report_instance_id", &report_metric_write::report_instance_id},
        utility::codec::field{"result_id", &report_metric_write::result_id},
        utility::codec::field{"as_of", &report_metric_write::as_of},
        utility::codec::field{"measure", &report_metric_write::measure},
        utility::codec::field{"source", &report_metric_write::source},
        utility::codec::field{"line", &report_metric_write::line},
        utility::codec::field{"report_definition_id", &report_metric_write::report_definition_id},
        utility::codec::field{"family", &report_metric_write::family},
        utility::codec::field{"subject_type", &report_metric_write::subject_type},
        utility::codec::field{"subject_id", &report_metric_write::subject_id},
        utility::codec::field{"currency", &report_metric_write::currency},
        utility::codec::field{"scenario", &report_metric_write::scenario},
        utility::codec::field{"factor", &report_metric_write::factor},
        utility::codec::field{"qualifiers", &report_metric_write::qualifiers},
        utility::codec::field{"value", &report_metric_write::value},
    };
}

/**
 * @brief The members of report_metric_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_change>) {
    return std::tuple{
        utility::codec::field{"write", &report_metric_change::write},
        utility::codec::field{"precondition", &report_metric_change::precondition},
    };
}

/**
 * @brief The members of report_metric_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_removal>) {
    return std::tuple{
        utility::codec::field{"key", &report_metric_removal::key},
        utility::codec::field{"precondition", &report_metric_removal::precondition},
    };
}

/**
 * @brief The members of report_metric_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &report_metric_lookup::key},
        utility::codec::field{"report_metric", &report_metric_lookup::report_metric},
    };
}

/**
 * @brief The members of report_metrics_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metrics_filter>) {
    return std::tuple{
        utility::codec::field{"report_instance_id", &report_metrics_filter::report_instance_id},
        utility::codec::field{"measure", &report_metrics_filter::measure},
        utility::codec::field{"family", &report_metrics_filter::family},
        utility::codec::field{"subject_id", &report_metrics_filter::subject_id},
        utility::codec::field{"id_one_of", &report_metrics_filter::id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &report_metrics_filter::report_instance_id_one_of},
        utility::codec::field{"measure_one_of", &report_metrics_filter::measure_one_of},
        utility::codec::field{"family_one_of", &report_metrics_filter::family_one_of},
        utility::codec::field{"subject_id_one_of", &report_metrics_filter::subject_id_one_of},
    };
}

/**
 * @brief The members of report_metric_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_metric_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &report_metric_event::event_id},
        utility::codec::field{"key", &report_metric_event::key},
        utility::codec::field{"action", &report_metric_event::action},
        utility::codec::field{"version", &report_metric_event::version},
        utility::codec::field{"occurred_at", &report_metric_event::occurred_at},
        utility::codec::field{"correlation_id", &report_metric_event::correlation_id},
    };
}

/**
 * @brief The members of list_report_metrics_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_metrics_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_metrics_request::offset},
        utility::codec::field{"limit", &list_report_metrics_request::limit},
        utility::codec::field{"order", &list_report_metrics_request::order},
        utility::codec::field{"filter", &list_report_metrics_request::filter},
        utility::codec::field{"as_of", &list_report_metrics_request::as_of},
    };
}

/**
 * @brief The members of list_report_metrics_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_metrics_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_report_metrics_response::result},
        utility::codec::field{"metrics", &list_report_metrics_response::metrics},
        utility::codec::field{"total", &list_report_metrics_response::total},
    };
}

/**
 * @brief The members of get_report_metric_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_metric_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_report_metric_request::key},
    };
}

/**
 * @brief The members of get_report_metric_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_metric_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_report_metric_response::result},
        utility::codec::field{"report_metric", &get_report_metric_response::report_metric},
    };
}

/**
 * @brief The members of get_many_report_metrics_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_metrics_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_metrics_request::keys},
    };
}

/**
 * @brief The members of get_many_report_metrics_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_metrics_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_metrics_response::result},
        utility::codec::field{"entries", &get_many_report_metrics_response::entries},
    };
}

/**
 * @brief The members of put_report_metric_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_metric_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_report_metric_request::change},
        utility::codec::field{"intent", &put_report_metric_request::intent},
    };
}

/**
 * @brief The members of put_report_metric_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_metric_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_report_metric_response::result},
        utility::codec::field{"report_metric", &put_report_metric_response::report_metric},
    };
}

/**
 * @brief The members of put_many_report_metrics_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_metrics_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_metrics_request::changes},
        utility::codec::field{"intent", &put_many_report_metrics_request::intent},
    };
}

/**
 * @brief The members of put_many_report_metrics_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_metrics_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_metrics_response::result},
        utility::codec::field{"metrics", &put_many_report_metrics_response::metrics},
    };
}

/**
 * @brief The members of delete_report_metric_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_metric_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_metric_request::removal},
        utility::codec::field{"intent", &delete_report_metric_request::intent},
    };
}

/**
 * @brief The members of delete_report_metric_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_metric_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_metric_response::result},
    };
}

/**
 * @brief The members of delete_many_report_metrics_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_report_metrics_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_metrics_request::removals},
        utility::codec::field{"intent", &delete_many_report_metrics_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_metrics_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_report_metrics_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_metrics_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_key& v);
    static ores::reporting::messaging::report_metric_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_key& v);
    static ores::reporting::messaging::report_metric_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_write& v);
    static ores::reporting::messaging::report_metric_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_write& v);
    static ores::reporting::messaging::report_metric_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_change& v);
    static ores::reporting::messaging::report_metric_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_change& v);
    static ores::reporting::messaging::report_metric_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_removal& v);
    static ores::reporting::messaging::report_metric_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_removal& v);
    static ores::reporting::messaging::report_metric_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_lookup& v);
    static ores::reporting::messaging::report_metric_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_lookup& v);
    static ores::reporting::messaging::report_metric_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metrics_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metrics_filter& v);
    static ores::reporting::messaging::report_metrics_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metrics_filter& v);
    static ores::reporting::messaging::report_metrics_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_metric_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_metric_event& v);
    static ores::reporting::messaging::report_metric_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_metric_event& v);
    static ores::reporting::messaging::report_metric_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_metrics_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_report_metrics_request& v);
    static ores::reporting::messaging::list_report_metrics_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_metrics_request& v);
    static ores::reporting::messaging::list_report_metrics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_metrics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_metrics_response& v);
    static ores::reporting::messaging::list_report_metrics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_metrics_response& v);
    static ores::reporting::messaging::list_report_metrics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_metric_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_report_metric_request& v);
    static ores::reporting::messaging::get_report_metric_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_metric_request& v);
    static ores::reporting::messaging::get_report_metric_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_metric_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_report_metric_response& v);
    static ores::reporting::messaging::get_report_metric_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_metric_response& v);
    static ores::reporting::messaging::get_report_metric_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_metrics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_metrics_request& v);
    static ores::reporting::messaging::get_many_report_metrics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_metrics_request& v);
    static ores::reporting::messaging::get_many_report_metrics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_metrics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_metrics_response& v);
    static ores::reporting::messaging::get_many_report_metrics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_metrics_response& v);
    static ores::reporting::messaging::get_many_report_metrics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_metric_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_report_metric_request& v);
    static ores::reporting::messaging::put_report_metric_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_metric_request& v);
    static ores::reporting::messaging::put_report_metric_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_metric_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_report_metric_response& v);
    static ores::reporting::messaging::put_report_metric_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_metric_response& v);
    static ores::reporting::messaging::put_report_metric_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_metrics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_metrics_request& v);
    static ores::reporting::messaging::put_many_report_metrics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_metrics_request& v);
    static ores::reporting::messaging::put_many_report_metrics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_metrics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_metrics_response& v);
    static ores::reporting::messaging::put_many_report_metrics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_metrics_response& v);
    static ores::reporting::messaging::put_many_report_metrics_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_metric_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_metric_request& v);
    static ores::reporting::messaging::delete_report_metric_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_metric_request& v);
    static ores::reporting::messaging::delete_report_metric_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_metric_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_metric_response& v);
    static ores::reporting::messaging::delete_report_metric_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_metric_response& v);
    static ores::reporting::messaging::delete_report_metric_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_metrics_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_metrics_request& v);
    static ores::reporting::messaging::delete_many_report_metrics_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_metrics_request& v);
    static ores::reporting::messaging::delete_many_report_metrics_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_metrics_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_metrics_response& v);
    static ores::reporting::messaging::delete_many_report_metrics_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_metrics_response& v);
    static ores::reporting::messaging::delete_many_report_metrics_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
