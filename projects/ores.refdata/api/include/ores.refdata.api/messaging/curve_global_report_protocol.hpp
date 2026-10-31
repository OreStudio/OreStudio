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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_GLOBAL_REPORT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_GLOBAL_REPORT_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_global_report.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct curve_global_report_key {
    boost::uuids::uuid id;
};

struct curve_global_report_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_configuration_id;
    std::string family;
    bool has_report;
    std::optional<std::string> report_on_delta_grid;
    std::optional<std::string> report_on_moneyness_grid;
    std::optional<std::string> report_on_strike_grid;
    std::optional<std::string> report_on_strike_spread_grid;
    std::optional<std::string> deltas;
    std::optional<std::string> moneyness;
    std::optional<std::string> strikes;
    std::optional<std::string> strike_spreads;
    std::optional<std::string> expiries;
    std::optional<std::string> pillar_dates;
    std::optional<std::string> underlying_tenors;
    std::optional<std::string> continuation_expiry;
    int position;
};

struct curve_global_report_change {
    curve_global_report_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_global_report_removal {
    curve_global_report_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_global_report_lookup {
    curve_global_report_key key;
    std::optional<ores::refdata::domain::curve_global_report> curve_global_report;
};

struct curve_global_reports_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_global_report_event {
    boost::uuids::uuid event_id;
    curve_global_report_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_global_report_version_key {
    curve_global_report_key curve_global_report;
    std::uint32_t version;
};

struct curve_global_report_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_global_reports_request {
    using response_type = struct list_curve_global_reports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.list";
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
    std::optional<curve_global_reports_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_global_reports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_global_report> global_reports;
    std::uint64_t total;
};

struct get_curve_global_report_request {
    using response_type = struct get_curve_global_report_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_global_report_key key;
};

struct get_curve_global_report_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_global_report> curve_global_report;
};

struct get_many_curve_global_reports_request {
    using response_type = struct get_many_curve_global_reports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_global_report_key> keys;
};

struct get_many_curve_global_reports_response {
    ores::utility::domain::result result;
    std::vector<curve_global_report_lookup> entries;
};

struct put_curve_global_report_request {
    using response_type = struct put_curve_global_report_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_global_report_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_global_report_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_global_report> curve_global_report;
};

struct put_many_curve_global_reports_request {
    using response_type = struct put_many_curve_global_reports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_global_report_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_global_reports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_global_report> global_reports;
};

struct delete_curve_global_report_request {
    using response_type = struct delete_curve_global_report_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_global_report_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_global_report_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_global_reports_request {
    using response_type = struct delete_many_curve_global_reports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_global_report_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_global_reports_response {
    ores::utility::domain::result result;
};

struct list_curve_global_report_versions_request {
    using response_type = struct list_curve_global_report_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_global_reports_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_global_report_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_global_report_versions_filter> filter;
};

struct list_curve_global_report_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_global_report> versions;
    std::uint64_t total;
};

struct get_curve_global_report_version_request {
    using response_type = struct get_curve_global_report_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_global_reports_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_global_report_version_key key;
};

struct get_curve_global_report_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_global_report> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_global_report_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_global_reports_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_global_reports_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_global_reports_events.deleted";
}
/**
 * @brief The members of curve_global_report_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_global_report_key::id},
    };
}

/**
 * @brief The members of curve_global_report_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_global_report_write::id},
        utility::codec::field{"curve_configuration_id",
                              &curve_global_report_write::curve_configuration_id},
        utility::codec::field{"family", &curve_global_report_write::family},
        utility::codec::field{"has_report", &curve_global_report_write::has_report},
        utility::codec::field{"report_on_delta_grid",
                              &curve_global_report_write::report_on_delta_grid},
        utility::codec::field{"report_on_moneyness_grid",
                              &curve_global_report_write::report_on_moneyness_grid},
        utility::codec::field{"report_on_strike_grid",
                              &curve_global_report_write::report_on_strike_grid},
        utility::codec::field{"report_on_strike_spread_grid",
                              &curve_global_report_write::report_on_strike_spread_grid},
        utility::codec::field{"deltas", &curve_global_report_write::deltas},
        utility::codec::field{"moneyness", &curve_global_report_write::moneyness},
        utility::codec::field{"strikes", &curve_global_report_write::strikes},
        utility::codec::field{"strike_spreads", &curve_global_report_write::strike_spreads},
        utility::codec::field{"expiries", &curve_global_report_write::expiries},
        utility::codec::field{"pillar_dates", &curve_global_report_write::pillar_dates},
        utility::codec::field{"underlying_tenors", &curve_global_report_write::underlying_tenors},
        utility::codec::field{"continuation_expiry",
                              &curve_global_report_write::continuation_expiry},
        utility::codec::field{"position", &curve_global_report_write::position},
    };
}

/**
 * @brief The members of curve_global_report_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_global_report_change::write},
        utility::codec::field{"precondition", &curve_global_report_change::precondition},
    };
}

/**
 * @brief The members of curve_global_report_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_global_report_removal::key},
        utility::codec::field{"precondition", &curve_global_report_removal::precondition},
    };
}

/**
 * @brief The members of curve_global_report_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_global_report_lookup::key},
        utility::codec::field{"curve_global_report",
                              &curve_global_report_lookup::curve_global_report},
    };
}

/**
 * @brief The members of curve_global_reports_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_reports_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_global_reports_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_global_report_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_global_report_event::event_id},
        utility::codec::field{"key", &curve_global_report_event::key},
        utility::codec::field{"action", &curve_global_report_event::action},
        utility::codec::field{"version", &curve_global_report_event::version},
        utility::codec::field{"occurred_at", &curve_global_report_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_global_report_event::correlation_id},
    };
}

/**
 * @brief The members of curve_global_report_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_global_report",
                              &curve_global_report_version_key::curve_global_report},
        utility::codec::field{"version", &curve_global_report_version_key::version},
    };
}

/**
 * @brief The members of curve_global_report_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_global_report_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_global_report_versions_filter::version},
        utility::codec::field{"from_version", &curve_global_report_versions_filter::from_version},
        utility::codec::field{"to_version", &curve_global_report_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_global_reports_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_global_reports_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_global_reports_request::offset},
        utility::codec::field{"limit", &list_curve_global_reports_request::limit},
        utility::codec::field{"order", &list_curve_global_reports_request::order},
        utility::codec::field{"filter", &list_curve_global_reports_request::filter},
        utility::codec::field{"as_of", &list_curve_global_reports_request::as_of},
    };
}

/**
 * @brief The members of list_curve_global_reports_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_global_reports_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_global_reports_response::result},
        utility::codec::field{"global_reports",
                              &list_curve_global_reports_response::global_reports},
        utility::codec::field{"total", &list_curve_global_reports_response::total},
    };
}

/**
 * @brief The members of get_curve_global_report_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_global_report_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_global_report_request::key},
    };
}

/**
 * @brief The members of get_curve_global_report_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_global_report_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_global_report_response::result},
        utility::codec::field{"curve_global_report",
                              &get_curve_global_report_response::curve_global_report},
    };
}

/**
 * @brief The members of get_many_curve_global_reports_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_global_reports_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_global_reports_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_global_reports_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_global_reports_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_global_reports_response::result},
        utility::codec::field{"entries", &get_many_curve_global_reports_response::entries},
    };
}

/**
 * @brief The members of put_curve_global_report_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_global_report_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_global_report_request::change},
        utility::codec::field{"intent", &put_curve_global_report_request::intent},
    };
}

/**
 * @brief The members of put_curve_global_report_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_global_report_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_global_report_response::result},
        utility::codec::field{"curve_global_report",
                              &put_curve_global_report_response::curve_global_report},
    };
}

/**
 * @brief The members of put_many_curve_global_reports_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_global_reports_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_global_reports_request::changes},
        utility::codec::field{"intent", &put_many_curve_global_reports_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_global_reports_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_global_reports_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_global_reports_response::result},
        utility::codec::field{"global_reports",
                              &put_many_curve_global_reports_response::global_reports},
    };
}

/**
 * @brief The members of delete_curve_global_report_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_global_report_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_global_report_request::removal},
        utility::codec::field{"intent", &delete_curve_global_report_request::intent},
    };
}

/**
 * @brief The members of delete_curve_global_report_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_global_report_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_global_report_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_global_reports_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_global_reports_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_curve_global_reports_request::removals},
        utility::codec::field{"intent", &delete_many_curve_global_reports_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_global_reports_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_global_reports_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_curve_global_reports_response::result},
    };
}

/**
 * @brief The members of list_curve_global_report_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_global_report_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_global_report_versions_request::key},
        utility::codec::field{"offset", &list_curve_global_report_versions_request::offset},
        utility::codec::field{"limit", &list_curve_global_report_versions_request::limit},
        utility::codec::field{"order", &list_curve_global_report_versions_request::order},
        utility::codec::field{"filter", &list_curve_global_report_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_global_report_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_global_report_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_global_report_versions_response::result},
        utility::codec::field{"versions", &list_curve_global_report_versions_response::versions},
        utility::codec::field{"total", &list_curve_global_report_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_global_report_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_global_report_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_global_report_version_request::key},
    };
}

/**
 * @brief The members of get_curve_global_report_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_global_report_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_global_report_version_response::result},
        utility::codec::field{"version", &get_curve_global_report_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_key& v);
    static ores::refdata::messaging::curve_global_report_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_global_report_key& v);
    static ores::refdata::messaging::curve_global_report_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_write& v);
    static ores::refdata::messaging::curve_global_report_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_global_report_write& v);
    static ores::refdata::messaging::curve_global_report_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_change& v);
    static ores::refdata::messaging::curve_global_report_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_global_report_change& v);
    static ores::refdata::messaging::curve_global_report_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_removal& v);
    static ores::refdata::messaging::curve_global_report_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_global_report_removal& v);
    static ores::refdata::messaging::curve_global_report_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_lookup& v);
    static ores::refdata::messaging::curve_global_report_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_global_report_lookup& v);
    static ores::refdata::messaging::curve_global_report_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_reports_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_reports_filter& v);
    static ores::refdata::messaging::curve_global_reports_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_global_reports_filter& v);
    static ores::refdata::messaging::curve_global_reports_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_global_report_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_global_report_event& v);
    static ores::refdata::messaging::curve_global_report_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_global_report_event& v);
    static ores::refdata::messaging::curve_global_report_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_global_report_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_global_report_version_key& v);
    static ores::refdata::messaging::curve_global_report_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_global_report_version_key& v);
    static ores::refdata::messaging::curve_global_report_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_global_report_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_global_report_versions_filter& v);
    static ores::refdata::messaging::curve_global_report_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_global_report_versions_filter& v);
    static ores::refdata::messaging::curve_global_report_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_global_reports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_global_reports_request& v);
    static ores::refdata::messaging::list_curve_global_reports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_global_reports_request& v);
    static ores::refdata::messaging::list_curve_global_reports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_global_reports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_global_reports_response& v);
    static ores::refdata::messaging::list_curve_global_reports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_global_reports_response& v);
    static ores::refdata::messaging::list_curve_global_reports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_global_report_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_global_report_request& v);
    static ores::refdata::messaging::get_curve_global_report_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_global_report_request& v);
    static ores::refdata::messaging::get_curve_global_report_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_global_report_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_global_report_response& v);
    static ores::refdata::messaging::get_curve_global_report_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_global_report_response& v);
    static ores::refdata::messaging::get_curve_global_report_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_global_reports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_global_reports_request& v);
    static ores::refdata::messaging::get_many_curve_global_reports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_global_reports_request& v);
    static ores::refdata::messaging::get_many_curve_global_reports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_global_reports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_global_reports_response& v);
    static ores::refdata::messaging::get_many_curve_global_reports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_global_reports_response& v);
    static ores::refdata::messaging::get_many_curve_global_reports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_global_report_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_global_report_request& v);
    static ores::refdata::messaging::put_curve_global_report_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_global_report_request& v);
    static ores::refdata::messaging::put_curve_global_report_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_global_report_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_global_report_response& v);
    static ores::refdata::messaging::put_curve_global_report_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_global_report_response& v);
    static ores::refdata::messaging::put_curve_global_report_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_global_reports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_global_reports_request& v);
    static ores::refdata::messaging::put_many_curve_global_reports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_global_reports_request& v);
    static ores::refdata::messaging::put_many_curve_global_reports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_global_reports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_global_reports_response& v);
    static ores::refdata::messaging::put_many_curve_global_reports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_global_reports_response& v);
    static ores::refdata::messaging::put_many_curve_global_reports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_global_report_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_global_report_request& v);
    static ores::refdata::messaging::delete_curve_global_report_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_global_report_request& v);
    static ores::refdata::messaging::delete_curve_global_report_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_global_report_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_global_report_response& v);
    static ores::refdata::messaging::delete_curve_global_report_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_global_report_response& v);
    static ores::refdata::messaging::delete_curve_global_report_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_global_reports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_global_reports_request& v);
    static ores::refdata::messaging::delete_many_curve_global_reports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_global_reports_request& v);
    static ores::refdata::messaging::delete_many_curve_global_reports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_global_reports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_global_reports_response& v);
    static ores::refdata::messaging::delete_many_curve_global_reports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_global_reports_response& v);
    static ores::refdata::messaging::delete_many_curve_global_reports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_global_report_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_global_report_versions_request& v);
    static ores::refdata::messaging::list_curve_global_report_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_global_report_versions_request& v);
    static ores::refdata::messaging::list_curve_global_report_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_global_report_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_global_report_versions_response& v);
    static ores::refdata::messaging::list_curve_global_report_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_global_report_versions_response& v);
    static ores::refdata::messaging::list_curve_global_report_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_global_report_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_global_report_version_request& v);
    static ores::refdata::messaging::get_curve_global_report_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_global_report_version_request& v);
    static ores::refdata::messaging::get_curve_global_report_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_global_report_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_global_report_version_response& v);
    static ores::refdata::messaging::get_curve_global_report_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_global_report_version_response& v);
    static ores::refdata::messaging::get_curve_global_report_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
