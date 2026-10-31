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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_REPORT_CONFIGURATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_REPORT_CONFIGURATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_report_configuration.hpp"
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

struct curve_report_configuration_key {
    boost::uuids::uuid id;
};

struct curve_report_configuration_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
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
};

struct curve_report_configuration_change {
    curve_report_configuration_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_report_configuration_removal {
    curve_report_configuration_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_report_configuration_lookup {
    curve_report_configuration_key key;
    std::optional<ores::refdata::domain::curve_report_configuration> curve_report_configuration;
};

struct curve_report_configurations_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_report_configuration_event {
    boost::uuids::uuid event_id;
    curve_report_configuration_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_report_configuration_version_key {
    curve_report_configuration_key curve_report_configuration;
    std::uint32_t version;
};

struct curve_report_configuration_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_report_configurations_request {
    using response_type = struct list_curve_report_configurations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_report_configurations.list";
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
    std::optional<curve_report_configurations_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_report_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_report_configuration> report_configurations;
    std::uint64_t total;
};

struct get_curve_report_configuration_request {
    using response_type = struct get_curve_report_configuration_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_report_configurations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_report_configuration_key key;
};

struct get_curve_report_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_report_configuration> curve_report_configuration;
};

struct get_many_curve_report_configurations_request {
    using response_type = struct get_many_curve_report_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_report_configuration_key> keys;
};

struct get_many_curve_report_configurations_response {
    ores::utility::domain::result result;
    std::vector<curve_report_configuration_lookup> entries;
};

struct put_curve_report_configuration_request {
    using response_type = struct put_curve_report_configuration_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_report_configurations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_report_configuration_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_report_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_report_configuration> curve_report_configuration;
};

struct put_many_curve_report_configurations_request {
    using response_type = struct put_many_curve_report_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_report_configuration_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_report_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_report_configuration> report_configurations;
};

struct delete_curve_report_configuration_request {
    using response_type = struct delete_curve_report_configuration_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_report_configuration_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_report_configuration_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_report_configurations_request {
    using response_type = struct delete_many_curve_report_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_report_configuration_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_report_configurations_response {
    ores::utility::domain::result result;
};

struct list_curve_report_configuration_versions_request {
    using response_type = struct list_curve_report_configuration_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_report_configuration_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_report_configuration_versions_filter> filter;
};

struct list_curve_report_configuration_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_report_configuration> versions;
    std::uint64_t total;
};

struct get_curve_report_configuration_version_request {
    using response_type = struct get_curve_report_configuration_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_report_configurations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_report_configuration_version_key key;
};

struct get_curve_report_configuration_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_report_configuration> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_report_configuration_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_report_configurations_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_report_configurations_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_report_configurations_events.deleted";
}
/**
 * @brief The members of curve_report_configuration_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_report_configuration_key::id},
    };
}

/**
 * @brief The members of curve_report_configuration_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_report_configuration_write::id},
        utility::codec::field{"curve_definition_id",
                              &curve_report_configuration_write::curve_definition_id},
        utility::codec::field{"report_on_delta_grid",
                              &curve_report_configuration_write::report_on_delta_grid},
        utility::codec::field{"report_on_moneyness_grid",
                              &curve_report_configuration_write::report_on_moneyness_grid},
        utility::codec::field{"report_on_strike_grid",
                              &curve_report_configuration_write::report_on_strike_grid},
        utility::codec::field{"report_on_strike_spread_grid",
                              &curve_report_configuration_write::report_on_strike_spread_grid},
        utility::codec::field{"deltas", &curve_report_configuration_write::deltas},
        utility::codec::field{"moneyness", &curve_report_configuration_write::moneyness},
        utility::codec::field{"strikes", &curve_report_configuration_write::strikes},
        utility::codec::field{"strike_spreads", &curve_report_configuration_write::strike_spreads},
        utility::codec::field{"expiries", &curve_report_configuration_write::expiries},
        utility::codec::field{"pillar_dates", &curve_report_configuration_write::pillar_dates},
        utility::codec::field{"underlying_tenors",
                              &curve_report_configuration_write::underlying_tenors},
        utility::codec::field{"continuation_expiry",
                              &curve_report_configuration_write::continuation_expiry},
    };
}

/**
 * @brief The members of curve_report_configuration_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_report_configuration_change::write},
        utility::codec::field{"precondition", &curve_report_configuration_change::precondition},
    };
}

/**
 * @brief The members of curve_report_configuration_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_report_configuration_removal::key},
        utility::codec::field{"precondition", &curve_report_configuration_removal::precondition},
    };
}

/**
 * @brief The members of curve_report_configuration_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_report_configuration_lookup::key},
        utility::codec::field{"curve_report_configuration",
                              &curve_report_configuration_lookup::curve_report_configuration},
    };
}

/**
 * @brief The members of curve_report_configurations_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configurations_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_report_configurations_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_report_configuration_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_report_configuration_event::event_id},
        utility::codec::field{"key", &curve_report_configuration_event::key},
        utility::codec::field{"action", &curve_report_configuration_event::action},
        utility::codec::field{"version", &curve_report_configuration_event::version},
        utility::codec::field{"occurred_at", &curve_report_configuration_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_report_configuration_event::correlation_id},
    };
}

/**
 * @brief The members of curve_report_configuration_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_report_configuration",
                              &curve_report_configuration_version_key::curve_report_configuration},
        utility::codec::field{"version", &curve_report_configuration_version_key::version},
    };
}

/**
 * @brief The members of curve_report_configuration_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_report_configuration_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_report_configuration_versions_filter::version},
        utility::codec::field{"from_version",
                              &curve_report_configuration_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &curve_report_configuration_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_report_configurations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_report_configurations_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_report_configurations_request::offset},
        utility::codec::field{"limit", &list_curve_report_configurations_request::limit},
        utility::codec::field{"order", &list_curve_report_configurations_request::order},
        utility::codec::field{"filter", &list_curve_report_configurations_request::filter},
        utility::codec::field{"as_of", &list_curve_report_configurations_request::as_of},
    };
}

/**
 * @brief The members of list_curve_report_configurations_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_report_configurations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_report_configurations_response::result},
        utility::codec::field{"report_configurations",
                              &list_curve_report_configurations_response::report_configurations},
        utility::codec::field{"total", &list_curve_report_configurations_response::total},
    };
}

/**
 * @brief The members of get_curve_report_configuration_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_report_configuration_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_report_configuration_request::key},
    };
}

/**
 * @brief The members of get_curve_report_configuration_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_report_configuration_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_report_configuration_response::result},
        utility::codec::field{"curve_report_configuration",
                              &get_curve_report_configuration_response::curve_report_configuration},
    };
}

/**
 * @brief The members of get_many_curve_report_configurations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_report_configurations_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_report_configurations_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_report_configurations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_report_configurations_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_report_configurations_response::result},
        utility::codec::field{"entries", &get_many_curve_report_configurations_response::entries},
    };
}

/**
 * @brief The members of put_curve_report_configuration_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_report_configuration_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_report_configuration_request::change},
        utility::codec::field{"intent", &put_curve_report_configuration_request::intent},
    };
}

/**
 * @brief The members of put_curve_report_configuration_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_report_configuration_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_report_configuration_response::result},
        utility::codec::field{"curve_report_configuration",
                              &put_curve_report_configuration_response::curve_report_configuration},
    };
}

/**
 * @brief The members of put_many_curve_report_configurations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_report_configurations_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_report_configurations_request::changes},
        utility::codec::field{"intent", &put_many_curve_report_configurations_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_report_configurations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_report_configurations_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_report_configurations_response::result},
        utility::codec::field{
            "report_configurations",
            &put_many_curve_report_configurations_response::report_configurations},
    };
}

/**
 * @brief The members of delete_curve_report_configuration_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_report_configuration_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_report_configuration_request::removal},
        utility::codec::field{"intent", &delete_curve_report_configuration_request::intent},
    };
}

/**
 * @brief The members of delete_curve_report_configuration_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_report_configuration_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_report_configuration_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_report_configurations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_report_configurations_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_curve_report_configurations_request::removals},
        utility::codec::field{"intent", &delete_many_curve_report_configurations_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_report_configurations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_report_configurations_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_curve_report_configurations_response::result},
    };
}

/**
 * @brief The members of list_curve_report_configuration_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_report_configuration_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_report_configuration_versions_request::key},
        utility::codec::field{"offset", &list_curve_report_configuration_versions_request::offset},
        utility::codec::field{"limit", &list_curve_report_configuration_versions_request::limit},
        utility::codec::field{"order", &list_curve_report_configuration_versions_request::order},
        utility::codec::field{"filter", &list_curve_report_configuration_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_report_configuration_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_report_configuration_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_report_configuration_versions_response::result},
        utility::codec::field{"versions",
                              &list_curve_report_configuration_versions_response::versions},
        utility::codec::field{"total", &list_curve_report_configuration_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_report_configuration_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_report_configuration_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_report_configuration_version_request::key},
    };
}

/**
 * @brief The members of get_curve_report_configuration_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_report_configuration_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_report_configuration_version_response::result},
        utility::codec::field{"version", &get_curve_report_configuration_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_key& v);
    static ores::refdata::messaging::curve_report_configuration_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_key& v);
    static ores::refdata::messaging::curve_report_configuration_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_write& v);
    static ores::refdata::messaging::curve_report_configuration_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_write& v);
    static ores::refdata::messaging::curve_report_configuration_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_change& v);
    static ores::refdata::messaging::curve_report_configuration_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_change& v);
    static ores::refdata::messaging::curve_report_configuration_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_removal& v);
    static ores::refdata::messaging::curve_report_configuration_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_removal& v);
    static ores::refdata::messaging::curve_report_configuration_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_lookup& v);
    static ores::refdata::messaging::curve_report_configuration_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_lookup& v);
    static ores::refdata::messaging::curve_report_configuration_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configurations_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configurations_filter& v);
    static ores::refdata::messaging::curve_report_configurations_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configurations_filter& v);
    static ores::refdata::messaging::curve_report_configurations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_event& v);
    static ores::refdata::messaging::curve_report_configuration_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_event& v);
    static ores::refdata::messaging::curve_report_configuration_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_version_key& v);
    static ores::refdata::messaging::curve_report_configuration_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_version_key& v);
    static ores::refdata::messaging::curve_report_configuration_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_report_configuration_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_report_configuration_versions_filter& v);
    static ores::refdata::messaging::curve_report_configuration_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_report_configuration_versions_filter& v);
    static ores::refdata::messaging::curve_report_configuration_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_report_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_report_configurations_request& v);
    static ores::refdata::messaging::list_curve_report_configurations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_report_configurations_request& v);
    static ores::refdata::messaging::list_curve_report_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_report_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_report_configurations_response& v);
    static ores::refdata::messaging::list_curve_report_configurations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_report_configurations_response& v);
    static ores::refdata::messaging::list_curve_report_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_report_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_report_configuration_request& v);
    static ores::refdata::messaging::get_curve_report_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_report_configuration_request& v);
    static ores::refdata::messaging::get_curve_report_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_report_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_report_configuration_response& v);
    static ores::refdata::messaging::get_curve_report_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_report_configuration_response& v);
    static ores::refdata::messaging::get_curve_report_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_report_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::get_many_curve_report_configurations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::get_many_curve_report_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_report_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::get_many_curve_report_configurations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::get_many_curve_report_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_report_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_report_configuration_request& v);
    static ores::refdata::messaging::put_curve_report_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_report_configuration_request& v);
    static ores::refdata::messaging::put_curve_report_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_report_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_report_configuration_response& v);
    static ores::refdata::messaging::put_curve_report_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_report_configuration_response& v);
    static ores::refdata::messaging::put_curve_report_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_report_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::put_many_curve_report_configurations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::put_many_curve_report_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_report_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::put_many_curve_report_configurations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::put_many_curve_report_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_report_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_report_configuration_request& v);
    static ores::refdata::messaging::delete_curve_report_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_report_configuration_request& v);
    static ores::refdata::messaging::delete_curve_report_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_report_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_report_configuration_response& v);
    static ores::refdata::messaging::delete_curve_report_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_report_configuration_response& v);
    static ores::refdata::messaging::delete_curve_report_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_report_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::delete_many_curve_report_configurations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_report_configurations_request& v);
    static ores::refdata::messaging::delete_many_curve_report_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_report_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::delete_many_curve_report_configurations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_report_configurations_response& v);
    static ores::refdata::messaging::delete_many_curve_report_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_report_configuration_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_report_configuration_versions_request& v);
    static ores::refdata::messaging::list_curve_report_configuration_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_report_configuration_versions_request& v);
    static ores::refdata::messaging::list_curve_report_configuration_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_report_configuration_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_report_configuration_versions_response& v);
    static ores::refdata::messaging::list_curve_report_configuration_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_report_configuration_versions_response& v);
    static ores::refdata::messaging::list_curve_report_configuration_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_report_configuration_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_report_configuration_version_request& v);
    static ores::refdata::messaging::get_curve_report_configuration_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_report_configuration_version_request& v);
    static ores::refdata::messaging::get_curve_report_configuration_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_report_configuration_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_report_configuration_version_response& v);
    static ores::refdata::messaging::get_curve_report_configuration_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_report_configuration_version_response& v);
    static ores::refdata::messaging::get_curve_report_configuration_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
