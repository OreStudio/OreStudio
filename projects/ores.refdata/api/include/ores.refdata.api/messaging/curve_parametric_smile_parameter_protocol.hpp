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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_PARAMETRIC_SMILE_PARAMETER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_PARAMETRIC_SMILE_PARAMETER_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_parametric_smile_parameter.hpp"
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

struct curve_parametric_smile_parameter_key {
    boost::uuids::uuid id;
};

struct curve_parametric_smile_parameter_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::string name;
    std::optional<std::string> initial_value;
    std::string calibration;
    int position;
};

struct curve_parametric_smile_parameter_change {
    curve_parametric_smile_parameter_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_parametric_smile_parameter_removal {
    curve_parametric_smile_parameter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_parametric_smile_parameter_lookup {
    curve_parametric_smile_parameter_key key;
    std::optional<ores::refdata::domain::curve_parametric_smile_parameter>
        curve_parametric_smile_parameter;
};

struct curve_parametric_smile_parameters_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_parametric_smile_parameter_event {
    boost::uuids::uuid event_id;
    curve_parametric_smile_parameter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_parametric_smile_parameter_version_key {
    curve_parametric_smile_parameter_key curve_parametric_smile_parameter;
    std::uint32_t version;
};

struct curve_parametric_smile_parameter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_parametric_smile_parameters_request {
    using response_type = struct list_curve_parametric_smile_parameters_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.list";
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
    std::optional<curve_parametric_smile_parameters_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_parametric_smile_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile_parameter> smile_parameters;
    std::uint64_t total;
};

struct get_curve_parametric_smile_parameter_request {
    using response_type = struct get_curve_parametric_smile_parameter_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_parameter_key key;
};

struct get_curve_parametric_smile_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile_parameter>
        curve_parametric_smile_parameter;
};

struct get_many_curve_parametric_smile_parameters_request {
    using response_type = struct get_many_curve_parametric_smile_parameters_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_parameter_key> keys;
};

struct get_many_curve_parametric_smile_parameters_response {
    ores::utility::domain::result result;
    std::vector<curve_parametric_smile_parameter_lookup> entries;
};

struct put_curve_parametric_smile_parameter_request {
    using response_type = struct put_curve_parametric_smile_parameter_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_parameter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_parametric_smile_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile_parameter>
        curve_parametric_smile_parameter;
};

struct put_many_curve_parametric_smile_parameters_request {
    using response_type = struct put_many_curve_parametric_smile_parameters_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_parameter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_parametric_smile_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile_parameter> smile_parameters;
};

struct delete_curve_parametric_smile_parameter_request {
    using response_type = struct delete_curve_parametric_smile_parameter_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_parameter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_parametric_smile_parameter_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_parametric_smile_parameters_request {
    using response_type = struct delete_many_curve_parametric_smile_parameters_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_parametric_smile_parameter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_parametric_smile_parameters_response {
    ores::utility::domain::result result;
};

struct list_curve_parametric_smile_parameter_versions_request {
    using response_type = struct list_curve_parametric_smile_parameter_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_parameter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_parametric_smile_parameter_versions_filter> filter;
};

struct list_curve_parametric_smile_parameter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_parametric_smile_parameter> versions;
    std::uint64_t total;
};

struct get_curve_parametric_smile_parameter_version_request {
    using response_type = struct get_curve_parametric_smile_parameter_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_parametric_smile_parameters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_parametric_smile_parameter_version_key key;
};

struct get_curve_parametric_smile_parameter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_parametric_smile_parameter> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_parametric_smile_parameter_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.curve_parametric_smile_parameters_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.curve_parametric_smile_parameters_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.curve_parametric_smile_parameters_events.deleted";
}
/**
 * @brief The members of curve_parametric_smile_parameter_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_parametric_smile_parameter_key::id},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_parametric_smile_parameter_write::id},
        utility::codec::field{"curve_definition_id",
                              &curve_parametric_smile_parameter_write::curve_definition_id},
        utility::codec::field{"name", &curve_parametric_smile_parameter_write::name},
        utility::codec::field{"initial_value",
                              &curve_parametric_smile_parameter_write::initial_value},
        utility::codec::field{"calibration", &curve_parametric_smile_parameter_write::calibration},
        utility::codec::field{"position", &curve_parametric_smile_parameter_write::position},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_change, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_parametric_smile_parameter_change::write},
        utility::codec::field{"precondition",
                              &curve_parametric_smile_parameter_change::precondition},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_parametric_smile_parameter_removal::key},
        utility::codec::field{"precondition",
                              &curve_parametric_smile_parameter_removal::precondition},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_lookup, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_parametric_smile_parameter_lookup::key},
        utility::codec::field{
            "curve_parametric_smile_parameter",
            &curve_parametric_smile_parameter_lookup::curve_parametric_smile_parameter},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameters_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameters_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_parametric_smile_parameters_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_parametric_smile_parameter_event::event_id},
        utility::codec::field{"key", &curve_parametric_smile_parameter_event::key},
        utility::codec::field{"action", &curve_parametric_smile_parameter_event::action},
        utility::codec::field{"version", &curve_parametric_smile_parameter_event::version},
        utility::codec::field{"occurred_at", &curve_parametric_smile_parameter_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &curve_parametric_smile_parameter_event::correlation_id},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_version_key&) {
    return std::tuple{
        utility::codec::field{
            "curve_parametric_smile_parameter",
            &curve_parametric_smile_parameter_version_key::curve_parametric_smile_parameter},
        utility::codec::field{"version", &curve_parametric_smile_parameter_version_key::version},
    };
}

/**
 * @brief The members of curve_parametric_smile_parameter_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_parametric_smile_parameter_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version",
                              &curve_parametric_smile_parameter_versions_filter::version},
        utility::codec::field{"from_version",
                              &curve_parametric_smile_parameter_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &curve_parametric_smile_parameter_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_parameters_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_parametric_smile_parameters_request::offset},
        utility::codec::field{"limit", &list_curve_parametric_smile_parameters_request::limit},
        utility::codec::field{"order", &list_curve_parametric_smile_parameters_request::order},
        utility::codec::field{"filter", &list_curve_parametric_smile_parameters_request::filter},
        utility::codec::field{"as_of", &list_curve_parametric_smile_parameters_request::as_of},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_parameters_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_parametric_smile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_parametric_smile_parameters_response::result},
        utility::codec::field{"smile_parameters",
                              &list_curve_parametric_smile_parameters_response::smile_parameters},
        utility::codec::field{"total", &list_curve_parametric_smile_parameters_response::total},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_parameter_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_parametric_smile_parameter_request::key},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_parameter_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_parametric_smile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_parametric_smile_parameter_response::result},
        utility::codec::field{
            "curve_parametric_smile_parameter",
            &get_curve_parametric_smile_parameter_response::curve_parametric_smile_parameter},
    };
}

/**
 * @brief The members of get_many_curve_parametric_smile_parameters_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_curve_parametric_smile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_parametric_smile_parameters_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_parametric_smile_parameters_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_curve_parametric_smile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_curve_parametric_smile_parameters_response::result},
        utility::codec::field{"entries",
                              &get_many_curve_parametric_smile_parameters_response::entries},
    };
}

/**
 * @brief The members of put_curve_parametric_smile_parameter_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_parametric_smile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_parametric_smile_parameter_request::change},
        utility::codec::field{"intent", &put_curve_parametric_smile_parameter_request::intent},
    };
}

/**
 * @brief The members of put_curve_parametric_smile_parameter_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_parametric_smile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_parametric_smile_parameter_response::result},
        utility::codec::field{
            "curve_parametric_smile_parameter",
            &put_curve_parametric_smile_parameter_response::curve_parametric_smile_parameter},
    };
}

/**
 * @brief The members of put_many_curve_parametric_smile_parameters_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_curve_parametric_smile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_curve_parametric_smile_parameters_request::changes},
        utility::codec::field{"intent",
                              &put_many_curve_parametric_smile_parameters_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_parametric_smile_parameters_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_curve_parametric_smile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_curve_parametric_smile_parameters_response::result},
        utility::codec::field{
            "smile_parameters",
            &put_many_curve_parametric_smile_parameters_response::smile_parameters},
    };
}

/**
 * @brief The members of delete_curve_parametric_smile_parameter_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_parametric_smile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_parametric_smile_parameter_request::removal},
        utility::codec::field{"intent", &delete_curve_parametric_smile_parameter_request::intent},
    };
}

/**
 * @brief The members of delete_curve_parametric_smile_parameter_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_parametric_smile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_parametric_smile_parameter_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_parametric_smile_parameters_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_curve_parametric_smile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_curve_parametric_smile_parameters_request::removals},
        utility::codec::field{"intent",
                              &delete_many_curve_parametric_smile_parameters_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_parametric_smile_parameters_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_curve_parametric_smile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_curve_parametric_smile_parameters_response::result},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_parameter_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_curve_parametric_smile_parameter_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_parametric_smile_parameter_versions_request::key},
        utility::codec::field{"offset",
                              &list_curve_parametric_smile_parameter_versions_request::offset},
        utility::codec::field{"limit",
                              &list_curve_parametric_smile_parameter_versions_request::limit},
        utility::codec::field{"order",
                              &list_curve_parametric_smile_parameter_versions_request::order},
        utility::codec::field{"filter",
                              &list_curve_parametric_smile_parameter_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_parametric_smile_parameter_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_curve_parametric_smile_parameter_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_curve_parametric_smile_parameter_versions_response::result},
        utility::codec::field{"versions",
                              &list_curve_parametric_smile_parameter_versions_response::versions},
        utility::codec::field{"total",
                              &list_curve_parametric_smile_parameter_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_parameter_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_curve_parametric_smile_parameter_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_parametric_smile_parameter_version_request::key},
    };
}

/**
 * @brief The members of get_curve_parametric_smile_parameter_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_curve_parametric_smile_parameter_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_curve_parametric_smile_parameter_version_response::result},
        utility::codec::field{"version",
                              &get_curve_parametric_smile_parameter_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_key& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_key& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_write& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_write& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_change& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_change& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_removal& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_removal& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_lookup& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_lookup& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameters_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameters_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_parameters_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameters_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_parameters_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_event& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_event& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_version_key& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_parametric_smile_parameter_version_key& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_parametric_smile_parameter_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_parametric_smile_parameter_versions_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::curve_parametric_smile_parameter_versions_filter& v);
    static ores::refdata::messaging::curve_parametric_smile_parameter_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameters_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameters_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_parametric_smile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::get_many_curve_parametric_smile_parameters_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::get_many_curve_parametric_smile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_parametric_smile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::get_many_curve_parametric_smile_parameters_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::get_many_curve_parametric_smile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_parametric_smile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::put_curve_parametric_smile_parameter_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::put_curve_parametric_smile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_parametric_smile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::put_curve_parametric_smile_parameter_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::put_curve_parametric_smile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_parametric_smile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::put_many_curve_parametric_smile_parameters_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::put_many_curve_parametric_smile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_parametric_smile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::put_many_curve_parametric_smile_parameters_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::put_many_curve_parametric_smile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_parametric_smile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_parameter_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_curve_parametric_smile_parameter_request& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_parametric_smile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_parameter_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_curve_parametric_smile_parameter_response& v);
    static ores::refdata::messaging::delete_curve_parametric_smile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_request& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_response& v);
    static ores::refdata::messaging::delete_many_curve_parametric_smile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_request& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_response& v);
    static ores::refdata::messaging::list_curve_parametric_smile_parameter_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_parameter_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_curve_parametric_smile_parameter_version_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_parametric_smile_parameter_version_request& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_parametric_smile_parameter_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_curve_parametric_smile_parameter_version_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_curve_parametric_smile_parameter_version_response& v);
    static ores::refdata::messaging::get_curve_parametric_smile_parameter_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
