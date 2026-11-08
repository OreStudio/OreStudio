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
#ifndef ORES_SYNTHETIC_API_MESSAGING_VARIANCE_CALIBRATION_INPUT_POINT_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_VARIANCE_CALIBRATION_INPUT_POINT_PROTOCOL_HPP

#include "ores.synthetic.api/domain/variance_calibration_input_point_codec.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct variance_calibration_input_point_key {
    boost::uuids::uuid id;

    friend bool operator==(const variance_calibration_input_point_key&,
                           const variance_calibration_input_point_key&) = default;
};

struct variance_calibration_input_point_write {
    boost::uuids::uuid id;
    boost::uuids::uuid input_id;
    std::chrono::year_month_day fixing_date;
    std::string fixing_text;
    std::optional<double> fixing_value;
    std::optional<double> log_return;
    bool used{};
    std::string exclusion_reason;

    friend bool operator==(const variance_calibration_input_point_write&,
                           const variance_calibration_input_point_write&) = default;
};

struct variance_calibration_input_point_change {
    variance_calibration_input_point_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const variance_calibration_input_point_change&,
                           const variance_calibration_input_point_change&) = default;
};

struct variance_calibration_input_point_removal {
    variance_calibration_input_point_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const variance_calibration_input_point_removal&,
                           const variance_calibration_input_point_removal&) = default;
};

struct variance_calibration_input_point_lookup {
    variance_calibration_input_point_key key;
    std::optional<ores::synthetic::domain::variance_calibration_input_point>
        variance_calibration_input_point;

    friend bool operator==(const variance_calibration_input_point_lookup&,
                           const variance_calibration_input_point_lookup&) = default;
};

struct variance_calibration_input_points_filter {
    std::optional<boost::uuids::uuid> input_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> input_id_one_of;

    friend bool operator==(const variance_calibration_input_points_filter&,
                           const variance_calibration_input_points_filter&) = default;
};

struct variance_calibration_input_point_event {
    boost::uuids::uuid event_id;
    variance_calibration_input_point_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const variance_calibration_input_point_event&,
                           const variance_calibration_input_point_event&) = default;
};

struct variance_calibration_input_point_version_key {
    variance_calibration_input_point_key variance_calibration_input_point;
    std::uint32_t version{};

    friend bool operator==(const variance_calibration_input_point_version_key&,
                           const variance_calibration_input_point_version_key&) = default;
};

struct variance_calibration_input_point_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const variance_calibration_input_point_versions_filter&,
                           const variance_calibration_input_point_versions_filter&) = default;
};

struct list_variance_calibration_input_points_request {
    using response_type = struct list_variance_calibration_input_points_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.list";
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
    std::optional<variance_calibration_input_points_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_variance_calibration_input_points_request&,
                           const list_variance_calibration_input_points_request&) = default;
};

struct list_variance_calibration_input_points_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::variance_calibration_input_point>
        variance_calibration_input_points;
    std::uint64_t total{};

    friend bool operator==(const list_variance_calibration_input_points_response&,
                           const list_variance_calibration_input_points_response&) = default;
};

struct get_variance_calibration_input_point_request {
    using response_type = struct get_variance_calibration_input_point_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    variance_calibration_input_point_key key;

    friend bool operator==(const get_variance_calibration_input_point_request&,
                           const get_variance_calibration_input_point_request&) = default;
};

struct get_variance_calibration_input_point_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::variance_calibration_input_point>
        variance_calibration_input_point;

    friend bool operator==(const get_variance_calibration_input_point_response&,
                           const get_variance_calibration_input_point_response&) = default;
};

struct get_many_variance_calibration_input_points_request {
    using response_type = struct get_many_variance_calibration_input_points_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<variance_calibration_input_point_key> keys;

    friend bool operator==(const get_many_variance_calibration_input_points_request&,
                           const get_many_variance_calibration_input_points_request&) = default;
};

struct get_many_variance_calibration_input_points_response {
    ores::utility::domain::result result;
    std::vector<variance_calibration_input_point_lookup> entries;

    friend bool operator==(const get_many_variance_calibration_input_points_response&,
                           const get_many_variance_calibration_input_points_response&) = default;
};

struct put_variance_calibration_input_point_request {
    using response_type = struct put_variance_calibration_input_point_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    variance_calibration_input_point_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_variance_calibration_input_point_request&,
                           const put_variance_calibration_input_point_request&) = default;
};

struct put_variance_calibration_input_point_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::variance_calibration_input_point>
        variance_calibration_input_point;

    friend bool operator==(const put_variance_calibration_input_point_response&,
                           const put_variance_calibration_input_point_response&) = default;
};

struct put_many_variance_calibration_input_points_request {
    using response_type = struct put_many_variance_calibration_input_points_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<variance_calibration_input_point_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_variance_calibration_input_points_request&,
                           const put_many_variance_calibration_input_points_request&) = default;
};

struct put_many_variance_calibration_input_points_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::variance_calibration_input_point>
        variance_calibration_input_points;

    friend bool operator==(const put_many_variance_calibration_input_points_response&,
                           const put_many_variance_calibration_input_points_response&) = default;
};

struct delete_variance_calibration_input_point_request {
    using response_type = struct delete_variance_calibration_input_point_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    variance_calibration_input_point_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_variance_calibration_input_point_request&,
                           const delete_variance_calibration_input_point_request&) = default;
};

struct delete_variance_calibration_input_point_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_variance_calibration_input_point_response&,
                           const delete_variance_calibration_input_point_response&) = default;
};

struct delete_many_variance_calibration_input_points_request {
    using response_type = struct delete_many_variance_calibration_input_points_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<variance_calibration_input_point_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_variance_calibration_input_points_request&,
                           const delete_many_variance_calibration_input_points_request&) = default;
};

struct delete_many_variance_calibration_input_points_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_variance_calibration_input_points_response&,
                           const delete_many_variance_calibration_input_points_response&) = default;
};

struct list_variance_calibration_input_point_versions_request {
    using response_type = struct list_variance_calibration_input_point_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    variance_calibration_input_point_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<variance_calibration_input_point_versions_filter> filter;

    friend bool operator==(const list_variance_calibration_input_point_versions_request&,
                           const list_variance_calibration_input_point_versions_request&) = default;
};

struct list_variance_calibration_input_point_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::variance_calibration_input_point> versions;
    std::uint64_t total{};

    friend bool
    operator==(const list_variance_calibration_input_point_versions_response&,
               const list_variance_calibration_input_point_versions_response&) = default;
};

struct get_variance_calibration_input_point_version_request {
    using response_type = struct get_variance_calibration_input_point_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.variance_calibration_input_points_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    variance_calibration_input_point_version_key key;

    friend bool operator==(const get_variance_calibration_input_point_version_request&,
                           const get_variance_calibration_input_point_version_request&) = default;
};

struct get_variance_calibration_input_point_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::variance_calibration_input_point> version;

    friend bool operator==(const get_variance_calibration_input_point_version_response&,
                           const get_variance_calibration_input_point_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace variance_calibration_input_point_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.events.variance_calibration_input_points.created";
inline constexpr std::string_view updated =
    "synthetic.v1.events.variance_calibration_input_points.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.events.variance_calibration_input_points.deleted";
}
/**
 * @brief The members of variance_calibration_input_point_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<variance_calibration_input_point_key>) {
    return std::tuple{
        utility::codec::field{"id", &variance_calibration_input_point_key::id},
    };
}

/**
 * @brief The members of variance_calibration_input_point_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_write>) {
    return std::tuple{
        utility::codec::field{"id", &variance_calibration_input_point_write::id},
        utility::codec::field{"input_id", &variance_calibration_input_point_write::input_id},
        utility::codec::field{"fixing_date", &variance_calibration_input_point_write::fixing_date},
        utility::codec::field{"fixing_text", &variance_calibration_input_point_write::fixing_text},
        utility::codec::field{"fixing_value",
                              &variance_calibration_input_point_write::fixing_value},
        utility::codec::field{"log_return", &variance_calibration_input_point_write::log_return},
        utility::codec::field{"used", &variance_calibration_input_point_write::used},
        utility::codec::field{"exclusion_reason",
                              &variance_calibration_input_point_write::exclusion_reason},
    };
}

/**
 * @brief The members of variance_calibration_input_point_change, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_change>) {
    return std::tuple{
        utility::codec::field{"write", &variance_calibration_input_point_change::write},
        utility::codec::field{"precondition",
                              &variance_calibration_input_point_change::precondition},
    };
}

/**
 * @brief The members of variance_calibration_input_point_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_removal>) {
    return std::tuple{
        utility::codec::field{"key", &variance_calibration_input_point_removal::key},
        utility::codec::field{"precondition",
                              &variance_calibration_input_point_removal::precondition},
    };
}

/**
 * @brief The members of variance_calibration_input_point_lookup, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &variance_calibration_input_point_lookup::key},
        utility::codec::field{
            "variance_calibration_input_point",
            &variance_calibration_input_point_lookup::variance_calibration_input_point},
    };
}

/**
 * @brief The members of variance_calibration_input_points_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_points_filter>) {
    return std::tuple{
        utility::codec::field{"input_id", &variance_calibration_input_points_filter::input_id},
        utility::codec::field{"id_one_of", &variance_calibration_input_points_filter::id_one_of},
        utility::codec::field{"input_id_one_of",
                              &variance_calibration_input_points_filter::input_id_one_of},
    };
}

/**
 * @brief The members of variance_calibration_input_point_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &variance_calibration_input_point_event::event_id},
        utility::codec::field{"key", &variance_calibration_input_point_event::key},
        utility::codec::field{"action", &variance_calibration_input_point_event::action},
        utility::codec::field{"version", &variance_calibration_input_point_event::version},
        utility::codec::field{"occurred_at", &variance_calibration_input_point_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &variance_calibration_input_point_event::correlation_id},
    };
}

/**
 * @brief The members of variance_calibration_input_point_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_version_key>) {
    return std::tuple{
        utility::codec::field{
            "variance_calibration_input_point",
            &variance_calibration_input_point_version_key::variance_calibration_input_point},
        utility::codec::field{"version", &variance_calibration_input_point_version_key::version},
    };
}

/**
 * @brief The members of variance_calibration_input_point_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<variance_calibration_input_point_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version",
                              &variance_calibration_input_point_versions_filter::version},
        utility::codec::field{"from_version",
                              &variance_calibration_input_point_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &variance_calibration_input_point_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_variance_calibration_input_points_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_variance_calibration_input_points_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_variance_calibration_input_points_request::offset},
        utility::codec::field{"limit", &list_variance_calibration_input_points_request::limit},
        utility::codec::field{"order", &list_variance_calibration_input_points_request::order},
        utility::codec::field{"filter", &list_variance_calibration_input_points_request::filter},
        utility::codec::field{"as_of", &list_variance_calibration_input_points_request::as_of},
    };
}

/**
 * @brief The members of list_variance_calibration_input_points_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_variance_calibration_input_points_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_variance_calibration_input_points_response::result},
        utility::codec::field{
            "variance_calibration_input_points",
            &list_variance_calibration_input_points_response::variance_calibration_input_points},
        utility::codec::field{"total", &list_variance_calibration_input_points_response::total},
    };
}

/**
 * @brief The members of get_variance_calibration_input_point_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_variance_calibration_input_point_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_variance_calibration_input_point_request::key},
    };
}

/**
 * @brief The members of get_variance_calibration_input_point_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_variance_calibration_input_point_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_variance_calibration_input_point_response::result},
        utility::codec::field{
            "variance_calibration_input_point",
            &get_variance_calibration_input_point_response::variance_calibration_input_point},
    };
}

/**
 * @brief The members of get_many_variance_calibration_input_points_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_variance_calibration_input_points_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_variance_calibration_input_points_request::keys},
    };
}

/**
 * @brief The members of get_many_variance_calibration_input_points_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_variance_calibration_input_points_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_variance_calibration_input_points_response::result},
        utility::codec::field{"entries",
                              &get_many_variance_calibration_input_points_response::entries},
    };
}

/**
 * @brief The members of put_variance_calibration_input_point_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_variance_calibration_input_point_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_variance_calibration_input_point_request::change},
        utility::codec::field{"intent", &put_variance_calibration_input_point_request::intent},
    };
}

/**
 * @brief The members of put_variance_calibration_input_point_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_variance_calibration_input_point_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_variance_calibration_input_point_response::result},
        utility::codec::field{
            "variance_calibration_input_point",
            &put_variance_calibration_input_point_response::variance_calibration_input_point},
    };
}

/**
 * @brief The members of put_many_variance_calibration_input_points_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_variance_calibration_input_points_request>) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_variance_calibration_input_points_request::changes},
        utility::codec::field{"intent",
                              &put_many_variance_calibration_input_points_request::intent},
    };
}

/**
 * @brief The members of put_many_variance_calibration_input_points_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_variance_calibration_input_points_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_variance_calibration_input_points_response::result},
        utility::codec::field{"variance_calibration_input_points",
                              &put_many_variance_calibration_input_points_response::
                                  variance_calibration_input_points},
    };
}

/**
 * @brief The members of delete_variance_calibration_input_point_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_variance_calibration_input_point_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_variance_calibration_input_point_request::removal},
        utility::codec::field{"intent", &delete_variance_calibration_input_point_request::intent},
    };
}

/**
 * @brief The members of delete_variance_calibration_input_point_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_variance_calibration_input_point_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_variance_calibration_input_point_response::result},
    };
}

/**
 * @brief The members of delete_many_variance_calibration_input_points_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_variance_calibration_input_points_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_variance_calibration_input_points_request::removals},
        utility::codec::field{"intent",
                              &delete_many_variance_calibration_input_points_request::intent},
    };
}

/**
 * @brief The members of delete_many_variance_calibration_input_points_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_variance_calibration_input_points_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_variance_calibration_input_points_response::result},
    };
}

/**
 * @brief The members of list_variance_calibration_input_point_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_variance_calibration_input_point_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_variance_calibration_input_point_versions_request::key},
        utility::codec::field{"offset",
                              &list_variance_calibration_input_point_versions_request::offset},
        utility::codec::field{"limit",
                              &list_variance_calibration_input_point_versions_request::limit},
        utility::codec::field{"order",
                              &list_variance_calibration_input_point_versions_request::order},
        utility::codec::field{"filter",
                              &list_variance_calibration_input_point_versions_request::filter},
    };
}

/**
 * @brief The members of list_variance_calibration_input_point_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_variance_calibration_input_point_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_variance_calibration_input_point_versions_response::result},
        utility::codec::field{"versions",
                              &list_variance_calibration_input_point_versions_response::versions},
        utility::codec::field{"total",
                              &list_variance_calibration_input_point_versions_response::total},
    };
}

/**
 * @brief The members of get_variance_calibration_input_point_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_variance_calibration_input_point_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_variance_calibration_input_point_version_request::key},
    };
}

/**
 * @brief The members of get_variance_calibration_input_point_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_variance_calibration_input_point_version_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_variance_calibration_input_point_version_response::result},
        utility::codec::field{"version",
                              &get_variance_calibration_input_point_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_key& v);
    static ores::synthetic::messaging::variance_calibration_input_point_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_key& v);
    static ores::synthetic::messaging::variance_calibration_input_point_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_write& v);
    static ores::synthetic::messaging::variance_calibration_input_point_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_write& v);
    static ores::synthetic::messaging::variance_calibration_input_point_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_change& v);
    static ores::synthetic::messaging::variance_calibration_input_point_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_change& v);
    static ores::synthetic::messaging::variance_calibration_input_point_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_removal& v);
    static ores::synthetic::messaging::variance_calibration_input_point_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_removal& v);
    static ores::synthetic::messaging::variance_calibration_input_point_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_lookup& v);
    static ores::synthetic::messaging::variance_calibration_input_point_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_lookup& v);
    static ores::synthetic::messaging::variance_calibration_input_point_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_points_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_points_filter& v);
    static ores::synthetic::messaging::variance_calibration_input_points_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_points_filter& v);
    static ores::synthetic::messaging::variance_calibration_input_points_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_event& v);
    static ores::synthetic::messaging::variance_calibration_input_point_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::variance_calibration_input_point_event& v);
    static ores::synthetic::messaging::variance_calibration_input_point_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::variance_calibration_input_point_version_key& v);
    static ores::synthetic::messaging::variance_calibration_input_point_version_key
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::variance_calibration_input_point_version_key& v);
    static ores::synthetic::messaging::variance_calibration_input_point_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::variance_calibration_input_point_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::variance_calibration_input_point_versions_filter& v);
    static ores::synthetic::messaging::variance_calibration_input_point_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::variance_calibration_input_point_versions_filter& v);
    static ores::synthetic::messaging::variance_calibration_input_point_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_variance_calibration_input_points_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::list_variance_calibration_input_points_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::list_variance_calibration_input_points_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_variance_calibration_input_points_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::list_variance_calibration_input_points_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::list_variance_calibration_input_points_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_variance_calibration_input_point_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_variance_calibration_input_point_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_variance_calibration_input_points_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::get_many_variance_calibration_input_points_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::get_many_variance_calibration_input_points_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_variance_calibration_input_points_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_many_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::get_many_variance_calibration_input_points_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::get_many_variance_calibration_input_points_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_variance_calibration_input_point_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::put_variance_calibration_input_point_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::put_variance_calibration_input_point_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_variance_calibration_input_point_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::put_variance_calibration_input_point_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::put_variance_calibration_input_point_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_variance_calibration_input_points_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::put_many_variance_calibration_input_points_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::put_many_variance_calibration_input_points_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_variance_calibration_input_points_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_many_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::put_many_variance_calibration_input_points_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_variance_calibration_input_points_response& v);
    static ores::synthetic::messaging::put_many_variance_calibration_input_points_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_variance_calibration_input_point_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::delete_variance_calibration_input_point_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_variance_calibration_input_point_request& v);
    static ores::synthetic::messaging::delete_variance_calibration_input_point_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_variance_calibration_input_point_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::delete_variance_calibration_input_point_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_variance_calibration_input_point_response& v);
    static ores::synthetic::messaging::delete_variance_calibration_input_point_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_variance_calibration_input_points_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::delete_many_variance_calibration_input_points_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_variance_calibration_input_points_request& v);
    static ores::synthetic::messaging::delete_many_variance_calibration_input_points_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::delete_many_variance_calibration_input_points_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_variance_calibration_input_points_response&
            v);
    static ores::synthetic::messaging::delete_many_variance_calibration_input_points_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_variance_calibration_input_points_response&
            v);
    static ores::synthetic::messaging::delete_many_variance_calibration_input_points_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_variance_calibration_input_point_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_variance_calibration_input_point_versions_request&
            v);
    static ores::synthetic::messaging::list_variance_calibration_input_point_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_variance_calibration_input_point_versions_request&
            v);
    static ores::synthetic::messaging::list_variance_calibration_input_point_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<
    ores::synthetic::messaging::list_variance_calibration_input_point_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_variance_calibration_input_point_versions_response&
            v);
    static ores::synthetic::messaging::list_variance_calibration_input_point_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_variance_calibration_input_point_versions_response&
            v);
    static ores::synthetic::messaging::list_variance_calibration_input_point_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_variance_calibration_input_point_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_variance_calibration_input_point_version_request& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_variance_calibration_input_point_version_request& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_variance_calibration_input_point_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_variance_calibration_input_point_version_response& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_variance_calibration_input_point_version_response& v);
    static ores::synthetic::messaging::get_variance_calibration_input_point_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
