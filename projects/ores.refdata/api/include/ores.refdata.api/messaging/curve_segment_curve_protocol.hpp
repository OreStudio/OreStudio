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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_CURVE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_SEGMENT_CURVE_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_segment_curve.hpp"
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

struct curve_segment_curve_key {
    boost::uuids::uuid id;
};

struct curve_segment_curve_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_segment_id;
    std::string role;
    std::string curve;
    std::optional<std::string> index_name;
    std::optional<double> weight;
    int position;
};

struct curve_segment_curve_change {
    curve_segment_curve_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_segment_curve_removal {
    curve_segment_curve_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_segment_curve_lookup {
    curve_segment_curve_key key;
    std::optional<ores::refdata::domain::curve_segment_curve> curve_segment_curve;
};

struct curve_segment_curves_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct curve_segment_curve_event {
    boost::uuids::uuid event_id;
    curve_segment_curve_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_segment_curve_version_key {
    curve_segment_curve_key curve_segment_curve;
    std::uint32_t version;
};

struct curve_segment_curve_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_segment_curves_request {
    using response_type = struct list_curve_segment_curves_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.list";
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
    std::optional<curve_segment_curves_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_segment_curves_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_curve> segment_curves;
    std::uint64_t total;
};

struct get_curve_segment_curve_request {
    using response_type = struct get_curve_segment_curve_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_curve_key key;
};

struct get_curve_segment_curve_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_curve> curve_segment_curve;
};

struct get_many_curve_segment_curves_request {
    using response_type = struct get_many_curve_segment_curves_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_curve_key> keys;
};

struct get_many_curve_segment_curves_response {
    ores::utility::domain::result result;
    std::vector<curve_segment_curve_lookup> entries;
};

struct put_curve_segment_curve_request {
    using response_type = struct put_curve_segment_curve_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_curve_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_segment_curve_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_curve> curve_segment_curve;
};

struct put_many_curve_segment_curves_request {
    using response_type = struct put_many_curve_segment_curves_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_curve_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_segment_curves_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_curve> segment_curves;
};

struct delete_curve_segment_curve_request {
    using response_type = struct delete_curve_segment_curve_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_curve_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_segment_curve_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_segment_curves_request {
    using response_type = struct delete_many_curve_segment_curves_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_segment_curve_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_segment_curves_response {
    ores::utility::domain::result result;
};

struct list_curve_segment_curve_versions_request {
    using response_type = struct list_curve_segment_curve_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.curve_segment_curves_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_curve_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_segment_curve_versions_filter> filter;
};

struct list_curve_segment_curve_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_segment_curve> versions;
    std::uint64_t total;
};

struct get_curve_segment_curve_version_request {
    using response_type = struct get_curve_segment_curve_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_segment_curves_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_segment_curve_version_key key;
};

struct get_curve_segment_curve_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_segment_curve> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_segment_curve_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_segment_curves_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_segment_curves_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_segment_curves_events.deleted";
}
/**
 * @brief The members of curve_segment_curve_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_curve_key::id},
    };
}

/**
 * @brief The members of curve_segment_curve_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_write&) {
    return std::tuple{
        utility::codec::field{"id", &curve_segment_curve_write::id},
        utility::codec::field{"curve_segment_id", &curve_segment_curve_write::curve_segment_id},
        utility::codec::field{"role", &curve_segment_curve_write::role},
        utility::codec::field{"curve", &curve_segment_curve_write::curve},
        utility::codec::field{"index_name", &curve_segment_curve_write::index_name},
        utility::codec::field{"weight", &curve_segment_curve_write::weight},
        utility::codec::field{"position", &curve_segment_curve_write::position},
    };
}

/**
 * @brief The members of curve_segment_curve_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_change&) {
    return std::tuple{
        utility::codec::field{"write", &curve_segment_curve_change::write},
        utility::codec::field{"precondition", &curve_segment_curve_change::precondition},
    };
}

/**
 * @brief The members of curve_segment_curve_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_removal&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_curve_removal::key},
        utility::codec::field{"precondition", &curve_segment_curve_removal::precondition},
    };
}

/**
 * @brief The members of curve_segment_curve_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_segment_curve_lookup::key},
        utility::codec::field{"curve_segment_curve",
                              &curve_segment_curve_lookup::curve_segment_curve},
    };
}

/**
 * @brief The members of curve_segment_curves_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curves_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &curve_segment_curves_filter::id_one_of},
    };
}

/**
 * @brief The members of curve_segment_curve_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_segment_curve_event::event_id},
        utility::codec::field{"key", &curve_segment_curve_event::key},
        utility::codec::field{"action", &curve_segment_curve_event::action},
        utility::codec::field{"version", &curve_segment_curve_event::version},
        utility::codec::field{"occurred_at", &curve_segment_curve_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_segment_curve_event::correlation_id},
    };
}

/**
 * @brief The members of curve_segment_curve_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_segment_curve",
                              &curve_segment_curve_version_key::curve_segment_curve},
        utility::codec::field{"version", &curve_segment_curve_version_key::version},
    };
}

/**
 * @brief The members of curve_segment_curve_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_segment_curve_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_segment_curve_versions_filter::version},
        utility::codec::field{"from_version", &curve_segment_curve_versions_filter::from_version},
        utility::codec::field{"to_version", &curve_segment_curve_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_segment_curves_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_curves_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_segment_curves_request::offset},
        utility::codec::field{"limit", &list_curve_segment_curves_request::limit},
        utility::codec::field{"order", &list_curve_segment_curves_request::order},
        utility::codec::field{"filter", &list_curve_segment_curves_request::filter},
        utility::codec::field{"as_of", &list_curve_segment_curves_request::as_of},
    };
}

/**
 * @brief The members of list_curve_segment_curves_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_curves_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_segment_curves_response::result},
        utility::codec::field{"segment_curves",
                              &list_curve_segment_curves_response::segment_curves},
        utility::codec::field{"total", &list_curve_segment_curves_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_curve_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_curve_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_curve_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_curve_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_curve_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_curve_response::result},
        utility::codec::field{"curve_segment_curve",
                              &get_curve_segment_curve_response::curve_segment_curve},
    };
}

/**
 * @brief The members of get_many_curve_segment_curves_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segment_curves_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_segment_curves_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_segment_curves_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_segment_curves_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_segment_curves_response::result},
        utility::codec::field{"entries", &get_many_curve_segment_curves_response::entries},
    };
}

/**
 * @brief The members of put_curve_segment_curve_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_curve_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_curve_segment_curve_request::change},
        utility::codec::field{"intent", &put_curve_segment_curve_request::intent},
    };
}

/**
 * @brief The members of put_curve_segment_curve_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_curve_segment_curve_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_curve_segment_curve_response::result},
        utility::codec::field{"curve_segment_curve",
                              &put_curve_segment_curve_response::curve_segment_curve},
    };
}

/**
 * @brief The members of put_many_curve_segment_curves_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segment_curves_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_curve_segment_curves_request::changes},
        utility::codec::field{"intent", &put_many_curve_segment_curves_request::intent},
    };
}

/**
 * @brief The members of put_many_curve_segment_curves_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_curve_segment_curves_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_curve_segment_curves_response::result},
        utility::codec::field{"segment_curves",
                              &put_many_curve_segment_curves_response::segment_curves},
    };
}

/**
 * @brief The members of delete_curve_segment_curve_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_curve_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_curve_segment_curve_request::removal},
        utility::codec::field{"intent", &delete_curve_segment_curve_request::intent},
    };
}

/**
 * @brief The members of delete_curve_segment_curve_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_curve_segment_curve_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_curve_segment_curve_response::result},
    };
}

/**
 * @brief The members of delete_many_curve_segment_curves_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_segment_curves_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_curve_segment_curves_request::removals},
        utility::codec::field{"intent", &delete_many_curve_segment_curves_request::intent},
    };
}

/**
 * @brief The members of delete_many_curve_segment_curves_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_curve_segment_curves_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_curve_segment_curves_response::result},
    };
}

/**
 * @brief The members of list_curve_segment_curve_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_curve_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_segment_curve_versions_request::key},
        utility::codec::field{"offset", &list_curve_segment_curve_versions_request::offset},
        utility::codec::field{"limit", &list_curve_segment_curve_versions_request::limit},
        utility::codec::field{"order", &list_curve_segment_curve_versions_request::order},
        utility::codec::field{"filter", &list_curve_segment_curve_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_segment_curve_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_segment_curve_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_segment_curve_versions_response::result},
        utility::codec::field{"versions", &list_curve_segment_curve_versions_response::versions},
        utility::codec::field{"total", &list_curve_segment_curve_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_segment_curve_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_curve_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_segment_curve_version_request::key},
    };
}

/**
 * @brief The members of get_curve_segment_curve_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_segment_curve_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_segment_curve_version_response::result},
        utility::codec::field{"version", &get_curve_segment_curve_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_key& v);
    static ores::refdata::messaging::curve_segment_curve_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_curve_key& v);
    static ores::refdata::messaging::curve_segment_curve_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_write& v);
    static ores::refdata::messaging::curve_segment_curve_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_curve_write& v);
    static ores::refdata::messaging::curve_segment_curve_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_change& v);
    static ores::refdata::messaging::curve_segment_curve_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_curve_change& v);
    static ores::refdata::messaging::curve_segment_curve_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_removal& v);
    static ores::refdata::messaging::curve_segment_curve_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_curve_removal& v);
    static ores::refdata::messaging::curve_segment_curve_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_lookup& v);
    static ores::refdata::messaging::curve_segment_curve_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_curve_lookup& v);
    static ores::refdata::messaging::curve_segment_curve_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curves_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curves_filter& v);
    static ores::refdata::messaging::curve_segment_curves_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_curves_filter& v);
    static ores::refdata::messaging::curve_segment_curves_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::curve_segment_curve_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::curve_segment_curve_event& v);
    static ores::refdata::messaging::curve_segment_curve_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::curve_segment_curve_event& v);
    static ores::refdata::messaging::curve_segment_curve_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_curve_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_curve_version_key& v);
    static ores::refdata::messaging::curve_segment_curve_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_curve_version_key& v);
    static ores::refdata::messaging::curve_segment_curve_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::curve_segment_curve_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::curve_segment_curve_versions_filter& v);
    static ores::refdata::messaging::curve_segment_curve_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::curve_segment_curve_versions_filter& v);
    static ores::refdata::messaging::curve_segment_curve_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_curves_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_curves_request& v);
    static ores::refdata::messaging::list_curve_segment_curves_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_curves_request& v);
    static ores::refdata::messaging::list_curve_segment_curves_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_curves_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_curves_response& v);
    static ores::refdata::messaging::list_curve_segment_curves_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_curves_response& v);
    static ores::refdata::messaging::list_curve_segment_curves_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_curve_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_curve_request& v);
    static ores::refdata::messaging::get_curve_segment_curve_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_curve_request& v);
    static ores::refdata::messaging::get_curve_segment_curve_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_curve_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_curve_response& v);
    static ores::refdata::messaging::get_curve_segment_curve_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_curve_response& v);
    static ores::refdata::messaging::get_curve_segment_curve_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_curves_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::get_many_curve_segment_curves_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::get_many_curve_segment_curves_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_curve_segment_curves_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::get_many_curve_segment_curves_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::get_many_curve_segment_curves_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_curve_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_curve_request& v);
    static ores::refdata::messaging::put_curve_segment_curve_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_curve_request& v);
    static ores::refdata::messaging::put_curve_segment_curve_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_curve_segment_curve_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_curve_segment_curve_response& v);
    static ores::refdata::messaging::put_curve_segment_curve_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_curve_segment_curve_response& v);
    static ores::refdata::messaging::put_curve_segment_curve_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_curves_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::put_many_curve_segment_curves_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::put_many_curve_segment_curves_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_curve_segment_curves_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::put_many_curve_segment_curves_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::put_many_curve_segment_curves_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_curve_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_curve_request& v);
    static ores::refdata::messaging::delete_curve_segment_curve_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_curve_request& v);
    static ores::refdata::messaging::delete_curve_segment_curve_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_curve_segment_curve_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_curve_segment_curve_response& v);
    static ores::refdata::messaging::delete_curve_segment_curve_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_curve_segment_curve_response& v);
    static ores::refdata::messaging::delete_curve_segment_curve_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_curves_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_curves_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_segment_curves_request& v);
    static ores::refdata::messaging::delete_many_curve_segment_curves_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_curve_segment_curves_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_curves_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_curve_segment_curves_response& v);
    static ores::refdata::messaging::delete_many_curve_segment_curves_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_curve_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_curve_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_curve_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_curve_versions_request& v);
    static ores::refdata::messaging::list_curve_segment_curve_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_curve_segment_curve_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_curve_segment_curve_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_curve_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_curve_segment_curve_versions_response& v);
    static ores::refdata::messaging::list_curve_segment_curve_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_curve_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_curve_version_request& v);
    static ores::refdata::messaging::get_curve_segment_curve_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_curve_version_request& v);
    static ores::refdata::messaging::get_curve_segment_curve_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_curve_segment_curve_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_curve_segment_curve_version_response& v);
    static ores::refdata::messaging::get_curve_segment_curve_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_curve_segment_curve_version_response& v);
    static ores::refdata::messaging::get_curve_segment_curve_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
