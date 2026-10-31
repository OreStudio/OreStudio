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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_ANCHOR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_ANCHOR_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_anchor.hpp"
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

struct tenor_anchor_key {
    std::string code;
};

struct tenor_anchor_write {
    std::string code;
    std::string description;
    int display_order;
};

struct tenor_anchor_change {
    tenor_anchor_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_anchor_removal {
    tenor_anchor_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_anchor_lookup {
    tenor_anchor_key key;
    std::optional<ores::refdata::domain::tenor_anchor> tenor_anchor;
};

struct tenor_anchors_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct tenor_anchor_event {
    boost::uuids::uuid event_id;
    tenor_anchor_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenor_anchor_version_key {
    tenor_anchor_key tenor_anchor;
    std::uint32_t version;
};

struct tenor_anchor_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenor_anchors_request {
    using response_type = struct list_tenor_anchors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.list";
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
    std::optional<tenor_anchors_filter> filter;
    std::optional<std::string> as_of;
};

struct list_tenor_anchors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_anchor> anchors;
    std::uint64_t total;
};

struct get_tenor_anchor_request {
    using response_type = struct get_tenor_anchor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_anchor_key key;
};

struct get_tenor_anchor_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_anchor> tenor_anchor;
};

struct get_many_tenor_anchors_request {
    using response_type = struct get_many_tenor_anchors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_anchor_key> keys;
};

struct get_many_tenor_anchors_response {
    ores::utility::domain::result result;
    std::vector<tenor_anchor_lookup> entries;
};

struct put_tenor_anchor_request {
    using response_type = struct put_tenor_anchor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_anchor_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_anchor_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_anchor> tenor_anchor;
};

struct put_many_tenor_anchors_request {
    using response_type = struct put_many_tenor_anchors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_anchor_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenor_anchors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_anchor> anchors;
};

struct delete_tenor_anchor_request {
    using response_type = struct delete_tenor_anchor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_anchor_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_anchor_response {
    ores::utility::domain::result result;
};

struct delete_many_tenor_anchors_request {
    using response_type = struct delete_many_tenor_anchors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_anchor_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenor_anchors_response {
    ores::utility::domain::result result;
};

struct list_tenor_anchor_versions_request {
    using response_type = struct list_tenor_anchor_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_anchor_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_anchor_versions_filter> filter;
};

struct list_tenor_anchor_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_anchor> versions;
    std::uint64_t total;
};

struct get_tenor_anchor_version_request {
    using response_type = struct get_tenor_anchor_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_anchors_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_anchor_version_key key;
};

struct get_tenor_anchor_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_anchor> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenor_anchor_event_subjects {
inline constexpr std::string_view created = "refdata.v1.tenor_anchors_events.created";
inline constexpr std::string_view updated = "refdata.v1.tenor_anchors_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.tenor_anchors_events.deleted";
}
/**
 * @brief The members of tenor_anchor_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_key&) {
    return std::tuple{
        utility::codec::field{"code", &tenor_anchor_key::code},
    };
}

/**
 * @brief The members of tenor_anchor_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_write&) {
    return std::tuple{
        utility::codec::field{"code", &tenor_anchor_write::code},
        utility::codec::field{"description", &tenor_anchor_write::description},
        utility::codec::field{"display_order", &tenor_anchor_write::display_order},
    };
}

/**
 * @brief The members of tenor_anchor_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_change&) {
    return std::tuple{
        utility::codec::field{"write", &tenor_anchor_change::write},
        utility::codec::field{"precondition", &tenor_anchor_change::precondition},
    };
}

/**
 * @brief The members of tenor_anchor_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_removal&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_anchor_removal::key},
        utility::codec::field{"precondition", &tenor_anchor_removal::precondition},
    };
}

/**
 * @brief The members of tenor_anchor_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &tenor_anchor_lookup::key},
        utility::codec::field{"tenor_anchor", &tenor_anchor_lookup::tenor_anchor},
    };
}

/**
 * @brief The members of tenor_anchors_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchors_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &tenor_anchors_filter::code_one_of},
    };
}

/**
 * @brief The members of tenor_anchor_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &tenor_anchor_event::event_id},
        utility::codec::field{"key", &tenor_anchor_event::key},
        utility::codec::field{"action", &tenor_anchor_event::action},
        utility::codec::field{"version", &tenor_anchor_event::version},
        utility::codec::field{"occurred_at", &tenor_anchor_event::occurred_at},
        utility::codec::field{"correlation_id", &tenor_anchor_event::correlation_id},
    };
}

/**
 * @brief The members of tenor_anchor_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_version_key&) {
    return std::tuple{
        utility::codec::field{"tenor_anchor", &tenor_anchor_version_key::tenor_anchor},
        utility::codec::field{"version", &tenor_anchor_version_key::version},
    };
}

/**
 * @brief The members of tenor_anchor_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const tenor_anchor_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &tenor_anchor_versions_filter::version},
        utility::codec::field{"from_version", &tenor_anchor_versions_filter::from_version},
        utility::codec::field{"to_version", &tenor_anchor_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_tenor_anchors_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_anchors_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_tenor_anchors_request::offset},
        utility::codec::field{"limit", &list_tenor_anchors_request::limit},
        utility::codec::field{"order", &list_tenor_anchors_request::order},
        utility::codec::field{"filter", &list_tenor_anchors_request::filter},
        utility::codec::field{"as_of", &list_tenor_anchors_request::as_of},
    };
}

/**
 * @brief The members of list_tenor_anchors_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_anchors_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenor_anchors_response::result},
        utility::codec::field{"anchors", &list_tenor_anchors_response::anchors},
        utility::codec::field{"total", &list_tenor_anchors_response::total},
    };
}

/**
 * @brief The members of get_tenor_anchor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_anchor_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenor_anchor_request::key},
    };
}

/**
 * @brief The members of get_tenor_anchor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_anchor_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenor_anchor_response::result},
        utility::codec::field{"tenor_anchor", &get_tenor_anchor_response::tenor_anchor},
    };
}

/**
 * @brief The members of get_many_tenor_anchors_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenor_anchors_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_tenor_anchors_request::keys},
    };
}

/**
 * @brief The members of get_many_tenor_anchors_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_tenor_anchors_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_tenor_anchors_response::result},
        utility::codec::field{"entries", &get_many_tenor_anchors_response::entries},
    };
}

/**
 * @brief The members of put_tenor_anchor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_anchor_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_tenor_anchor_request::change},
        utility::codec::field{"intent", &put_tenor_anchor_request::intent},
    };
}

/**
 * @brief The members of put_tenor_anchor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_tenor_anchor_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_tenor_anchor_response::result},
        utility::codec::field{"tenor_anchor", &put_tenor_anchor_response::tenor_anchor},
    };
}

/**
 * @brief The members of put_many_tenor_anchors_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenor_anchors_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_tenor_anchors_request::changes},
        utility::codec::field{"intent", &put_many_tenor_anchors_request::intent},
    };
}

/**
 * @brief The members of put_many_tenor_anchors_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_tenor_anchors_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_tenor_anchors_response::result},
        utility::codec::field{"anchors", &put_many_tenor_anchors_response::anchors},
    };
}

/**
 * @brief The members of delete_tenor_anchor_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_anchor_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_tenor_anchor_request::removal},
        utility::codec::field{"intent", &delete_tenor_anchor_request::intent},
    };
}

/**
 * @brief The members of delete_tenor_anchor_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_tenor_anchor_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_tenor_anchor_response::result},
    };
}

/**
 * @brief The members of delete_many_tenor_anchors_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenor_anchors_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_tenor_anchors_request::removals},
        utility::codec::field{"intent", &delete_many_tenor_anchors_request::intent},
    };
}

/**
 * @brief The members of delete_many_tenor_anchors_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_tenor_anchors_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_tenor_anchors_response::result},
    };
}

/**
 * @brief The members of list_tenor_anchor_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_anchor_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_tenor_anchor_versions_request::key},
        utility::codec::field{"offset", &list_tenor_anchor_versions_request::offset},
        utility::codec::field{"limit", &list_tenor_anchor_versions_request::limit},
        utility::codec::field{"order", &list_tenor_anchor_versions_request::order},
        utility::codec::field{"filter", &list_tenor_anchor_versions_request::filter},
    };
}

/**
 * @brief The members of list_tenor_anchor_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_tenor_anchor_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_tenor_anchor_versions_response::result},
        utility::codec::field{"versions", &list_tenor_anchor_versions_response::versions},
        utility::codec::field{"total", &list_tenor_anchor_versions_response::total},
    };
}

/**
 * @brief The members of get_tenor_anchor_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_anchor_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_tenor_anchor_version_request::key},
    };
}

/**
 * @brief The members of get_tenor_anchor_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_tenor_anchor_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_tenor_anchor_version_response::result},
        utility::codec::field{"version", &get_tenor_anchor_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_key& v);
    static ores::refdata::messaging::tenor_anchor_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_key& v);
    static ores::refdata::messaging::tenor_anchor_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_write& v);
    static ores::refdata::messaging::tenor_anchor_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_write& v);
    static ores::refdata::messaging::tenor_anchor_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_change& v);
    static ores::refdata::messaging::tenor_anchor_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_change& v);
    static ores::refdata::messaging::tenor_anchor_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_removal& v);
    static ores::refdata::messaging::tenor_anchor_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_removal& v);
    static ores::refdata::messaging::tenor_anchor_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_lookup& v);
    static ores::refdata::messaging::tenor_anchor_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_lookup& v);
    static ores::refdata::messaging::tenor_anchor_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchors_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchors_filter& v);
    static ores::refdata::messaging::tenor_anchors_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchors_filter& v);
    static ores::refdata::messaging::tenor_anchors_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_event& v);
    static ores::refdata::messaging::tenor_anchor_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_event& v);
    static ores::refdata::messaging::tenor_anchor_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_version_key& v);
    static ores::refdata::messaging::tenor_anchor_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::tenor_anchor_version_key& v);
    static ores::refdata::messaging::tenor_anchor_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::tenor_anchor_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::tenor_anchor_versions_filter& v);
    static ores::refdata::messaging::tenor_anchor_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::tenor_anchor_versions_filter& v);
    static ores::refdata::messaging::tenor_anchor_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenor_anchors_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenor_anchors_request& v);
    static ores::refdata::messaging::list_tenor_anchors_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_tenor_anchors_request& v);
    static ores::refdata::messaging::list_tenor_anchors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_tenor_anchors_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_tenor_anchors_response& v);
    static ores::refdata::messaging::list_tenor_anchors_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_anchors_response& v);
    static ores::refdata::messaging::list_tenor_anchors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_anchor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_anchor_request& v);
    static ores::refdata::messaging::get_tenor_anchor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_anchor_request& v);
    static ores::refdata::messaging::get_tenor_anchor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_tenor_anchor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_tenor_anchor_response& v);
    static ores::refdata::messaging::get_tenor_anchor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_tenor_anchor_response& v);
    static ores::refdata::messaging::get_tenor_anchor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_tenor_anchors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_tenor_anchors_request& v);
    static ores::refdata::messaging::get_many_tenor_anchors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_tenor_anchors_request& v);
    static ores::refdata::messaging::get_many_tenor_anchors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_tenor_anchors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_tenor_anchors_response& v);
    static ores::refdata::messaging::get_many_tenor_anchors_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_tenor_anchors_response& v);
    static ores::refdata::messaging::get_many_tenor_anchors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_tenor_anchor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_tenor_anchor_request& v);
    static ores::refdata::messaging::put_tenor_anchor_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_tenor_anchor_request& v);
    static ores::refdata::messaging::put_tenor_anchor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_tenor_anchor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_tenor_anchor_response& v);
    static ores::refdata::messaging::put_tenor_anchor_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_tenor_anchor_response& v);
    static ores::refdata::messaging::put_tenor_anchor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_tenor_anchors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_tenor_anchors_request& v);
    static ores::refdata::messaging::put_many_tenor_anchors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_tenor_anchors_request& v);
    static ores::refdata::messaging::put_many_tenor_anchors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_tenor_anchors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_tenor_anchors_response& v);
    static ores::refdata::messaging::put_many_tenor_anchors_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_tenor_anchors_response& v);
    static ores::refdata::messaging::put_many_tenor_anchors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_tenor_anchor_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_tenor_anchor_request& v);
    static ores::refdata::messaging::delete_tenor_anchor_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_tenor_anchor_request& v);
    static ores::refdata::messaging::delete_tenor_anchor_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_tenor_anchor_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_tenor_anchor_response& v);
    static ores::refdata::messaging::delete_tenor_anchor_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_tenor_anchor_response& v);
    static ores::refdata::messaging::delete_tenor_anchor_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_tenor_anchors_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_tenor_anchors_request& v);
    static ores::refdata::messaging::delete_many_tenor_anchors_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_tenor_anchors_request& v);
    static ores::refdata::messaging::delete_many_tenor_anchors_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_tenor_anchors_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_tenor_anchors_response& v);
    static ores::refdata::messaging::delete_many_tenor_anchors_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_tenor_anchors_response& v);
    static ores::refdata::messaging::delete_many_tenor_anchors_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_tenor_anchor_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_tenor_anchor_versions_request& v);
    static ores::refdata::messaging::list_tenor_anchor_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_anchor_versions_request& v);
    static ores::refdata::messaging::list_tenor_anchor_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_tenor_anchor_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_tenor_anchor_versions_response& v);
    static ores::refdata::messaging::list_tenor_anchor_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_tenor_anchor_versions_response& v);
    static ores::refdata::messaging::list_tenor_anchor_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_tenor_anchor_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_tenor_anchor_version_request& v);
    static ores::refdata::messaging::get_tenor_anchor_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_tenor_anchor_version_request& v);
    static ores::refdata::messaging::get_tenor_anchor_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_tenor_anchor_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_tenor_anchor_version_response& v);
    static ores::refdata::messaging::get_tenor_anchor_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_tenor_anchor_version_response& v);
    static ores::refdata::messaging::get_tenor_anchor_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
