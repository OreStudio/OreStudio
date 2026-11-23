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
#ifndef ORES_SYNTHETIC_API_MESSAGING_STOPPED_FEED_NODE_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_STOPPED_FEED_NODE_PROTOCOL_HPP

#include "ores.synthetic.api/domain/stopped_feed_node_codec.hpp"
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

struct stopped_feed_node_key {
    std::string node_name;

    friend bool operator==(const stopped_feed_node_key&, const stopped_feed_node_key&) = default;
};

struct stopped_feed_node_write {
    std::string node_name;
    std::string kind;
    std::string provider_code;

    friend bool operator==(const stopped_feed_node_write&,
                           const stopped_feed_node_write&) = default;
};

struct stopped_feed_node_change {
    stopped_feed_node_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const stopped_feed_node_change&,
                           const stopped_feed_node_change&) = default;
};

struct stopped_feed_node_removal {
    stopped_feed_node_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const stopped_feed_node_removal&,
                           const stopped_feed_node_removal&) = default;
};

struct stopped_feed_node_lookup {
    stopped_feed_node_key key;
    std::optional<ores::synthetic::domain::stopped_feed_node> stopped_feed_node;

    friend bool operator==(const stopped_feed_node_lookup&,
                           const stopped_feed_node_lookup&) = default;
};

struct stopped_feed_nodes_filter {
    std::optional<std::vector<std::string>> node_name_one_of;

    friend bool operator==(const stopped_feed_nodes_filter&,
                           const stopped_feed_nodes_filter&) = default;
};

struct stopped_feed_node_event {
    boost::uuids::uuid event_id;
    stopped_feed_node_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const stopped_feed_node_event&,
                           const stopped_feed_node_event&) = default;
};

struct stopped_feed_node_version_key {
    stopped_feed_node_key stopped_feed_node;
    std::uint32_t version{};

    friend bool operator==(const stopped_feed_node_version_key&,
                           const stopped_feed_node_version_key&) = default;
};

struct stopped_feed_node_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const stopped_feed_node_versions_filter&,
                           const stopped_feed_node_versions_filter&) = default;
};

struct list_stopped_feed_nodes_request {
    using response_type = struct list_stopped_feed_nodes_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.list";
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
    std::optional<stopped_feed_nodes_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_stopped_feed_nodes_request&,
                           const list_stopped_feed_nodes_request&) = default;
};

struct list_stopped_feed_nodes_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::stopped_feed_node> stopped_nodes;
    std::uint64_t total{};

    friend bool operator==(const list_stopped_feed_nodes_response&,
                           const list_stopped_feed_nodes_response&) = default;
};

struct get_stopped_feed_node_request {
    using response_type = struct get_stopped_feed_node_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stopped_feed_node_key key;

    friend bool operator==(const get_stopped_feed_node_request&,
                           const get_stopped_feed_node_request&) = default;
};

struct get_stopped_feed_node_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::stopped_feed_node> stopped_feed_node;

    friend bool operator==(const get_stopped_feed_node_response&,
                           const get_stopped_feed_node_response&) = default;
};

struct get_many_stopped_feed_nodes_request {
    using response_type = struct get_many_stopped_feed_nodes_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stopped_feed_node_key> keys;

    friend bool operator==(const get_many_stopped_feed_nodes_request&,
                           const get_many_stopped_feed_nodes_request&) = default;
};

struct get_many_stopped_feed_nodes_response {
    ores::utility::domain::result result;
    std::vector<stopped_feed_node_lookup> entries;

    friend bool operator==(const get_many_stopped_feed_nodes_response&,
                           const get_many_stopped_feed_nodes_response&) = default;
};

struct put_stopped_feed_node_request {
    using response_type = struct put_stopped_feed_node_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stopped_feed_node_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_stopped_feed_node_request&,
                           const put_stopped_feed_node_request&) = default;
};

struct put_stopped_feed_node_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::stopped_feed_node> stopped_feed_node;

    friend bool operator==(const put_stopped_feed_node_response&,
                           const put_stopped_feed_node_response&) = default;
};

struct put_many_stopped_feed_nodes_request {
    using response_type = struct put_many_stopped_feed_nodes_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stopped_feed_node_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_stopped_feed_nodes_request&,
                           const put_many_stopped_feed_nodes_request&) = default;
};

struct put_many_stopped_feed_nodes_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::stopped_feed_node> stopped_nodes;

    friend bool operator==(const put_many_stopped_feed_nodes_response&,
                           const put_many_stopped_feed_nodes_response&) = default;
};

struct delete_stopped_feed_node_request {
    using response_type = struct delete_stopped_feed_node_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stopped_feed_node_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_stopped_feed_node_request&,
                           const delete_stopped_feed_node_request&) = default;
};

struct delete_stopped_feed_node_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_stopped_feed_node_response&,
                           const delete_stopped_feed_node_response&) = default;
};

struct delete_many_stopped_feed_nodes_request {
    using response_type = struct delete_many_stopped_feed_nodes_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stopped_feed_node_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_stopped_feed_nodes_request&,
                           const delete_many_stopped_feed_nodes_request&) = default;
};

struct delete_many_stopped_feed_nodes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_stopped_feed_nodes_response&,
                           const delete_many_stopped_feed_nodes_response&) = default;
};

struct list_stopped_feed_node_versions_request {
    using response_type = struct list_stopped_feed_node_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.stopped_feed_nodes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stopped_feed_node_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<stopped_feed_node_versions_filter> filter;

    friend bool operator==(const list_stopped_feed_node_versions_request&,
                           const list_stopped_feed_node_versions_request&) = default;
};

struct list_stopped_feed_node_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::stopped_feed_node> versions;
    std::uint64_t total{};

    friend bool operator==(const list_stopped_feed_node_versions_response&,
                           const list_stopped_feed_node_versions_response&) = default;
};

struct get_stopped_feed_node_version_request {
    using response_type = struct get_stopped_feed_node_version_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.stopped_feed_nodes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stopped_feed_node_version_key key;

    friend bool operator==(const get_stopped_feed_node_version_request&,
                           const get_stopped_feed_node_version_request&) = default;
};

struct get_stopped_feed_node_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::stopped_feed_node> version;

    friend bool operator==(const get_stopped_feed_node_version_response&,
                           const get_stopped_feed_node_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace stopped_feed_node_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.events.stopped_feed_nodes.created";
inline constexpr std::string_view updated = "synthetic.v1.events.stopped_feed_nodes.updated";
inline constexpr std::string_view deleted = "synthetic.v1.events.stopped_feed_nodes.deleted";
}
/**
 * @brief The members of stopped_feed_node_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"node_name", &stopped_feed_node_key::node_name},
    };
}

/**
 * @brief The members of stopped_feed_node_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"node_name", &stopped_feed_node_write::node_name},
        utility::codec::field{"kind", &stopped_feed_node_write::kind},
        utility::codec::field{"provider_code", &stopped_feed_node_write::provider_code},
    };
}

/**
 * @brief The members of stopped_feed_node_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &stopped_feed_node_change::write},
        utility::codec::field{"precondition", &stopped_feed_node_change::precondition},
    };
}

/**
 * @brief The members of stopped_feed_node_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &stopped_feed_node_removal::key},
        utility::codec::field{"precondition", &stopped_feed_node_removal::precondition},
    };
}

/**
 * @brief The members of stopped_feed_node_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &stopped_feed_node_lookup::key},
        utility::codec::field{"stopped_feed_node", &stopped_feed_node_lookup::stopped_feed_node},
    };
}

/**
 * @brief The members of stopped_feed_nodes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_nodes_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"node_name_one_of", &stopped_feed_nodes_filter::node_name_one_of},
    };
}

/**
 * @brief The members of stopped_feed_node_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &stopped_feed_node_event::event_id},
        utility::codec::field{"key", &stopped_feed_node_event::key},
        utility::codec::field{"action", &stopped_feed_node_event::action},
        utility::codec::field{"version", &stopped_feed_node_event::version},
        utility::codec::field{"occurred_at", &stopped_feed_node_event::occurred_at},
        utility::codec::field{"correlation_id", &stopped_feed_node_event::correlation_id},
    };
}

/**
 * @brief The members of stopped_feed_node_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"stopped_feed_node",
                              &stopped_feed_node_version_key::stopped_feed_node},
        utility::codec::field{"version", &stopped_feed_node_version_key::version},
    };
}

/**
 * @brief The members of stopped_feed_node_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stopped_feed_node_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &stopped_feed_node_versions_filter::version},
        utility::codec::field{"from_version", &stopped_feed_node_versions_filter::from_version},
        utility::codec::field{"to_version", &stopped_feed_node_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_stopped_feed_nodes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stopped_feed_nodes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_stopped_feed_nodes_request::offset},
        utility::codec::field{"limit", &list_stopped_feed_nodes_request::limit},
        utility::codec::field{"order", &list_stopped_feed_nodes_request::order},
        utility::codec::field{"filter", &list_stopped_feed_nodes_request::filter},
        utility::codec::field{"as_of", &list_stopped_feed_nodes_request::as_of},
    };
}

/**
 * @brief The members of list_stopped_feed_nodes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_stopped_feed_nodes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_stopped_feed_nodes_response::result},
        utility::codec::field{"stopped_nodes", &list_stopped_feed_nodes_response::stopped_nodes},
        utility::codec::field{"total", &list_stopped_feed_nodes_response::total},
    };
}

/**
 * @brief The members of get_stopped_feed_node_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stopped_feed_node_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_stopped_feed_node_request::key},
    };
}

/**
 * @brief The members of get_stopped_feed_node_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_stopped_feed_node_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_stopped_feed_node_response::result},
        utility::codec::field{"stopped_feed_node",
                              &get_stopped_feed_node_response::stopped_feed_node},
    };
}

/**
 * @brief The members of get_many_stopped_feed_nodes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_stopped_feed_nodes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_stopped_feed_nodes_request::keys},
    };
}

/**
 * @brief The members of get_many_stopped_feed_nodes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_stopped_feed_nodes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_stopped_feed_nodes_response::result},
        utility::codec::field{"entries", &get_many_stopped_feed_nodes_response::entries},
    };
}

/**
 * @brief The members of put_stopped_feed_node_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stopped_feed_node_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_stopped_feed_node_request::change},
        utility::codec::field{"intent", &put_stopped_feed_node_request::intent},
    };
}

/**
 * @brief The members of put_stopped_feed_node_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_stopped_feed_node_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_stopped_feed_node_response::result},
        utility::codec::field{"stopped_feed_node",
                              &put_stopped_feed_node_response::stopped_feed_node},
    };
}

/**
 * @brief The members of put_many_stopped_feed_nodes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_stopped_feed_nodes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_stopped_feed_nodes_request::changes},
        utility::codec::field{"intent", &put_many_stopped_feed_nodes_request::intent},
    };
}

/**
 * @brief The members of put_many_stopped_feed_nodes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_stopped_feed_nodes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_stopped_feed_nodes_response::result},
        utility::codec::field{"stopped_nodes",
                              &put_many_stopped_feed_nodes_response::stopped_nodes},
    };
}

/**
 * @brief The members of delete_stopped_feed_node_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stopped_feed_node_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_stopped_feed_node_request::removal},
        utility::codec::field{"intent", &delete_stopped_feed_node_request::intent},
    };
}

/**
 * @brief The members of delete_stopped_feed_node_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_stopped_feed_node_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_stopped_feed_node_response::result},
    };
}

/**
 * @brief The members of delete_many_stopped_feed_nodes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stopped_feed_nodes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_stopped_feed_nodes_request::removals},
        utility::codec::field{"intent", &delete_many_stopped_feed_nodes_request::intent},
    };
}

/**
 * @brief The members of delete_many_stopped_feed_nodes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_stopped_feed_nodes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_stopped_feed_nodes_response::result},
    };
}

/**
 * @brief The members of list_stopped_feed_node_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stopped_feed_node_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_stopped_feed_node_versions_request::key},
        utility::codec::field{"offset", &list_stopped_feed_node_versions_request::offset},
        utility::codec::field{"limit", &list_stopped_feed_node_versions_request::limit},
        utility::codec::field{"order", &list_stopped_feed_node_versions_request::order},
        utility::codec::field{"filter", &list_stopped_feed_node_versions_request::filter},
    };
}

/**
 * @brief The members of list_stopped_feed_node_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_stopped_feed_node_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_stopped_feed_node_versions_response::result},
        utility::codec::field{"versions", &list_stopped_feed_node_versions_response::versions},
        utility::codec::field{"total", &list_stopped_feed_node_versions_response::total},
    };
}

/**
 * @brief The members of get_stopped_feed_node_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stopped_feed_node_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_stopped_feed_node_version_request::key},
    };
}

/**
 * @brief The members of get_stopped_feed_node_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_stopped_feed_node_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_stopped_feed_node_version_response::result},
        utility::codec::field{"version", &get_stopped_feed_node_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stopped_feed_node_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_key& v);
    static ores::synthetic::messaging::stopped_feed_node_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stopped_feed_node_key& v);
    static ores::synthetic::messaging::stopped_feed_node_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stopped_feed_node_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_write& v);
    static ores::synthetic::messaging::stopped_feed_node_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stopped_feed_node_write& v);
    static ores::synthetic::messaging::stopped_feed_node_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stopped_feed_node_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_change& v);
    static ores::synthetic::messaging::stopped_feed_node_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stopped_feed_node_change& v);
    static ores::synthetic::messaging::stopped_feed_node_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stopped_feed_node_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_removal& v);
    static ores::synthetic::messaging::stopped_feed_node_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stopped_feed_node_removal& v);
    static ores::synthetic::messaging::stopped_feed_node_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stopped_feed_node_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_lookup& v);
    static ores::synthetic::messaging::stopped_feed_node_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stopped_feed_node_lookup& v);
    static ores::synthetic::messaging::stopped_feed_node_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stopped_feed_nodes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_nodes_filter& v);
    static ores::synthetic::messaging::stopped_feed_nodes_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stopped_feed_nodes_filter& v);
    static ores::synthetic::messaging::stopped_feed_nodes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::stopped_feed_node_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::stopped_feed_node_event& v);
    static ores::synthetic::messaging::stopped_feed_node_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::stopped_feed_node_event& v);
    static ores::synthetic::messaging::stopped_feed_node_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stopped_feed_node_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::stopped_feed_node_version_key& v);
    static ores::synthetic::messaging::stopped_feed_node_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stopped_feed_node_version_key& v);
    static ores::synthetic::messaging::stopped_feed_node_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::stopped_feed_node_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::stopped_feed_node_versions_filter& v);
    static ores::synthetic::messaging::stopped_feed_node_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::stopped_feed_node_versions_filter& v);
    static ores::synthetic::messaging::stopped_feed_node_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::list_stopped_feed_nodes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::list_stopped_feed_nodes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_stopped_feed_nodes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::list_stopped_feed_nodes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::list_stopped_feed_nodes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_stopped_feed_node_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_stopped_feed_node_request& v);
    static ores::synthetic::messaging::get_stopped_feed_node_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_stopped_feed_node_request& v);
    static ores::synthetic::messaging::get_stopped_feed_node_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_stopped_feed_node_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_stopped_feed_node_response& v);
    static ores::synthetic::messaging::get_stopped_feed_node_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_stopped_feed_node_response& v);
    static ores::synthetic::messaging::get_stopped_feed_node_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::get_many_stopped_feed_nodes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::get_many_stopped_feed_nodes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_stopped_feed_nodes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::get_many_stopped_feed_nodes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::get_many_stopped_feed_nodes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_stopped_feed_node_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_stopped_feed_node_request& v);
    static ores::synthetic::messaging::put_stopped_feed_node_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_stopped_feed_node_request& v);
    static ores::synthetic::messaging::put_stopped_feed_node_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_stopped_feed_node_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_stopped_feed_node_response& v);
    static ores::synthetic::messaging::put_stopped_feed_node_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_stopped_feed_node_response& v);
    static ores::synthetic::messaging::put_stopped_feed_node_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::put_many_stopped_feed_nodes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::put_many_stopped_feed_nodes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_stopped_feed_nodes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::put_many_stopped_feed_nodes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::put_many_stopped_feed_nodes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_stopped_feed_node_request& v);
    static ores::synthetic::messaging::delete_stopped_feed_node_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_stopped_feed_node_request& v);
    static ores::synthetic::messaging::delete_stopped_feed_node_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_stopped_feed_node_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_stopped_feed_node_response& v);
    static ores::synthetic::messaging::delete_stopped_feed_node_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_stopped_feed_node_response& v);
    static ores::synthetic::messaging::delete_stopped_feed_node_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::delete_many_stopped_feed_nodes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_stopped_feed_nodes_request& v);
    static ores::synthetic::messaging::delete_many_stopped_feed_nodes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_stopped_feed_nodes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::delete_many_stopped_feed_nodes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_stopped_feed_nodes_response& v);
    static ores::synthetic::messaging::delete_many_stopped_feed_nodes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_stopped_feed_node_versions_request& v);
    static ores::synthetic::messaging::list_stopped_feed_node_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_stopped_feed_node_versions_request& v);
    static ores::synthetic::messaging::list_stopped_feed_node_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_stopped_feed_node_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_stopped_feed_node_versions_response& v);
    static ores::synthetic::messaging::list_stopped_feed_node_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_stopped_feed_node_versions_response& v);
    static ores::synthetic::messaging::list_stopped_feed_node_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_stopped_feed_node_version_request& v);
    static ores::synthetic::messaging::get_stopped_feed_node_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_stopped_feed_node_version_request& v);
    static ores::synthetic::messaging::get_stopped_feed_node_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_stopped_feed_node_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_stopped_feed_node_version_response& v);
    static ores::synthetic::messaging::get_stopped_feed_node_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_stopped_feed_node_version_response& v);
    static ores::synthetic::messaging::get_stopped_feed_node_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
