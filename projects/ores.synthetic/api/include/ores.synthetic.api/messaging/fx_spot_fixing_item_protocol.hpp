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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_FIXING_ITEM_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_FIXING_ITEM_PROTOCOL_HPP

#include "ores.synthetic.api/domain/fx_spot_fixing_item_codec.hpp"
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

struct fx_spot_fixing_item_key {
    boost::uuids::uuid id;

    friend bool operator==(const fx_spot_fixing_item_key&,
                           const fx_spot_fixing_item_key&) = default;
};

struct fx_spot_fixing_item_write {
    boost::uuids::uuid id;
    boost::uuids::uuid fx_spot_config_id;
    int rank{};
    std::string item;
    std::string provider_code;
    std::string policy;
    int window_days{};

    friend bool operator==(const fx_spot_fixing_item_write&,
                           const fx_spot_fixing_item_write&) = default;
};

struct fx_spot_fixing_item_change {
    fx_spot_fixing_item_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const fx_spot_fixing_item_change&,
                           const fx_spot_fixing_item_change&) = default;
};

struct fx_spot_fixing_item_removal {
    fx_spot_fixing_item_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const fx_spot_fixing_item_removal&,
                           const fx_spot_fixing_item_removal&) = default;
};

struct fx_spot_fixing_item_lookup {
    fx_spot_fixing_item_key key;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;

    friend bool operator==(const fx_spot_fixing_item_lookup&,
                           const fx_spot_fixing_item_lookup&) = default;
};

struct fx_spot_fixing_items_filter {
    std::optional<boost::uuids::uuid> fx_spot_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> fx_spot_config_id_one_of;

    friend bool operator==(const fx_spot_fixing_items_filter&,
                           const fx_spot_fixing_items_filter&) = default;
};

struct fx_spot_fixing_item_event {
    boost::uuids::uuid event_id;
    fx_spot_fixing_item_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fx_spot_fixing_item_event&,
                           const fx_spot_fixing_item_event&) = default;
};

struct fx_spot_fixing_item_version_key {
    fx_spot_fixing_item_key fx_spot_fixing_item;
    std::uint32_t version{};

    friend bool operator==(const fx_spot_fixing_item_version_key&,
                           const fx_spot_fixing_item_version_key&) = default;
};

struct fx_spot_fixing_item_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fx_spot_fixing_item_versions_filter&,
                           const fx_spot_fixing_item_versions_filter&) = default;
};

struct list_fx_spot_fixing_items_request {
    using response_type = struct list_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.list";
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
    std::optional<fx_spot_fixing_items_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fx_spot_fixing_items_request&,
                           const list_fx_spot_fixing_items_request&) = default;
};

struct list_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_items;
    std::uint64_t total{};

    friend bool operator==(const list_fx_spot_fixing_items_response&,
                           const list_fx_spot_fixing_items_response&) = default;
};

struct get_fx_spot_fixing_item_request {
    using response_type = struct get_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_key key;

    friend bool operator==(const get_fx_spot_fixing_item_request&,
                           const get_fx_spot_fixing_item_request&) = default;
};

struct get_fx_spot_fixing_item_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;

    friend bool operator==(const get_fx_spot_fixing_item_response&,
                           const get_fx_spot_fixing_item_response&) = default;
};

struct get_many_fx_spot_fixing_items_request {
    using response_type = struct get_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_key> keys;

    friend bool operator==(const get_many_fx_spot_fixing_items_request&,
                           const get_many_fx_spot_fixing_items_request&) = default;
};

struct get_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<fx_spot_fixing_item_lookup> entries;

    friend bool operator==(const get_many_fx_spot_fixing_items_response&,
                           const get_many_fx_spot_fixing_items_response&) = default;
};

struct put_fx_spot_fixing_item_request {
    using response_type = struct put_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_fx_spot_fixing_item_request&,
                           const put_fx_spot_fixing_item_request&) = default;
};

struct put_fx_spot_fixing_item_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_item;

    friend bool operator==(const put_fx_spot_fixing_item_response&,
                           const put_fx_spot_fixing_item_response&) = default;
};

struct put_many_fx_spot_fixing_items_request {
    using response_type = struct put_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_fx_spot_fixing_items_request&,
                           const put_many_fx_spot_fixing_items_request&) = default;
};

struct put_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> fx_spot_fixing_items;

    friend bool operator==(const put_many_fx_spot_fixing_items_response&,
                           const put_many_fx_spot_fixing_items_response&) = default;
};

struct delete_fx_spot_fixing_item_request {
    using response_type = struct delete_fx_spot_fixing_item_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_fixing_items.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_fx_spot_fixing_item_request&,
                           const delete_fx_spot_fixing_item_request&) = default;
};

struct delete_fx_spot_fixing_item_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_fx_spot_fixing_item_response&,
                           const delete_fx_spot_fixing_item_response&) = default;
};

struct delete_many_fx_spot_fixing_items_request {
    using response_type = struct delete_many_fx_spot_fixing_items_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_fixing_item_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_fx_spot_fixing_items_request&,
                           const delete_many_fx_spot_fixing_items_request&) = default;
};

struct delete_many_fx_spot_fixing_items_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_fx_spot_fixing_items_response&,
                           const delete_many_fx_spot_fixing_items_response&) = default;
};

struct list_fx_spot_fixing_item_versions_request {
    using response_type = struct list_fx_spot_fixing_item_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_spot_fixing_item_versions_filter> filter;

    friend bool operator==(const list_fx_spot_fixing_item_versions_request&,
                           const list_fx_spot_fixing_item_versions_request&) = default;
};

struct list_fx_spot_fixing_item_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_fixing_item> versions;
    std::uint64_t total{};

    friend bool operator==(const list_fx_spot_fixing_item_versions_response&,
                           const list_fx_spot_fixing_item_versions_response&) = default;
};

struct get_fx_spot_fixing_item_version_request {
    using response_type = struct get_fx_spot_fixing_item_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_fixing_items_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_fixing_item_version_key key;

    friend bool operator==(const get_fx_spot_fixing_item_version_request&,
                           const get_fx_spot_fixing_item_version_request&) = default;
};

struct get_fx_spot_fixing_item_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_fixing_item> version;

    friend bool operator==(const get_fx_spot_fixing_item_version_response&,
                           const get_fx_spot_fixing_item_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_spot_fixing_item_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.events.fx_spot_fixing_items.created";
inline constexpr std::string_view updated = "synthetic.v1.events.fx_spot_fixing_items.updated";
inline constexpr std::string_view deleted = "synthetic.v1.events.fx_spot_fixing_items.deleted";
}
/**
 * @brief The members of fx_spot_fixing_item_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &fx_spot_fixing_item_key::id},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &fx_spot_fixing_item_write::id},
        utility::codec::field{"fx_spot_config_id", &fx_spot_fixing_item_write::fx_spot_config_id},
        utility::codec::field{"rank", &fx_spot_fixing_item_write::rank},
        utility::codec::field{"item", &fx_spot_fixing_item_write::item},
        utility::codec::field{"provider_code", &fx_spot_fixing_item_write::provider_code},
        utility::codec::field{"policy", &fx_spot_fixing_item_write::policy},
        utility::codec::field{"window_days", &fx_spot_fixing_item_write::window_days},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &fx_spot_fixing_item_change::write},
        utility::codec::field{"precondition", &fx_spot_fixing_item_change::precondition},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fx_spot_fixing_item_removal::key},
        utility::codec::field{"precondition", &fx_spot_fixing_item_removal::precondition},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fx_spot_fixing_item_lookup::key},
        utility::codec::field{"fx_spot_fixing_item",
                              &fx_spot_fixing_item_lookup::fx_spot_fixing_item},
    };
}

/**
 * @brief The members of fx_spot_fixing_items_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_items_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"fx_spot_config_id", &fx_spot_fixing_items_filter::fx_spot_config_id},
        utility::codec::field{"id_one_of", &fx_spot_fixing_items_filter::id_one_of},
        utility::codec::field{"fx_spot_config_id_one_of",
                              &fx_spot_fixing_items_filter::fx_spot_config_id_one_of},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &fx_spot_fixing_item_event::event_id},
        utility::codec::field{"key", &fx_spot_fixing_item_event::key},
        utility::codec::field{"action", &fx_spot_fixing_item_event::action},
        utility::codec::field{"version", &fx_spot_fixing_item_event::version},
        utility::codec::field{"occurred_at", &fx_spot_fixing_item_event::occurred_at},
        utility::codec::field{"correlation_id", &fx_spot_fixing_item_event::correlation_id},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"fx_spot_fixing_item",
                              &fx_spot_fixing_item_version_key::fx_spot_fixing_item},
        utility::codec::field{"version", &fx_spot_fixing_item_version_key::version},
    };
}

/**
 * @brief The members of fx_spot_fixing_item_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_fixing_item_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &fx_spot_fixing_item_versions_filter::version},
        utility::codec::field{"from_version", &fx_spot_fixing_item_versions_filter::from_version},
        utility::codec::field{"to_version", &fx_spot_fixing_item_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fx_spot_fixing_items_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_spot_fixing_items_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_fx_spot_fixing_items_request::offset},
        utility::codec::field{"limit", &list_fx_spot_fixing_items_request::limit},
        utility::codec::field{"order", &list_fx_spot_fixing_items_request::order},
        utility::codec::field{"filter", &list_fx_spot_fixing_items_request::filter},
        utility::codec::field{"as_of", &list_fx_spot_fixing_items_request::as_of},
    };
}

/**
 * @brief The members of list_fx_spot_fixing_items_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_fx_spot_fixing_items_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_spot_fixing_items_response::result},
        utility::codec::field{"fx_spot_fixing_items",
                              &list_fx_spot_fixing_items_response::fx_spot_fixing_items},
        utility::codec::field{"total", &list_fx_spot_fixing_items_response::total},
    };
}

/**
 * @brief The members of get_fx_spot_fixing_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_spot_fixing_item_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_spot_fixing_item_request::key},
    };
}

/**
 * @brief The members of get_fx_spot_fixing_item_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_spot_fixing_item_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_spot_fixing_item_response::result},
        utility::codec::field{"fx_spot_fixing_item",
                              &get_fx_spot_fixing_item_response::fx_spot_fixing_item},
    };
}

/**
 * @brief The members of get_many_fx_spot_fixing_items_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_spot_fixing_items_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_fx_spot_fixing_items_request::keys},
    };
}

/**
 * @brief The members of get_many_fx_spot_fixing_items_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_spot_fixing_items_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_fx_spot_fixing_items_response::result},
        utility::codec::field{"entries", &get_many_fx_spot_fixing_items_response::entries},
    };
}

/**
 * @brief The members of put_fx_spot_fixing_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fx_spot_fixing_item_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_fx_spot_fixing_item_request::change},
        utility::codec::field{"intent", &put_fx_spot_fixing_item_request::intent},
    };
}

/**
 * @brief The members of put_fx_spot_fixing_item_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_fx_spot_fixing_item_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_fx_spot_fixing_item_response::result},
        utility::codec::field{"fx_spot_fixing_item",
                              &put_fx_spot_fixing_item_response::fx_spot_fixing_item},
    };
}

/**
 * @brief The members of put_many_fx_spot_fixing_items_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_fx_spot_fixing_items_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_fx_spot_fixing_items_request::changes},
        utility::codec::field{"intent", &put_many_fx_spot_fixing_items_request::intent},
    };
}

/**
 * @brief The members of put_many_fx_spot_fixing_items_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_fx_spot_fixing_items_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_fx_spot_fixing_items_response::result},
        utility::codec::field{"fx_spot_fixing_items",
                              &put_many_fx_spot_fixing_items_response::fx_spot_fixing_items},
    };
}

/**
 * @brief The members of delete_fx_spot_fixing_item_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fx_spot_fixing_item_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_fx_spot_fixing_item_request::removal},
        utility::codec::field{"intent", &delete_fx_spot_fixing_item_request::intent},
    };
}

/**
 * @brief The members of delete_fx_spot_fixing_item_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_fx_spot_fixing_item_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_fx_spot_fixing_item_response::result},
    };
}

/**
 * @brief The members of delete_many_fx_spot_fixing_items_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_fx_spot_fixing_items_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_fx_spot_fixing_items_request::removals},
        utility::codec::field{"intent", &delete_many_fx_spot_fixing_items_request::intent},
    };
}

/**
 * @brief The members of delete_many_fx_spot_fixing_items_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_fx_spot_fixing_items_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_fx_spot_fixing_items_response::result},
    };
}

/**
 * @brief The members of list_fx_spot_fixing_item_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_fixing_item_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_fx_spot_fixing_item_versions_request::key},
        utility::codec::field{"offset", &list_fx_spot_fixing_item_versions_request::offset},
        utility::codec::field{"limit", &list_fx_spot_fixing_item_versions_request::limit},
        utility::codec::field{"order", &list_fx_spot_fixing_item_versions_request::order},
        utility::codec::field{"filter", &list_fx_spot_fixing_item_versions_request::filter},
    };
}

/**
 * @brief The members of list_fx_spot_fixing_item_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_fixing_item_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_spot_fixing_item_versions_response::result},
        utility::codec::field{"versions", &list_fx_spot_fixing_item_versions_response::versions},
        utility::codec::field{"total", &list_fx_spot_fixing_item_versions_response::total},
    };
}

/**
 * @brief The members of get_fx_spot_fixing_item_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_fixing_item_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_spot_fixing_item_version_request::key},
    };
}

/**
 * @brief The members of get_fx_spot_fixing_item_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_fixing_item_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_spot_fixing_item_version_response::result},
        utility::codec::field{"version", &get_fx_spot_fixing_item_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_key& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_key& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_write& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_write& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_change& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_change& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_removal& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_removal& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_lookup& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_lookup& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_items_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_items_filter& v);
    static ores::synthetic::messaging::fx_spot_fixing_items_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_items_filter& v);
    static ores::synthetic::messaging::fx_spot_fixing_items_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::fx_spot_fixing_item_event& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_event& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_fixing_item_version_key& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_version_key& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_fixing_item_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_fixing_item_versions_filter& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_fixing_item_versions_filter& v);
    static ores::synthetic::messaging::fx_spot_fixing_item_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_fixing_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_fixing_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_fixing_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_fixing_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_spot_fixing_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::get_many_fx_spot_fixing_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::get_many_fx_spot_fixing_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_spot_fixing_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::get_many_fx_spot_fixing_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::get_many_fx_spot_fixing_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_fx_spot_fixing_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::put_fx_spot_fixing_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::put_fx_spot_fixing_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_fx_spot_fixing_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::put_fx_spot_fixing_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::put_fx_spot_fixing_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_spot_fixing_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::put_many_fx_spot_fixing_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::put_many_fx_spot_fixing_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_spot_fixing_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::put_many_fx_spot_fixing_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::put_many_fx_spot_fixing_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_spot_fixing_item_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::delete_fx_spot_fixing_item_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_spot_fixing_item_request& v);
    static ores::synthetic::messaging::delete_fx_spot_fixing_item_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_spot_fixing_item_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::delete_fx_spot_fixing_item_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_spot_fixing_item_response& v);
    static ores::synthetic::messaging::delete_fx_spot_fixing_item_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_spot_fixing_items_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::delete_many_fx_spot_fixing_items_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_fx_spot_fixing_items_request& v);
    static ores::synthetic::messaging::delete_many_fx_spot_fixing_items_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_spot_fixing_items_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::delete_many_fx_spot_fixing_items_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_fx_spot_fixing_items_response& v);
    static ores::synthetic::messaging::delete_many_fx_spot_fixing_items_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_fixing_item_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_fixing_item_versions_request& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_item_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_fixing_item_versions_request& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_item_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_fixing_item_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_fixing_item_versions_response& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_item_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_fixing_item_versions_response& v);
    static ores::synthetic::messaging::list_fx_spot_fixing_item_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_fixing_item_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_fixing_item_version_request& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_fixing_item_version_request& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_fixing_item_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_fixing_item_version_response& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_fixing_item_version_response& v);
    static ores::synthetic::messaging::get_fx_spot_fixing_item_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
