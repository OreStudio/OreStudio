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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_LINK_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_LINK_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_link_codec.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct trade_link_key {
    boost::uuids::uuid from_trade_id;
    boost::uuids::uuid to_trade_id;
    std::string link_type;

    friend bool operator==(const trade_link_key&, const trade_link_key&) = default;
};

struct trade_link_write {
    boost::uuids::uuid from_trade_id;
    boost::uuids::uuid to_trade_id;
    std::string link_type;
    boost::uuids::uuid trade_activity_id;

    friend bool operator==(const trade_link_write&, const trade_link_write&) = default;
};

struct trade_link_change {
    trade_link_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const trade_link_change&, const trade_link_change&) = default;
};

struct trade_link_removal {
    trade_link_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const trade_link_removal&, const trade_link_removal&) = default;
};

struct trade_link_lookup {
    trade_link_key key;
    std::optional<ores::trading::domain::trade_link> trade_link;

    friend bool operator==(const trade_link_lookup&, const trade_link_lookup&) = default;
};

struct trade_links_filter {
    std::optional<boost::uuids::uuid> from_trade_id;
    std::optional<boost::uuids::uuid> to_trade_id;
    std::optional<std::vector<boost::uuids::uuid>> from_trade_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> to_trade_id_one_of;

    friend bool operator==(const trade_links_filter&, const trade_links_filter&) = default;
};

struct trade_link_event {
    boost::uuids::uuid event_id;
    trade_link_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const trade_link_event&, const trade_link_event&) = default;
};

struct trade_link_version_key {
    trade_link_key trade_link;
    std::uint32_t version{};

    friend bool operator==(const trade_link_version_key&, const trade_link_version_key&) = default;
};

struct trade_link_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const trade_link_versions_filter&,
                           const trade_link_versions_filter&) = default;
};

struct list_trade_links_request {
    using response_type = struct list_trade_links_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.list";
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
    std::optional<trade_links_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_trade_links_request&,
                           const list_trade_links_request&) = default;
};

struct list_trade_links_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_link> links;
    std::uint64_t total{};

    friend bool operator==(const list_trade_links_response&,
                           const list_trade_links_response&) = default;
};

struct get_trade_link_request {
    using response_type = struct get_trade_link_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_link_key key;

    friend bool operator==(const get_trade_link_request&, const get_trade_link_request&) = default;
};

struct get_trade_link_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_link> trade_link;

    friend bool operator==(const get_trade_link_response&,
                           const get_trade_link_response&) = default;
};

struct get_many_trade_links_request {
    using response_type = struct get_many_trade_links_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_link_key> keys;

    friend bool operator==(const get_many_trade_links_request&,
                           const get_many_trade_links_request&) = default;
};

struct get_many_trade_links_response {
    ores::utility::domain::result result;
    std::vector<trade_link_lookup> entries;

    friend bool operator==(const get_many_trade_links_response&,
                           const get_many_trade_links_response&) = default;
};

struct put_trade_link_request {
    using response_type = struct put_trade_link_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_link_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_trade_link_request&, const put_trade_link_request&) = default;
};

struct put_trade_link_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_link> trade_link;

    friend bool operator==(const put_trade_link_response&,
                           const put_trade_link_response&) = default;
};

struct put_many_trade_links_request {
    using response_type = struct put_many_trade_links_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_link_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_trade_links_request&,
                           const put_many_trade_links_request&) = default;
};

struct put_many_trade_links_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_link> links;

    friend bool operator==(const put_many_trade_links_response&,
                           const put_many_trade_links_response&) = default;
};

struct delete_trade_link_request {
    using response_type = struct delete_trade_link_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_link_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_trade_link_request&,
                           const delete_trade_link_request&) = default;
};

struct delete_trade_link_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_trade_link_response&,
                           const delete_trade_link_response&) = default;
};

struct delete_many_trade_links_request {
    using response_type = struct delete_many_trade_links_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_link_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_trade_links_request&,
                           const delete_many_trade_links_request&) = default;
};

struct delete_many_trade_links_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_trade_links_response&,
                           const delete_many_trade_links_response&) = default;
};

struct list_trade_link_versions_request {
    using response_type = struct list_trade_link_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_link_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_link_versions_filter> filter;

    friend bool operator==(const list_trade_link_versions_request&,
                           const list_trade_link_versions_request&) = default;
};

struct list_trade_link_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_link> versions;
    std::uint64_t total{};

    friend bool operator==(const list_trade_link_versions_response&,
                           const list_trade_link_versions_response&) = default;
};

struct get_trade_link_version_request {
    using response_type = struct get_trade_link_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_links_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_link_version_key key;

    friend bool operator==(const get_trade_link_version_request&,
                           const get_trade_link_version_request&) = default;
};

struct get_trade_link_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_link> version;

    friend bool operator==(const get_trade_link_version_response&,
                           const get_trade_link_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_link_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.trade_links.created";
inline constexpr std::string_view updated = "trading.v1.events.trade_links.updated";
inline constexpr std::string_view deleted = "trading.v1.events.trade_links.deleted";
}
/**
 * @brief The members of trade_link_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"from_trade_id", &trade_link_key::from_trade_id},
        utility::codec::field{"to_trade_id", &trade_link_key::to_trade_id},
        utility::codec::field{"link_type", &trade_link_key::link_type},
    };
}

/**
 * @brief The members of trade_link_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"from_trade_id", &trade_link_write::from_trade_id},
        utility::codec::field{"to_trade_id", &trade_link_write::to_trade_id},
        utility::codec::field{"link_type", &trade_link_write::link_type},
        utility::codec::field{"trade_activity_id", &trade_link_write::trade_activity_id},
    };
}

/**
 * @brief The members of trade_link_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &trade_link_change::write},
        utility::codec::field{"precondition", &trade_link_change::precondition},
    };
}

/**
 * @brief The members of trade_link_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &trade_link_removal::key},
        utility::codec::field{"precondition", &trade_link_removal::precondition},
    };
}

/**
 * @brief The members of trade_link_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &trade_link_lookup::key},
        utility::codec::field{"trade_link", &trade_link_lookup::trade_link},
    };
}

/**
 * @brief The members of trade_links_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_links_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"from_trade_id", &trade_links_filter::from_trade_id},
        utility::codec::field{"to_trade_id", &trade_links_filter::to_trade_id},
        utility::codec::field{"from_trade_id_one_of", &trade_links_filter::from_trade_id_one_of},
        utility::codec::field{"to_trade_id_one_of", &trade_links_filter::to_trade_id_one_of},
    };
}

/**
 * @brief The members of trade_link_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &trade_link_event::event_id},
        utility::codec::field{"key", &trade_link_event::key},
        utility::codec::field{"action", &trade_link_event::action},
        utility::codec::field{"version", &trade_link_event::version},
        utility::codec::field{"occurred_at", &trade_link_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_link_event::correlation_id},
    };
}

/**
 * @brief The members of trade_link_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_link", &trade_link_version_key::trade_link},
        utility::codec::field{"version", &trade_link_version_key::version},
    };
}

/**
 * @brief The members of trade_link_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_link_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &trade_link_versions_filter::version},
        utility::codec::field{"from_version", &trade_link_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_link_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_links_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_trade_links_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_trade_links_request::offset},
        utility::codec::field{"limit", &list_trade_links_request::limit},
        utility::codec::field{"order", &list_trade_links_request::order},
        utility::codec::field{"filter", &list_trade_links_request::filter},
        utility::codec::field{"as_of", &list_trade_links_request::as_of},
    };
}

/**
 * @brief The members of list_trade_links_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_trade_links_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_trade_links_response::result},
        utility::codec::field{"links", &list_trade_links_response::links},
        utility::codec::field{"total", &list_trade_links_response::total},
    };
}

/**
 * @brief The members of get_trade_link_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_link_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_trade_link_request::key},
    };
}

/**
 * @brief The members of get_trade_link_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_link_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_trade_link_response::result},
        utility::codec::field{"trade_link", &get_trade_link_response::trade_link},
    };
}

/**
 * @brief The members of get_many_trade_links_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_trade_links_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_trade_links_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_links_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_trade_links_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_trade_links_response::result},
        utility::codec::field{"entries", &get_many_trade_links_response::entries},
    };
}

/**
 * @brief The members of put_trade_link_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_trade_link_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_trade_link_request::change},
        utility::codec::field{"intent", &put_trade_link_request::intent},
    };
}

/**
 * @brief The members of put_trade_link_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_trade_link_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_trade_link_response::result},
        utility::codec::field{"trade_link", &put_trade_link_response::trade_link},
    };
}

/**
 * @brief The members of put_many_trade_links_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_trade_links_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_trade_links_request::changes},
        utility::codec::field{"intent", &put_many_trade_links_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_links_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_trade_links_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_trade_links_response::result},
        utility::codec::field{"links", &put_many_trade_links_response::links},
    };
}

/**
 * @brief The members of delete_trade_link_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_trade_link_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_trade_link_request::removal},
        utility::codec::field{"intent", &delete_trade_link_request::intent},
    };
}

/**
 * @brief The members of delete_trade_link_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_trade_link_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_trade_link_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_links_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_trade_links_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_trade_links_request::removals},
        utility::codec::field{"intent", &delete_many_trade_links_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_links_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_trade_links_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_trade_links_response::result},
    };
}

/**
 * @brief The members of list_trade_link_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_trade_link_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_trade_link_versions_request::key},
        utility::codec::field{"offset", &list_trade_link_versions_request::offset},
        utility::codec::field{"limit", &list_trade_link_versions_request::limit},
        utility::codec::field{"order", &list_trade_link_versions_request::order},
        utility::codec::field{"filter", &list_trade_link_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_link_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_trade_link_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_trade_link_versions_response::result},
        utility::codec::field{"versions", &list_trade_link_versions_response::versions},
        utility::codec::field{"total", &list_trade_link_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_link_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_link_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_trade_link_version_request::key},
    };
}

/**
 * @brief The members of get_trade_link_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_trade_link_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_trade_link_version_response::result},
        utility::codec::field{"version", &get_trade_link_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_key& v);
    static ores::trading::messaging::trade_link_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_key& v);
    static ores::trading::messaging::trade_link_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_write& v);
    static ores::trading::messaging::trade_link_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_write& v);
    static ores::trading::messaging::trade_link_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_change& v);
    static ores::trading::messaging::trade_link_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_change& v);
    static ores::trading::messaging::trade_link_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_removal& v);
    static ores::trading::messaging::trade_link_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_removal& v);
    static ores::trading::messaging::trade_link_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_lookup& v);
    static ores::trading::messaging::trade_link_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_lookup& v);
    static ores::trading::messaging::trade_link_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_links_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_links_filter& v);
    static ores::trading::messaging::trade_links_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_links_filter& v);
    static ores::trading::messaging::trade_links_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_event& v);
    static ores::trading::messaging::trade_link_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_event& v);
    static ores::trading::messaging::trade_link_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_version_key& v);
    static ores::trading::messaging::trade_link_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_version_key& v);
    static ores::trading::messaging::trade_link_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::trade_link_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::trade_link_versions_filter& v);
    static ores::trading::messaging::trade_link_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::trade_link_versions_filter& v);
    static ores::trading::messaging::trade_link_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_links_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_links_request& v);
    static ores::trading::messaging::list_trade_links_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_trade_links_request& v);
    static ores::trading::messaging::list_trade_links_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_trade_links_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_trade_links_response& v);
    static ores::trading::messaging::list_trade_links_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_trade_links_response& v);
    static ores::trading::messaging::list_trade_links_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_link_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_link_request& v);
    static ores::trading::messaging::get_trade_link_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_link_request& v);
    static ores::trading::messaging::get_trade_link_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_trade_link_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_trade_link_response& v);
    static ores::trading::messaging::get_trade_link_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_trade_link_response& v);
    static ores::trading::messaging::get_trade_link_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_trade_links_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_trade_links_request& v);
    static ores::trading::messaging::get_many_trade_links_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_links_request& v);
    static ores::trading::messaging::get_many_trade_links_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_links_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_trade_links_response& v);
    static ores::trading::messaging::get_many_trade_links_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_links_response& v);
    static ores::trading::messaging::get_many_trade_links_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_link_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_link_request& v);
    static ores::trading::messaging::put_trade_link_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_link_request& v);
    static ores::trading::messaging::put_trade_link_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_trade_link_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_trade_link_response& v);
    static ores::trading::messaging::put_trade_link_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_trade_link_response& v);
    static ores::trading::messaging::put_trade_link_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_trade_links_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_trade_links_request& v);
    static ores::trading::messaging::put_many_trade_links_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_links_request& v);
    static ores::trading::messaging::put_many_trade_links_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_links_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_trade_links_response& v);
    static ores::trading::messaging::put_many_trade_links_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_links_response& v);
    static ores::trading::messaging::put_many_trade_links_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_trade_link_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_link_request& v);
    static ores::trading::messaging::delete_trade_link_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_trade_link_request& v);
    static ores::trading::messaging::delete_trade_link_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_trade_link_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_trade_link_response& v);
    static ores::trading::messaging::delete_trade_link_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_trade_link_response& v);
    static ores::trading::messaging::delete_trade_link_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_links_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_links_request& v);
    static ores::trading::messaging::delete_many_trade_links_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_links_request& v);
    static ores::trading::messaging::delete_many_trade_links_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_links_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_links_response& v);
    static ores::trading::messaging::delete_many_trade_links_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_trade_links_response& v);
    static ores::trading::messaging::delete_many_trade_links_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_link_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_link_versions_request& v);
    static ores::trading::messaging::list_trade_link_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_link_versions_request& v);
    static ores::trading::messaging::list_trade_link_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_link_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_link_versions_response& v);
    static ores::trading::messaging::list_trade_link_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_link_versions_response& v);
    static ores::trading::messaging::list_trade_link_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_link_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_link_version_request& v);
    static ores::trading::messaging::get_trade_link_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_link_version_request& v);
    static ores::trading::messaging::get_trade_link_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_link_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_link_version_response& v);
    static ores::trading::messaging::get_trade_link_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_link_version_response& v);
    static ores::trading::messaging::get_trade_link_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
