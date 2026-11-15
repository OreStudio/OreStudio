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
#ifndef ORES_MARKETDATA_API_MESSAGING_FEED_SCOPE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FEED_SCOPE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/feed_scope_codec.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct feed_scope_key {
    std::string item;

    friend bool operator==(const feed_scope_key&, const feed_scope_key&) = default;
};

struct feed_scope_write {
    boost::uuids::uuid id;
    boost::uuids::uuid feed_id;
    std::string asset_class;
    std::string item;
    boost::uuids::uuid vendor_instrument_id;

    friend bool operator==(const feed_scope_write&, const feed_scope_write&) = default;
};

struct feed_scope_change {
    feed_scope_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const feed_scope_change&, const feed_scope_change&) = default;
};

struct feed_scope_removal {
    feed_scope_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const feed_scope_removal&, const feed_scope_removal&) = default;
};

struct feed_scope_lookup {
    feed_scope_key key;
    std::optional<ores::marketdata::domain::feed_scope> feed_scope;

    friend bool operator==(const feed_scope_lookup&, const feed_scope_lookup&) = default;
};

struct feed_scopes_filter {
    std::optional<boost::uuids::uuid> feed_id;
    std::optional<std::string> asset_class;
    std::optional<std::string> item;
    std::optional<boost::uuids::uuid> vendor_instrument_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> feed_id_one_of;
    std::optional<std::vector<std::string>> asset_class_one_of;
    std::optional<std::vector<std::string>> item_one_of;
    std::optional<std::vector<boost::uuids::uuid>> vendor_instrument_id_one_of;
    std::optional<std::string> search;

    friend bool operator==(const feed_scopes_filter&, const feed_scopes_filter&) = default;
};

struct feed_scope_event {
    boost::uuids::uuid event_id;
    feed_scope_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const feed_scope_event&, const feed_scope_event&) = default;
};

struct feed_scope_version_key {
    feed_scope_key feed_scope;
    std::uint32_t version{};

    friend bool operator==(const feed_scope_version_key&, const feed_scope_version_key&) = default;
};

struct feed_scope_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const feed_scope_versions_filter&,
                           const feed_scope_versions_filter&) = default;
};

struct list_feed_scopes_request {
    using response_type = struct list_feed_scopes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.list";
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
    std::optional<feed_scopes_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_feed_scopes_request&,
                           const list_feed_scopes_request&) = default;
};

struct list_feed_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_scope> scopes;
    std::uint64_t total{};

    friend bool operator==(const list_feed_scopes_response&,
                           const list_feed_scopes_response&) = default;
};

struct get_feed_scope_request {
    using response_type = struct get_feed_scope_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_scope_key key;

    friend bool operator==(const get_feed_scope_request&, const get_feed_scope_request&) = default;
};

struct get_feed_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_scope> feed_scope;

    friend bool operator==(const get_feed_scope_response&,
                           const get_feed_scope_response&) = default;
};

struct get_many_feed_scopes_request {
    using response_type = struct get_many_feed_scopes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_scope_key> keys;

    friend bool operator==(const get_many_feed_scopes_request&,
                           const get_many_feed_scopes_request&) = default;
};

struct get_many_feed_scopes_response {
    ores::utility::domain::result result;
    std::vector<feed_scope_lookup> entries;

    friend bool operator==(const get_many_feed_scopes_response&,
                           const get_many_feed_scopes_response&) = default;
};

struct put_feed_scope_request {
    using response_type = struct put_feed_scope_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_scope_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_feed_scope_request&, const put_feed_scope_request&) = default;
};

struct put_feed_scope_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_scope> feed_scope;

    friend bool operator==(const put_feed_scope_response&,
                           const put_feed_scope_response&) = default;
};

struct put_many_feed_scopes_request {
    using response_type = struct put_many_feed_scopes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_scope_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_feed_scopes_request&,
                           const put_many_feed_scopes_request&) = default;
};

struct put_many_feed_scopes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_scope> scopes;

    friend bool operator==(const put_many_feed_scopes_response&,
                           const put_many_feed_scopes_response&) = default;
};

struct delete_feed_scope_request {
    using response_type = struct delete_feed_scope_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_scope_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_feed_scope_request&,
                           const delete_feed_scope_request&) = default;
};

struct delete_feed_scope_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_feed_scope_response&,
                           const delete_feed_scope_response&) = default;
};

struct delete_many_feed_scopes_request {
    using response_type = struct delete_many_feed_scopes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<feed_scope_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_feed_scopes_request&,
                           const delete_many_feed_scopes_request&) = default;
};

struct delete_many_feed_scopes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_feed_scopes_response&,
                           const delete_many_feed_scopes_response&) = default;
};

struct list_feed_scope_versions_request {
    using response_type = struct list_feed_scope_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_scope_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<feed_scope_versions_filter> filter;

    friend bool operator==(const list_feed_scope_versions_request&,
                           const list_feed_scope_versions_request&) = default;
};

struct list_feed_scope_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::feed_scope> versions;
    std::uint64_t total{};

    friend bool operator==(const list_feed_scope_versions_response&,
                           const list_feed_scope_versions_response&) = default;
};

struct get_feed_scope_version_request {
    using response_type = struct get_feed_scope_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.feed_scopes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    feed_scope_version_key key;

    friend bool operator==(const get_feed_scope_version_request&,
                           const get_feed_scope_version_request&) = default;
};

struct get_feed_scope_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::feed_scope> version;

    friend bool operator==(const get_feed_scope_version_response&,
                           const get_feed_scope_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace feed_scope_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.feed_scopes.created";
inline constexpr std::string_view updated = "marketdata.v1.events.feed_scopes.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.feed_scopes.deleted";
}
/**
 * @brief The members of feed_scope_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"item", &feed_scope_key::item},
    };
}

/**
 * @brief The members of feed_scope_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &feed_scope_write::id},
        utility::codec::field{"feed_id", &feed_scope_write::feed_id},
        utility::codec::field{"asset_class", &feed_scope_write::asset_class},
        utility::codec::field{"item", &feed_scope_write::item},
        utility::codec::field{"vendor_instrument_id", &feed_scope_write::vendor_instrument_id},
    };
}

/**
 * @brief The members of feed_scope_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &feed_scope_change::write},
        utility::codec::field{"precondition", &feed_scope_change::precondition},
    };
}

/**
 * @brief The members of feed_scope_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &feed_scope_removal::key},
        utility::codec::field{"precondition", &feed_scope_removal::precondition},
    };
}

/**
 * @brief The members of feed_scope_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &feed_scope_lookup::key},
        utility::codec::field{"feed_scope", &feed_scope_lookup::feed_scope},
    };
}

/**
 * @brief The members of feed_scopes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scopes_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"feed_id", &feed_scopes_filter::feed_id},
        utility::codec::field{"asset_class", &feed_scopes_filter::asset_class},
        utility::codec::field{"item", &feed_scopes_filter::item},
        utility::codec::field{"vendor_instrument_id", &feed_scopes_filter::vendor_instrument_id},
        utility::codec::field{"id_one_of", &feed_scopes_filter::id_one_of},
        utility::codec::field{"feed_id_one_of", &feed_scopes_filter::feed_id_one_of},
        utility::codec::field{"asset_class_one_of", &feed_scopes_filter::asset_class_one_of},
        utility::codec::field{"item_one_of", &feed_scopes_filter::item_one_of},
        utility::codec::field{"vendor_instrument_id_one_of",
                              &feed_scopes_filter::vendor_instrument_id_one_of},
        utility::codec::field{"search", &feed_scopes_filter::search},
    };
}

/**
 * @brief The members of feed_scope_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &feed_scope_event::event_id},
        utility::codec::field{"key", &feed_scope_event::key},
        utility::codec::field{"action", &feed_scope_event::action},
        utility::codec::field{"version", &feed_scope_event::version},
        utility::codec::field{"occurred_at", &feed_scope_event::occurred_at},
        utility::codec::field{"correlation_id", &feed_scope_event::correlation_id},
    };
}

/**
 * @brief The members of feed_scope_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"feed_scope", &feed_scope_version_key::feed_scope},
        utility::codec::field{"version", &feed_scope_version_key::version},
    };
}

/**
 * @brief The members of feed_scope_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_scope_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &feed_scope_versions_filter::version},
        utility::codec::field{"from_version", &feed_scope_versions_filter::from_version},
        utility::codec::field{"to_version", &feed_scope_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_feed_scopes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_scopes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_feed_scopes_request::offset},
        utility::codec::field{"limit", &list_feed_scopes_request::limit},
        utility::codec::field{"order", &list_feed_scopes_request::order},
        utility::codec::field{"filter", &list_feed_scopes_request::filter},
        utility::codec::field{"as_of", &list_feed_scopes_request::as_of},
    };
}

/**
 * @brief The members of list_feed_scopes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_scopes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_feed_scopes_response::result},
        utility::codec::field{"scopes", &list_feed_scopes_response::scopes},
        utility::codec::field{"total", &list_feed_scopes_response::total},
    };
}

/**
 * @brief The members of get_feed_scope_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_feed_scope_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_feed_scope_request::key},
    };
}

/**
 * @brief The members of get_feed_scope_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_feed_scope_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_feed_scope_response::result},
        utility::codec::field{"feed_scope", &get_feed_scope_response::feed_scope},
    };
}

/**
 * @brief The members of get_many_feed_scopes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_feed_scopes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_feed_scopes_request::keys},
    };
}

/**
 * @brief The members of get_many_feed_scopes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_feed_scopes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_feed_scopes_response::result},
        utility::codec::field{"entries", &get_many_feed_scopes_response::entries},
    };
}

/**
 * @brief The members of put_feed_scope_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_feed_scope_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_feed_scope_request::change},
        utility::codec::field{"intent", &put_feed_scope_request::intent},
    };
}

/**
 * @brief The members of put_feed_scope_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_feed_scope_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_feed_scope_response::result},
        utility::codec::field{"feed_scope", &put_feed_scope_response::feed_scope},
    };
}

/**
 * @brief The members of put_many_feed_scopes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_feed_scopes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_feed_scopes_request::changes},
        utility::codec::field{"intent", &put_many_feed_scopes_request::intent},
    };
}

/**
 * @brief The members of put_many_feed_scopes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_feed_scopes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_feed_scopes_response::result},
        utility::codec::field{"scopes", &put_many_feed_scopes_response::scopes},
    };
}

/**
 * @brief The members of delete_feed_scope_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_feed_scope_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_feed_scope_request::removal},
        utility::codec::field{"intent", &delete_feed_scope_request::intent},
    };
}

/**
 * @brief The members of delete_feed_scope_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_feed_scope_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_feed_scope_response::result},
    };
}

/**
 * @brief The members of delete_many_feed_scopes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_feed_scopes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_feed_scopes_request::removals},
        utility::codec::field{"intent", &delete_many_feed_scopes_request::intent},
    };
}

/**
 * @brief The members of delete_many_feed_scopes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_feed_scopes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_feed_scopes_response::result},
    };
}

/**
 * @brief The members of list_feed_scope_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_scope_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_feed_scope_versions_request::key},
        utility::codec::field{"offset", &list_feed_scope_versions_request::offset},
        utility::codec::field{"limit", &list_feed_scope_versions_request::limit},
        utility::codec::field{"order", &list_feed_scope_versions_request::order},
        utility::codec::field{"filter", &list_feed_scope_versions_request::filter},
    };
}

/**
 * @brief The members of list_feed_scope_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_feed_scope_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_feed_scope_versions_response::result},
        utility::codec::field{"versions", &list_feed_scope_versions_response::versions},
        utility::codec::field{"total", &list_feed_scope_versions_response::total},
    };
}

/**
 * @brief The members of get_feed_scope_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_feed_scope_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_feed_scope_version_request::key},
    };
}

/**
 * @brief The members of get_feed_scope_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_feed_scope_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_feed_scope_version_response::result},
        utility::codec::field{"version", &get_feed_scope_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_key& v);
    static ores::marketdata::messaging::feed_scope_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_key& v);
    static ores::marketdata::messaging::feed_scope_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_write& v);
    static ores::marketdata::messaging::feed_scope_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_write& v);
    static ores::marketdata::messaging::feed_scope_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_change& v);
    static ores::marketdata::messaging::feed_scope_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_change& v);
    static ores::marketdata::messaging::feed_scope_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_removal& v);
    static ores::marketdata::messaging::feed_scope_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_removal& v);
    static ores::marketdata::messaging::feed_scope_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_lookup& v);
    static ores::marketdata::messaging::feed_scope_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_lookup& v);
    static ores::marketdata::messaging::feed_scope_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scopes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scopes_filter& v);
    static ores::marketdata::messaging::feed_scopes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scopes_filter& v);
    static ores::marketdata::messaging::feed_scopes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_event& v);
    static ores::marketdata::messaging::feed_scope_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_event& v);
    static ores::marketdata::messaging::feed_scope_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_scope_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_version_key& v);
    static ores::marketdata::messaging::feed_scope_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_scope_version_key& v);
    static ores::marketdata::messaging::feed_scope_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::feed_scope_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_scope_versions_filter& v);
    static ores::marketdata::messaging::feed_scope_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::feed_scope_versions_filter& v);
    static ores::marketdata::messaging::feed_scope_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_feed_scopes_request& v);
    static ores::marketdata::messaging::list_feed_scopes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_scopes_request& v);
    static ores::marketdata::messaging::list_feed_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::list_feed_scopes_response& v);
    static ores::marketdata::messaging::list_feed_scopes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_scopes_response& v);
    static ores::marketdata::messaging::list_feed_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_feed_scope_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_scope_request& v);
    static ores::marketdata::messaging::get_feed_scope_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_feed_scope_request& v);
    static ores::marketdata::messaging::get_feed_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_feed_scope_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_feed_scope_response& v);
    static ores::marketdata::messaging::get_feed_scope_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_feed_scope_response& v);
    static ores::marketdata::messaging::get_feed_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_feed_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_feed_scopes_request& v);
    static ores::marketdata::messaging::get_many_feed_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_feed_scopes_request& v);
    static ores::marketdata::messaging::get_many_feed_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_feed_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_feed_scopes_response& v);
    static ores::marketdata::messaging::get_many_feed_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_feed_scopes_response& v);
    static ores::marketdata::messaging::get_many_feed_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::put_feed_scope_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_feed_scope_request& v);
    static ores::marketdata::messaging::put_feed_scope_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_feed_scope_request& v);
    static ores::marketdata::messaging::put_feed_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_feed_scope_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::put_feed_scope_response& v);
    static ores::marketdata::messaging::put_feed_scope_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::put_feed_scope_response& v);
    static ores::marketdata::messaging::put_feed_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_feed_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_feed_scopes_request& v);
    static ores::marketdata::messaging::put_many_feed_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_feed_scopes_request& v);
    static ores::marketdata::messaging::put_many_feed_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_feed_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_feed_scopes_response& v);
    static ores::marketdata::messaging::put_many_feed_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_feed_scopes_response& v);
    static ores::marketdata::messaging::put_many_feed_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_feed_scope_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_feed_scope_request& v);
    static ores::marketdata::messaging::delete_feed_scope_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_feed_scope_request& v);
    static ores::marketdata::messaging::delete_feed_scope_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_feed_scope_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::delete_feed_scope_response& v);
    static ores::marketdata::messaging::delete_feed_scope_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_feed_scope_response& v);
    static ores::marketdata::messaging::delete_feed_scope_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_feed_scopes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_feed_scopes_request& v);
    static ores::marketdata::messaging::delete_many_feed_scopes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_feed_scopes_request& v);
    static ores::marketdata::messaging::delete_many_feed_scopes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_feed_scopes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_feed_scopes_response& v);
    static ores::marketdata::messaging::delete_many_feed_scopes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_feed_scopes_response& v);
    static ores::marketdata::messaging::delete_many_feed_scopes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_scope_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_feed_scope_versions_request& v);
    static ores::marketdata::messaging::list_feed_scope_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_scope_versions_request& v);
    static ores::marketdata::messaging::list_feed_scope_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_feed_scope_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_feed_scope_versions_response& v);
    static ores::marketdata::messaging::list_feed_scope_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_feed_scope_versions_response& v);
    static ores::marketdata::messaging::list_feed_scope_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_feed_scope_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_feed_scope_version_request& v);
    static ores::marketdata::messaging::get_feed_scope_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_feed_scope_version_request& v);
    static ores::marketdata::messaging::get_feed_scope_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_feed_scope_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_feed_scope_version_response& v);
    static ores::marketdata::messaging::get_feed_scope_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_feed_scope_version_response& v);
    static ores::marketdata::messaging::get_feed_scope_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
