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
#ifndef ORES_TRADING_API_MESSAGING_COMPOSITE_LEG_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_COMPOSITE_LEG_PROTOCOL_HPP

#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct composite_leg_key {
    boost::uuids::uuid id;
};

struct composite_leg_write {
    boost::uuids::uuid id;
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    int leg_sequence;
    std::string constituent_trade_id;
};

struct composite_leg_change {
    composite_leg_write write;
    ores::utility::domain::precondition precondition;
};

struct composite_leg_removal {
    composite_leg_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct composite_leg_lookup {
    composite_leg_key key;
    std::optional<ores::trading::domain::composite_leg> composite_leg;
};

struct composite_legs_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct composite_leg_event {
    boost::uuids::uuid event_id;
    composite_leg_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct composite_leg_version_key {
    composite_leg_key composite_leg;
    std::uint32_t version;
};

struct composite_leg_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_composite_legs_request {
    using response_type = struct list_composite_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.list";
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
    std::optional<composite_legs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_composite_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_leg> composite_legs;
    std::uint64_t total;
};

struct get_composite_leg_request {
    using response_type = struct get_composite_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_leg_key key;
};

struct get_composite_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::composite_leg> composite_leg;
};

struct get_many_composite_legs_request {
    using response_type = struct get_many_composite_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_leg_key> keys;
};

struct get_many_composite_legs_response {
    ores::utility::domain::result result;
    std::vector<composite_leg_lookup> entries;
};

struct put_composite_leg_request {
    using response_type = struct put_composite_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_leg_change change;
    ores::utility::domain::change_intent intent;
};

struct put_composite_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::composite_leg> composite_leg;
};

struct put_many_composite_legs_request {
    using response_type = struct put_many_composite_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_leg_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_composite_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_leg> composite_legs;
};

struct delete_composite_leg_request {
    using response_type = struct delete_composite_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_leg_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_composite_leg_response {
    ores::utility::domain::result result;
};

struct delete_many_composite_legs_request {
    using response_type = struct delete_many_composite_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_leg_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_composite_legs_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_composite_legs_request {
    using response_type = struct list_by_trade_id_composite_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs.list_by_trade_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid trade_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_legs_filter> filter;
};

struct list_by_trade_id_composite_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_leg> composite_legs;
    std::uint64_t total;
};

struct list_composite_leg_versions_request {
    using response_type = struct list_composite_leg_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_leg_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_leg_versions_filter> filter;
};

struct list_composite_leg_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::composite_leg> versions;
    std::uint64_t total;
};

struct get_composite_leg_version_request {
    using response_type = struct get_composite_leg_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.composite_legs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_leg_version_key key;
};

struct get_composite_leg_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::composite_leg> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace composite_leg_event_subjects {
inline constexpr std::string_view created = "trading.v1.composite_legs_events.created";
inline constexpr std::string_view updated = "trading.v1.composite_legs_events.updated";
inline constexpr std::string_view deleted = "trading.v1.composite_legs_events.deleted";
}
/**
 * @brief The members of composite_leg_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_key&) {
    return std::tuple{
        utility::codec::field{"id", &composite_leg_key::id},
    };
}

/**
 * @brief The members of composite_leg_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_write&) {
    return std::tuple{
        utility::codec::field{"id", &composite_leg_write::id},
        utility::codec::field{"trade_id", &composite_leg_write::trade_id},
        utility::codec::field{"trade_activity_id", &composite_leg_write::trade_activity_id},
        utility::codec::field{"leg_sequence", &composite_leg_write::leg_sequence},
        utility::codec::field{"constituent_trade_id", &composite_leg_write::constituent_trade_id},
    };
}

/**
 * @brief The members of composite_leg_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_change&) {
    return std::tuple{
        utility::codec::field{"write", &composite_leg_change::write},
        utility::codec::field{"precondition", &composite_leg_change::precondition},
    };
}

/**
 * @brief The members of composite_leg_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_removal&) {
    return std::tuple{
        utility::codec::field{"key", &composite_leg_removal::key},
        utility::codec::field{"precondition", &composite_leg_removal::precondition},
    };
}

/**
 * @brief The members of composite_leg_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &composite_leg_lookup::key},
        utility::codec::field{"composite_leg", &composite_leg_lookup::composite_leg},
    };
}

/**
 * @brief The members of composite_legs_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_legs_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &composite_legs_filter::trade_id},
        utility::codec::field{"id_one_of", &composite_legs_filter::id_one_of},
        utility::codec::field{"trade_id_one_of", &composite_legs_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of composite_leg_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &composite_leg_event::event_id},
        utility::codec::field{"key", &composite_leg_event::key},
        utility::codec::field{"action", &composite_leg_event::action},
        utility::codec::field{"version", &composite_leg_event::version},
        utility::codec::field{"occurred_at", &composite_leg_event::occurred_at},
        utility::codec::field{"correlation_id", &composite_leg_event::correlation_id},
    };
}

/**
 * @brief The members of composite_leg_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_version_key&) {
    return std::tuple{
        utility::codec::field{"composite_leg", &composite_leg_version_key::composite_leg},
        utility::codec::field{"version", &composite_leg_version_key::version},
    };
}

/**
 * @brief The members of composite_leg_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const composite_leg_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &composite_leg_versions_filter::version},
        utility::codec::field{"from_version", &composite_leg_versions_filter::from_version},
        utility::codec::field{"to_version", &composite_leg_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_composite_legs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_legs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_composite_legs_request::offset},
        utility::codec::field{"limit", &list_composite_legs_request::limit},
        utility::codec::field{"order", &list_composite_legs_request::order},
        utility::codec::field{"filter", &list_composite_legs_request::filter},
        utility::codec::field{"as_of", &list_composite_legs_request::as_of},
    };
}

/**
 * @brief The members of list_composite_legs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_legs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_composite_legs_response::result},
        utility::codec::field{"composite_legs", &list_composite_legs_response::composite_legs},
        utility::codec::field{"total", &list_composite_legs_response::total},
    };
}

/**
 * @brief The members of get_composite_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_leg_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_leg_request::key},
    };
}

/**
 * @brief The members of get_composite_leg_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_leg_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_leg_response::result},
        utility::codec::field{"composite_leg", &get_composite_leg_response::composite_leg},
    };
}

/**
 * @brief The members of get_many_composite_legs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_composite_legs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_composite_legs_request::keys},
    };
}

/**
 * @brief The members of get_many_composite_legs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_composite_legs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_composite_legs_response::result},
        utility::codec::field{"entries", &get_many_composite_legs_response::entries},
    };
}

/**
 * @brief The members of put_composite_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_composite_leg_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_composite_leg_request::change},
        utility::codec::field{"intent", &put_composite_leg_request::intent},
    };
}

/**
 * @brief The members of put_composite_leg_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_composite_leg_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_composite_leg_response::result},
        utility::codec::field{"composite_leg", &put_composite_leg_response::composite_leg},
    };
}

/**
 * @brief The members of put_many_composite_legs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_composite_legs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_composite_legs_request::changes},
        utility::codec::field{"intent", &put_many_composite_legs_request::intent},
    };
}

/**
 * @brief The members of put_many_composite_legs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_composite_legs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_composite_legs_response::result},
        utility::codec::field{"composite_legs", &put_many_composite_legs_response::composite_legs},
    };
}

/**
 * @brief The members of delete_composite_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_composite_leg_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_composite_leg_request::removal},
        utility::codec::field{"intent", &delete_composite_leg_request::intent},
    };
}

/**
 * @brief The members of delete_composite_leg_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_composite_leg_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_composite_leg_response::result},
    };
}

/**
 * @brief The members of delete_many_composite_legs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_composite_legs_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_composite_legs_request::removals},
        utility::codec::field{"intent", &delete_many_composite_legs_request::intent},
    };
}

/**
 * @brief The members of delete_many_composite_legs_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_composite_legs_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_composite_legs_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_composite_legs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_composite_legs_request&) {
    return std::tuple{
        utility::codec::field{"trade_id", &list_by_trade_id_composite_legs_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_composite_legs_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_composite_legs_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_composite_legs_request::limit},
        utility::codec::field{"order", &list_by_trade_id_composite_legs_request::order},
        utility::codec::field{"filter", &list_by_trade_id_composite_legs_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_composite_legs_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_composite_legs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_composite_legs_response::result},
        utility::codec::field{"composite_legs",
                              &list_by_trade_id_composite_legs_response::composite_legs},
        utility::codec::field{"total", &list_by_trade_id_composite_legs_response::total},
    };
}

/**
 * @brief The members of list_composite_leg_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_leg_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_composite_leg_versions_request::key},
        utility::codec::field{"offset", &list_composite_leg_versions_request::offset},
        utility::codec::field{"limit", &list_composite_leg_versions_request::limit},
        utility::codec::field{"order", &list_composite_leg_versions_request::order},
        utility::codec::field{"filter", &list_composite_leg_versions_request::filter},
    };
}

/**
 * @brief The members of list_composite_leg_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_composite_leg_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_composite_leg_versions_response::result},
        utility::codec::field{"versions", &list_composite_leg_versions_response::versions},
        utility::codec::field{"total", &list_composite_leg_versions_response::total},
    };
}

/**
 * @brief The members of get_composite_leg_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_leg_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_composite_leg_version_request::key},
    };
}

/**
 * @brief The members of get_composite_leg_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_composite_leg_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_composite_leg_version_response::result},
        utility::codec::field{"version", &get_composite_leg_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_key& v);
    static ores::trading::messaging::composite_leg_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_key& v);
    static ores::trading::messaging::composite_leg_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_write& v);
    static ores::trading::messaging::composite_leg_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_write& v);
    static ores::trading::messaging::composite_leg_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_change& v);
    static ores::trading::messaging::composite_leg_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_change& v);
    static ores::trading::messaging::composite_leg_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_removal& v);
    static ores::trading::messaging::composite_leg_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_removal& v);
    static ores::trading::messaging::composite_leg_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_lookup& v);
    static ores::trading::messaging::composite_leg_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_lookup& v);
    static ores::trading::messaging::composite_leg_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_legs_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_legs_filter& v);
    static ores::trading::messaging::composite_legs_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_legs_filter& v);
    static ores::trading::messaging::composite_legs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_event& v);
    static ores::trading::messaging::composite_leg_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_event& v);
    static ores::trading::messaging::composite_leg_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::composite_leg_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_version_key& v);
    static ores::trading::messaging::composite_leg_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::composite_leg_version_key& v);
    static ores::trading::messaging::composite_leg_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::composite_leg_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::composite_leg_versions_filter& v);
    static ores::trading::messaging::composite_leg_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::composite_leg_versions_filter& v);
    static ores::trading::messaging::composite_leg_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_composite_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_composite_legs_request& v);
    static ores::trading::messaging::list_composite_legs_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_composite_legs_request& v);
    static ores::trading::messaging::list_composite_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_composite_legs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_composite_legs_response& v);
    static ores::trading::messaging::list_composite_legs_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_composite_legs_response& v);
    static ores::trading::messaging::list_composite_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_composite_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_composite_leg_request& v);
    static ores::trading::messaging::get_composite_leg_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_composite_leg_request& v);
    static ores::trading::messaging::get_composite_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_composite_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_composite_leg_response& v);
    static ores::trading::messaging::get_composite_leg_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_composite_leg_response& v);
    static ores::trading::messaging::get_composite_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_composite_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_composite_legs_request& v);
    static ores::trading::messaging::get_many_composite_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_composite_legs_request& v);
    static ores::trading::messaging::get_many_composite_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_composite_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_composite_legs_response& v);
    static ores::trading::messaging::get_many_composite_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_composite_legs_response& v);
    static ores::trading::messaging::get_many_composite_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_composite_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_composite_leg_request& v);
    static ores::trading::messaging::put_composite_leg_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_composite_leg_request& v);
    static ores::trading::messaging::put_composite_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_composite_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_composite_leg_response& v);
    static ores::trading::messaging::put_composite_leg_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_composite_leg_response& v);
    static ores::trading::messaging::put_composite_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_composite_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_composite_legs_request& v);
    static ores::trading::messaging::put_many_composite_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_composite_legs_request& v);
    static ores::trading::messaging::put_many_composite_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_composite_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_composite_legs_response& v);
    static ores::trading::messaging::put_many_composite_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_composite_legs_response& v);
    static ores::trading::messaging::put_many_composite_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_composite_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_composite_leg_request& v);
    static ores::trading::messaging::delete_composite_leg_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_composite_leg_request& v);
    static ores::trading::messaging::delete_composite_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_composite_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_composite_leg_response& v);
    static ores::trading::messaging::delete_composite_leg_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_composite_leg_response& v);
    static ores::trading::messaging::delete_composite_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_composite_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_composite_legs_request& v);
    static ores::trading::messaging::delete_many_composite_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_composite_legs_request& v);
    static ores::trading::messaging::delete_many_composite_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_composite_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_composite_legs_response& v);
    static ores::trading::messaging::delete_many_composite_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_composite_legs_response& v);
    static ores::trading::messaging::delete_many_composite_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_composite_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_composite_legs_request& v);
    static ores::trading::messaging::list_by_trade_id_composite_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_composite_legs_request& v);
    static ores::trading::messaging::list_by_trade_id_composite_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_composite_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_composite_legs_response& v);
    static ores::trading::messaging::list_by_trade_id_composite_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_composite_legs_response& v);
    static ores::trading::messaging::list_by_trade_id_composite_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_composite_leg_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_composite_leg_versions_request& v);
    static ores::trading::messaging::list_composite_leg_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_composite_leg_versions_request& v);
    static ores::trading::messaging::list_composite_leg_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_composite_leg_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_composite_leg_versions_response& v);
    static ores::trading::messaging::list_composite_leg_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_composite_leg_versions_response& v);
    static ores::trading::messaging::list_composite_leg_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_composite_leg_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_composite_leg_version_request& v);
    static ores::trading::messaging::get_composite_leg_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_composite_leg_version_request& v);
    static ores::trading::messaging::get_composite_leg_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_composite_leg_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_composite_leg_version_response& v);
    static ores::trading::messaging::get_composite_leg_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_composite_leg_version_response& v);
    static ores::trading::messaging::get_composite_leg_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
