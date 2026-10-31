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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_STRIKE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_STRIKE_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_strike.hpp"
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

struct instrument_strike_key {
    boost::uuids::uuid trade_id;
    std::string strike_role;
    int sequence_number;
};

struct instrument_strike_write {
    boost::uuids::uuid trade_id;
    std::string strike_role;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::optional<ores::utility::decimal::decimal> price_value;
    std::optional<std::string> price_currency;
    std::optional<ores::utility::decimal::decimal> yield_value;
    std::optional<std::string> yield_compounding;
    std::optional<ores::utility::decimal::decimal> bare_value;
    std::optional<std::string> bare_currency;
};

struct instrument_strike_change {
    instrument_strike_write write;
    ores::utility::domain::precondition precondition;
};

struct instrument_strike_removal {
    instrument_strike_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct instrument_strike_lookup {
    instrument_strike_key key;
    std::optional<ores::trading::domain::instrument_strike> instrument_strike;
};

struct instrument_strike_event {
    boost::uuids::uuid event_id;
    instrument_strike_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct instrument_strike_version_key {
    instrument_strike_key instrument_strike;
    std::uint32_t version;
};

struct instrument_strike_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_instrument_strikes_request {
    using response_type = struct list_instrument_strikes_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.list";
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
    std::optional<std::string> as_of;
};

struct list_instrument_strikes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_strike> instrument_strikes;
    std::uint64_t total;
};

struct get_instrument_strike_request {
    using response_type = struct get_instrument_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_strike_key key;
};

struct get_instrument_strike_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_strike> instrument_strike;
};

struct get_many_instrument_strikes_request {
    using response_type = struct get_many_instrument_strikes_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_strike_key> keys;
};

struct get_many_instrument_strikes_response {
    ores::utility::domain::result result;
    std::vector<instrument_strike_lookup> entries;
};

struct put_instrument_strike_request {
    using response_type = struct put_instrument_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_strike_change change;
    ores::utility::domain::change_intent intent;
};

struct put_instrument_strike_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_strike> instrument_strike;
};

struct put_many_instrument_strikes_request {
    using response_type = struct put_many_instrument_strikes_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_strike_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_instrument_strikes_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_strike> instrument_strikes;
};

struct delete_instrument_strike_request {
    using response_type = struct delete_instrument_strike_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_strike_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_instrument_strike_response {
    ores::utility::domain::result result;
};

struct delete_many_instrument_strikes_request {
    using response_type = struct delete_many_instrument_strikes_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_strike_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_instrument_strikes_response {
    ores::utility::domain::result result;
};

struct list_instrument_strike_versions_request {
    using response_type = struct list_instrument_strike_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_strike_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_strike_versions_filter> filter;
};

struct list_instrument_strike_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_strike> versions;
    std::uint64_t total;
};

struct get_instrument_strike_version_request {
    using response_type = struct get_instrument_strike_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_strikes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_strike_version_key key;
};

struct get_instrument_strike_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_strike> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_strike_event_subjects {
inline constexpr std::string_view created = "trading.v1.instrument_strikes_events.created";
inline constexpr std::string_view updated = "trading.v1.instrument_strikes_events.updated";
inline constexpr std::string_view deleted = "trading.v1.instrument_strikes_events.deleted";
}
/**
 * @brief The members of instrument_strike_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_strike_key::trade_id},
        utility::codec::field{"strike_role", &instrument_strike_key::strike_role},
        utility::codec::field{"sequence_number", &instrument_strike_key::sequence_number},
    };
}

/**
 * @brief The members of instrument_strike_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_strike_write::trade_id},
        utility::codec::field{"strike_role", &instrument_strike_write::strike_role},
        utility::codec::field{"sequence_number", &instrument_strike_write::sequence_number},
        utility::codec::field{"trade_activity_id", &instrument_strike_write::trade_activity_id},
        utility::codec::field{"price_value", &instrument_strike_write::price_value},
        utility::codec::field{"price_currency", &instrument_strike_write::price_currency},
        utility::codec::field{"yield_value", &instrument_strike_write::yield_value},
        utility::codec::field{"yield_compounding", &instrument_strike_write::yield_compounding},
        utility::codec::field{"bare_value", &instrument_strike_write::bare_value},
        utility::codec::field{"bare_currency", &instrument_strike_write::bare_currency},
    };
}

/**
 * @brief The members of instrument_strike_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_change&) {
    return std::tuple{
        utility::codec::field{"write", &instrument_strike_change::write},
        utility::codec::field{"precondition", &instrument_strike_change::precondition},
    };
}

/**
 * @brief The members of instrument_strike_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_removal&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_strike_removal::key},
        utility::codec::field{"precondition", &instrument_strike_removal::precondition},
    };
}

/**
 * @brief The members of instrument_strike_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_strike_lookup::key},
        utility::codec::field{"instrument_strike", &instrument_strike_lookup::instrument_strike},
    };
}

/**
 * @brief The members of instrument_strike_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &instrument_strike_event::event_id},
        utility::codec::field{"key", &instrument_strike_event::key},
        utility::codec::field{"action", &instrument_strike_event::action},
        utility::codec::field{"version", &instrument_strike_event::version},
        utility::codec::field{"occurred_at", &instrument_strike_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_strike_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_strike_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_version_key&) {
    return std::tuple{
        utility::codec::field{"instrument_strike",
                              &instrument_strike_version_key::instrument_strike},
        utility::codec::field{"version", &instrument_strike_version_key::version},
    };
}

/**
 * @brief The members of instrument_strike_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_strike_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &instrument_strike_versions_filter::version},
        utility::codec::field{"from_version", &instrument_strike_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_strike_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_strikes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_strikes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_instrument_strikes_request::offset},
        utility::codec::field{"limit", &list_instrument_strikes_request::limit},
        utility::codec::field{"order", &list_instrument_strikes_request::order},
        utility::codec::field{"as_of", &list_instrument_strikes_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_strikes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_strikes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_strikes_response::result},
        utility::codec::field{"instrument_strikes",
                              &list_instrument_strikes_response::instrument_strikes},
        utility::codec::field{"total", &list_instrument_strikes_response::total},
    };
}

/**
 * @brief The members of get_instrument_strike_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_strike_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_strike_request::key},
    };
}

/**
 * @brief The members of get_instrument_strike_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_strike_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_strike_response::result},
        utility::codec::field{"instrument_strike",
                              &get_instrument_strike_response::instrument_strike},
    };
}

/**
 * @brief The members of get_many_instrument_strikes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_strikes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_instrument_strikes_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_strikes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_strikes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_instrument_strikes_response::result},
        utility::codec::field{"entries", &get_many_instrument_strikes_response::entries},
    };
}

/**
 * @brief The members of put_instrument_strike_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_strike_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_instrument_strike_request::change},
        utility::codec::field{"intent", &put_instrument_strike_request::intent},
    };
}

/**
 * @brief The members of put_instrument_strike_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_strike_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_instrument_strike_response::result},
        utility::codec::field{"instrument_strike",
                              &put_instrument_strike_response::instrument_strike},
    };
}

/**
 * @brief The members of put_many_instrument_strikes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_strikes_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_instrument_strikes_request::changes},
        utility::codec::field{"intent", &put_many_instrument_strikes_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_strikes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_strikes_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_instrument_strikes_response::result},
        utility::codec::field{"instrument_strikes",
                              &put_many_instrument_strikes_response::instrument_strikes},
    };
}

/**
 * @brief The members of delete_instrument_strike_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_strike_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_instrument_strike_request::removal},
        utility::codec::field{"intent", &delete_instrument_strike_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_strike_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_strike_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_instrument_strike_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_strikes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_strikes_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_instrument_strikes_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_strikes_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_strikes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_strikes_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_instrument_strikes_response::result},
    };
}

/**
 * @brief The members of list_instrument_strike_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_strike_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_instrument_strike_versions_request::key},
        utility::codec::field{"offset", &list_instrument_strike_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_strike_versions_request::limit},
        utility::codec::field{"order", &list_instrument_strike_versions_request::order},
        utility::codec::field{"filter", &list_instrument_strike_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_strike_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_strike_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_strike_versions_response::result},
        utility::codec::field{"versions", &list_instrument_strike_versions_response::versions},
        utility::codec::field{"total", &list_instrument_strike_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_strike_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_strike_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_strike_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_strike_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_strike_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_strike_version_response::result},
        utility::codec::field{"version", &get_instrument_strike_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_key& v);
    static ores::trading::messaging::instrument_strike_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_key& v);
    static ores::trading::messaging::instrument_strike_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_write& v);
    static ores::trading::messaging::instrument_strike_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_write& v);
    static ores::trading::messaging::instrument_strike_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_change& v);
    static ores::trading::messaging::instrument_strike_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_change& v);
    static ores::trading::messaging::instrument_strike_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_removal& v);
    static ores::trading::messaging::instrument_strike_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_removal& v);
    static ores::trading::messaging::instrument_strike_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_lookup& v);
    static ores::trading::messaging::instrument_strike_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_lookup& v);
    static ores::trading::messaging::instrument_strike_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_strike_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_event& v);
    static ores::trading::messaging::instrument_strike_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_strike_event& v);
    static ores::trading::messaging::instrument_strike_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_strike_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_strike_version_key& v);
    static ores::trading::messaging::instrument_strike_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_strike_version_key& v);
    static ores::trading::messaging::instrument_strike_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_strike_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_strike_versions_filter& v);
    static ores::trading::messaging::instrument_strike_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_strike_versions_filter& v);
    static ores::trading::messaging::instrument_strike_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_strikes_request& v);
    static ores::trading::messaging::list_instrument_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_strikes_request& v);
    static ores::trading::messaging::list_instrument_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_strikes_response& v);
    static ores::trading::messaging::list_instrument_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_strikes_response& v);
    static ores::trading::messaging::list_instrument_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_strike_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_instrument_strike_request& v);
    static ores::trading::messaging::get_instrument_strike_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_strike_request& v);
    static ores::trading::messaging::get_instrument_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_strike_response& v);
    static ores::trading::messaging::get_instrument_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_strike_response& v);
    static ores::trading::messaging::get_instrument_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_strikes_request& v);
    static ores::trading::messaging::get_many_instrument_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_strikes_request& v);
    static ores::trading::messaging::get_many_instrument_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_strikes_response& v);
    static ores::trading::messaging::get_many_instrument_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_strikes_response& v);
    static ores::trading::messaging::get_many_instrument_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_strike_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_instrument_strike_request& v);
    static ores::trading::messaging::put_instrument_strike_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_strike_request& v);
    static ores::trading::messaging::put_instrument_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_strike_response& v);
    static ores::trading::messaging::put_instrument_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_strike_response& v);
    static ores::trading::messaging::put_instrument_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_strikes_request& v);
    static ores::trading::messaging::put_many_instrument_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_strikes_request& v);
    static ores::trading::messaging::put_many_instrument_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_strikes_response& v);
    static ores::trading::messaging::put_many_instrument_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_strikes_response& v);
    static ores::trading::messaging::put_many_instrument_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_strike_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_strike_request& v);
    static ores::trading::messaging::delete_instrument_strike_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_strike_request& v);
    static ores::trading::messaging::delete_instrument_strike_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_strike_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_strike_response& v);
    static ores::trading::messaging::delete_instrument_strike_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_strike_response& v);
    static ores::trading::messaging::delete_instrument_strike_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_strikes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_strikes_request& v);
    static ores::trading::messaging::delete_many_instrument_strikes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_strikes_request& v);
    static ores::trading::messaging::delete_many_instrument_strikes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_strikes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_strikes_response& v);
    static ores::trading::messaging::delete_many_instrument_strikes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_strikes_response& v);
    static ores::trading::messaging::delete_many_instrument_strikes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_strike_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_strike_versions_request& v);
    static ores::trading::messaging::list_instrument_strike_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_strike_versions_request& v);
    static ores::trading::messaging::list_instrument_strike_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_strike_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_strike_versions_response& v);
    static ores::trading::messaging::list_instrument_strike_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_strike_versions_response& v);
    static ores::trading::messaging::list_instrument_strike_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_strike_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_strike_version_request& v);
    static ores::trading::messaging::get_instrument_strike_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_strike_version_request& v);
    static ores::trading::messaging::get_instrument_strike_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_strike_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_strike_version_response& v);
    static ores::trading::messaging::get_instrument_strike_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_strike_version_response& v);
    static ores::trading::messaging::get_instrument_strike_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
