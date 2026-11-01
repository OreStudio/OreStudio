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
#ifndef ORES_TRADING_API_MESSAGING_BOND_FUTURE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_FUTURE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_future.hpp"
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

struct bond_future_key {
    boost::uuids::uuid trade_id;
};

struct bond_future_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string contract_name;
    ores::utility::decimal::decimal contract_notional;
    std::string long_short;
    std::optional<bool> apply_conversion_factor;
    std::optional<bool> use_future_price;
};

struct bond_future_change {
    bond_future_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_future_removal {
    bond_future_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_future_lookup {
    bond_future_key key;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct bond_futures_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct bond_future_event {
    boost::uuids::uuid event_id;
    bond_future_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_future_version_key {
    bond_future_key bond_future;
    std::uint32_t version;
};

struct bond_future_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_futures_request {
    using response_type = struct list_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.list";
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
    std::optional<bond_futures_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> futures;
    std::uint64_t total;
};

struct get_bond_future_request {
    using response_type = struct get_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_key key;
};

struct get_bond_future_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct get_many_bond_futures_request {
    using response_type = struct get_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_key> keys;
};

struct get_many_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<bond_future_lookup> entries;
};

struct put_bond_future_request {
    using response_type = struct put_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_future_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> bond_future;
};

struct put_many_bond_futures_request {
    using response_type = struct put_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_futures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> futures;
};

struct delete_bond_future_request {
    using response_type = struct delete_bond_future_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_future_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_futures_request {
    using response_type = struct delete_many_bond_futures_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_future_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_futures_response {
    ores::utility::domain::result result;
};

struct list_bond_future_versions_request {
    using response_type = struct list_bond_future_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_future_versions_filter> filter;
};

struct list_bond_future_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_future> versions;
    std::uint64_t total;
};

struct get_bond_future_version_request {
    using response_type = struct get_bond_future_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_futures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_future_version_key key;
};

struct get_bond_future_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_future> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_future_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_futures_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_futures_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_futures_events.deleted";
}
/**
 * @brief The members of bond_future_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_future_key::trade_id},
    };
}

/**
 * @brief The members of bond_future_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_future_write::trade_id},
        utility::codec::field{"trade_activity_id", &bond_future_write::trade_activity_id},
        utility::codec::field{"contract_name", &bond_future_write::contract_name},
        utility::codec::field{"contract_notional", &bond_future_write::contract_notional},
        utility::codec::field{"long_short", &bond_future_write::long_short},
        utility::codec::field{"apply_conversion_factor",
                              &bond_future_write::apply_conversion_factor},
        utility::codec::field{"use_future_price", &bond_future_write::use_future_price},
    };
}

/**
 * @brief The members of bond_future_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_future_change::write},
        utility::codec::field{"precondition", &bond_future_change::precondition},
    };
}

/**
 * @brief The members of bond_future_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_future_removal::key},
        utility::codec::field{"precondition", &bond_future_removal::precondition},
    };
}

/**
 * @brief The members of bond_future_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_future_lookup::key},
        utility::codec::field{"bond_future", &bond_future_lookup::bond_future},
    };
}

/**
 * @brief The members of bond_futures_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_futures_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &bond_futures_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of bond_future_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_future_event::event_id},
        utility::codec::field{"key", &bond_future_event::key},
        utility::codec::field{"action", &bond_future_event::action},
        utility::codec::field{"version", &bond_future_event::version},
        utility::codec::field{"occurred_at", &bond_future_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_future_event::correlation_id},
    };
}

/**
 * @brief The members of bond_future_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_future", &bond_future_version_key::bond_future},
        utility::codec::field{"version", &bond_future_version_key::version},
    };
}

/**
 * @brief The members of bond_future_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_future_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_future_versions_filter::version},
        utility::codec::field{"from_version", &bond_future_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_future_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_futures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_futures_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_futures_request::offset},
        utility::codec::field{"limit", &list_bond_futures_request::limit},
        utility::codec::field{"order", &list_bond_futures_request::order},
        utility::codec::field{"filter", &list_bond_futures_request::filter},
        utility::codec::field{"as_of", &list_bond_futures_request::as_of},
    };
}

/**
 * @brief The members of list_bond_futures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_futures_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_futures_response::result},
        utility::codec::field{"futures", &list_bond_futures_response::futures},
        utility::codec::field{"total", &list_bond_futures_response::total},
    };
}

/**
 * @brief The members of get_bond_future_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_future_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_future_request::key},
    };
}

/**
 * @brief The members of get_bond_future_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_future_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_future_response::result},
        utility::codec::field{"bond_future", &get_bond_future_response::bond_future},
    };
}

/**
 * @brief The members of get_many_bond_futures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_futures_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_futures_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_futures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_futures_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_futures_response::result},
        utility::codec::field{"entries", &get_many_bond_futures_response::entries},
    };
}

/**
 * @brief The members of put_bond_future_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_future_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_future_request::change},
        utility::codec::field{"intent", &put_bond_future_request::intent},
    };
}

/**
 * @brief The members of put_bond_future_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_future_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_future_response::result},
        utility::codec::field{"bond_future", &put_bond_future_response::bond_future},
    };
}

/**
 * @brief The members of put_many_bond_futures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_futures_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_futures_request::changes},
        utility::codec::field{"intent", &put_many_bond_futures_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_futures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_futures_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_futures_response::result},
        utility::codec::field{"futures", &put_many_bond_futures_response::futures},
    };
}

/**
 * @brief The members of delete_bond_future_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_future_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_future_request::removal},
        utility::codec::field{"intent", &delete_bond_future_request::intent},
    };
}

/**
 * @brief The members of delete_bond_future_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_future_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_future_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_futures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_futures_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_futures_request::removals},
        utility::codec::field{"intent", &delete_many_bond_futures_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_futures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_futures_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_futures_response::result},
    };
}

/**
 * @brief The members of list_bond_future_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_future_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_future_versions_request::key},
        utility::codec::field{"offset", &list_bond_future_versions_request::offset},
        utility::codec::field{"limit", &list_bond_future_versions_request::limit},
        utility::codec::field{"order", &list_bond_future_versions_request::order},
        utility::codec::field{"filter", &list_bond_future_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_future_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_future_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_future_versions_response::result},
        utility::codec::field{"versions", &list_bond_future_versions_response::versions},
        utility::codec::field{"total", &list_bond_future_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_future_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_future_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_future_version_request::key},
    };
}

/**
 * @brief The members of get_bond_future_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_future_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_future_version_response::result},
        utility::codec::field{"version", &get_bond_future_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_key& v);
    static ores::trading::messaging::bond_future_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_key& v);
    static ores::trading::messaging::bond_future_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_write& v);
    static ores::trading::messaging::bond_future_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_write& v);
    static ores::trading::messaging::bond_future_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_change& v);
    static ores::trading::messaging::bond_future_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_change& v);
    static ores::trading::messaging::bond_future_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_removal& v);
    static ores::trading::messaging::bond_future_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_removal& v);
    static ores::trading::messaging::bond_future_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_lookup& v);
    static ores::trading::messaging::bond_future_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_lookup& v);
    static ores::trading::messaging::bond_future_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_futures_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_futures_filter& v);
    static ores::trading::messaging::bond_futures_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_futures_filter& v);
    static ores::trading::messaging::bond_futures_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_event& v);
    static ores::trading::messaging::bond_future_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_event& v);
    static ores::trading::messaging::bond_future_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_version_key& v);
    static ores::trading::messaging::bond_future_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_future_version_key& v);
    static ores::trading::messaging::bond_future_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_future_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_future_versions_filter& v);
    static ores::trading::messaging::bond_future_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_future_versions_filter& v);
    static ores::trading::messaging::bond_future_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_futures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_futures_request& v);
    static ores::trading::messaging::list_bond_futures_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_futures_request& v);
    static ores::trading::messaging::list_bond_futures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_futures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_futures_response& v);
    static ores::trading::messaging::list_bond_futures_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_futures_response& v);
    static ores::trading::messaging::list_bond_futures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_future_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_future_request& v);
    static ores::trading::messaging::get_bond_future_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_future_request& v);
    static ores::trading::messaging::get_bond_future_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_future_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_future_response& v);
    static ores::trading::messaging::get_bond_future_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_future_response& v);
    static ores::trading::messaging::get_bond_future_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_futures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_bond_futures_request& v);
    static ores::trading::messaging::get_many_bond_futures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_futures_request& v);
    static ores::trading::messaging::get_many_bond_futures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_futures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_futures_response& v);
    static ores::trading::messaging::get_many_bond_futures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_futures_response& v);
    static ores::trading::messaging::get_many_bond_futures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_future_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_future_request& v);
    static ores::trading::messaging::put_bond_future_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_future_request& v);
    static ores::trading::messaging::put_bond_future_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_future_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_future_response& v);
    static ores::trading::messaging::put_bond_future_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_future_response& v);
    static ores::trading::messaging::put_bond_future_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_futures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_bond_futures_request& v);
    static ores::trading::messaging::put_many_bond_futures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_futures_request& v);
    static ores::trading::messaging::put_many_bond_futures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_futures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_futures_response& v);
    static ores::trading::messaging::put_many_bond_futures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_futures_response& v);
    static ores::trading::messaging::put_many_bond_futures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_future_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_future_request& v);
    static ores::trading::messaging::delete_bond_future_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_bond_future_request& v);
    static ores::trading::messaging::delete_bond_future_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_future_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_future_response& v);
    static ores::trading::messaging::delete_bond_future_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_future_response& v);
    static ores::trading::messaging::delete_bond_future_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_futures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_futures_request& v);
    static ores::trading::messaging::delete_many_bond_futures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_futures_request& v);
    static ores::trading::messaging::delete_many_bond_futures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_futures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_futures_response& v);
    static ores::trading::messaging::delete_many_bond_futures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_futures_response& v);
    static ores::trading::messaging::delete_many_bond_futures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_future_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_future_versions_request& v);
    static ores::trading::messaging::list_bond_future_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_future_versions_request& v);
    static ores::trading::messaging::list_bond_future_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_future_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_future_versions_response& v);
    static ores::trading::messaging::list_bond_future_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_future_versions_response& v);
    static ores::trading::messaging::list_bond_future_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_future_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_future_version_request& v);
    static ores::trading::messaging::get_bond_future_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_future_version_request& v);
    static ores::trading::messaging::get_bond_future_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_future_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_future_version_response& v);
    static ores::trading::messaging::get_bond_future_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_future_version_response& v);
    static ores::trading::messaging::get_bond_future_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
