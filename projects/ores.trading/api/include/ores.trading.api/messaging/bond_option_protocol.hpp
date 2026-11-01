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
#ifndef ORES_TRADING_API_MESSAGING_BOND_OPTION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_OPTION_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_option.hpp"
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

struct bond_option_key {
    boost::uuids::uuid trade_id;
};

struct bond_option_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string option_type;
    ores::utility::decimal::decimal option_strike;
    std::optional<std::string> redemption;
    std::optional<std::string> price_type;
    std::optional<std::string> knocks_out;
};

struct bond_option_change {
    bond_option_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_option_removal {
    bond_option_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_option_lookup {
    bond_option_key key;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct bond_options_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct bond_option_event {
    boost::uuids::uuid event_id;
    bond_option_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_option_version_key {
    bond_option_key bond_option;
    std::uint32_t version;
};

struct bond_option_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_options_request {
    using response_type = struct list_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.list";
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
    std::optional<bond_options_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bond_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> options;
    std::uint64_t total;
};

struct get_bond_option_request {
    using response_type = struct get_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_key key;
};

struct get_bond_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct get_many_bond_options_request {
    using response_type = struct get_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_key> keys;
};

struct get_many_bond_options_response {
    ores::utility::domain::result result;
    std::vector<bond_option_lookup> entries;
};

struct put_bond_option_request {
    using response_type = struct put_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> bond_option;
};

struct put_many_bond_options_request {
    using response_type = struct put_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> options;
};

struct delete_bond_option_request {
    using response_type = struct delete_bond_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_option_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_options_request {
    using response_type = struct delete_many_bond_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_option_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_options_response {
    ores::utility::domain::result result;
};

struct list_bond_option_versions_request {
    using response_type = struct list_bond_option_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_option_versions_filter> filter;
};

struct list_bond_option_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_option> versions;
    std::uint64_t total;
};

struct get_bond_option_version_request {
    using response_type = struct get_bond_option_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_options_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_option_version_key key;
};

struct get_bond_option_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_option> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_option_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_options_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_options_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_options_events.deleted";
}
/**
 * @brief The members of bond_option_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_option_key::trade_id},
    };
}

/**
 * @brief The members of bond_option_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_option_write::trade_id},
        utility::codec::field{"trade_activity_id", &bond_option_write::trade_activity_id},
        utility::codec::field{"option_type", &bond_option_write::option_type},
        utility::codec::field{"option_strike", &bond_option_write::option_strike},
        utility::codec::field{"redemption", &bond_option_write::redemption},
        utility::codec::field{"price_type", &bond_option_write::price_type},
        utility::codec::field{"knocks_out", &bond_option_write::knocks_out},
    };
}

/**
 * @brief The members of bond_option_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_option_change::write},
        utility::codec::field{"precondition", &bond_option_change::precondition},
    };
}

/**
 * @brief The members of bond_option_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_option_removal::key},
        utility::codec::field{"precondition", &bond_option_removal::precondition},
    };
}

/**
 * @brief The members of bond_option_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_option_lookup::key},
        utility::codec::field{"bond_option", &bond_option_lookup::bond_option},
    };
}

/**
 * @brief The members of bond_options_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_options_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &bond_options_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of bond_option_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_option_event::event_id},
        utility::codec::field{"key", &bond_option_event::key},
        utility::codec::field{"action", &bond_option_event::action},
        utility::codec::field{"version", &bond_option_event::version},
        utility::codec::field{"occurred_at", &bond_option_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_option_event::correlation_id},
    };
}

/**
 * @brief The members of bond_option_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_option", &bond_option_version_key::bond_option},
        utility::codec::field{"version", &bond_option_version_key::version},
    };
}

/**
 * @brief The members of bond_option_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_option_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_option_versions_filter::version},
        utility::codec::field{"from_version", &bond_option_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_option_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_options_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_options_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_options_request::offset},
        utility::codec::field{"limit", &list_bond_options_request::limit},
        utility::codec::field{"order", &list_bond_options_request::order},
        utility::codec::field{"filter", &list_bond_options_request::filter},
        utility::codec::field{"as_of", &list_bond_options_request::as_of},
    };
}

/**
 * @brief The members of list_bond_options_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_options_response::result},
        utility::codec::field{"options", &list_bond_options_response::options},
        utility::codec::field{"total", &list_bond_options_response::total},
    };
}

/**
 * @brief The members of get_bond_option_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_option_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_option_request::key},
    };
}

/**
 * @brief The members of get_bond_option_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_option_response::result},
        utility::codec::field{"bond_option", &get_bond_option_response::bond_option},
    };
}

/**
 * @brief The members of get_many_bond_options_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_options_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_options_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_options_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_options_response::result},
        utility::codec::field{"entries", &get_many_bond_options_response::entries},
    };
}

/**
 * @brief The members of put_bond_option_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_option_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_option_request::change},
        utility::codec::field{"intent", &put_bond_option_request::intent},
    };
}

/**
 * @brief The members of put_bond_option_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_option_response::result},
        utility::codec::field{"bond_option", &put_bond_option_response::bond_option},
    };
}

/**
 * @brief The members of put_many_bond_options_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_options_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_options_request::changes},
        utility::codec::field{"intent", &put_many_bond_options_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_options_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_options_response::result},
        utility::codec::field{"options", &put_many_bond_options_response::options},
    };
}

/**
 * @brief The members of delete_bond_option_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_option_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_option_request::removal},
        utility::codec::field{"intent", &delete_bond_option_request::intent},
    };
}

/**
 * @brief The members of delete_bond_option_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_option_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_options_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_options_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_options_request::removals},
        utility::codec::field{"intent", &delete_many_bond_options_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_options_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_options_response::result},
    };
}

/**
 * @brief The members of list_bond_option_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_option_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_option_versions_request::key},
        utility::codec::field{"offset", &list_bond_option_versions_request::offset},
        utility::codec::field{"limit", &list_bond_option_versions_request::limit},
        utility::codec::field{"order", &list_bond_option_versions_request::order},
        utility::codec::field{"filter", &list_bond_option_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_option_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_option_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_option_versions_response::result},
        utility::codec::field{"versions", &list_bond_option_versions_response::versions},
        utility::codec::field{"total", &list_bond_option_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_option_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_option_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_option_version_request::key},
    };
}

/**
 * @brief The members of get_bond_option_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_option_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_option_version_response::result},
        utility::codec::field{"version", &get_bond_option_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_key& v);
    static ores::trading::messaging::bond_option_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_key& v);
    static ores::trading::messaging::bond_option_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_write& v);
    static ores::trading::messaging::bond_option_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_write& v);
    static ores::trading::messaging::bond_option_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_change& v);
    static ores::trading::messaging::bond_option_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_change& v);
    static ores::trading::messaging::bond_option_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_removal& v);
    static ores::trading::messaging::bond_option_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_removal& v);
    static ores::trading::messaging::bond_option_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_lookup& v);
    static ores::trading::messaging::bond_option_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_lookup& v);
    static ores::trading::messaging::bond_option_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_options_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_options_filter& v);
    static ores::trading::messaging::bond_options_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_options_filter& v);
    static ores::trading::messaging::bond_options_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_event& v);
    static ores::trading::messaging::bond_option_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_event& v);
    static ores::trading::messaging::bond_option_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_version_key& v);
    static ores::trading::messaging::bond_option_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_option_version_key& v);
    static ores::trading::messaging::bond_option_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_option_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_option_versions_filter& v);
    static ores::trading::messaging::bond_option_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_option_versions_filter& v);
    static ores::trading::messaging::bond_option_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_options_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_options_request& v);
    static ores::trading::messaging::list_bond_options_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_options_request& v);
    static ores::trading::messaging::list_bond_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_options_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_options_response& v);
    static ores::trading::messaging::list_bond_options_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_options_response& v);
    static ores::trading::messaging::list_bond_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_option_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_option_request& v);
    static ores::trading::messaging::get_bond_option_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_option_request& v);
    static ores::trading::messaging::get_bond_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_option_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_option_response& v);
    static ores::trading::messaging::get_bond_option_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_option_response& v);
    static ores::trading::messaging::get_bond_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_options_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_bond_options_request& v);
    static ores::trading::messaging::get_many_bond_options_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_options_request& v);
    static ores::trading::messaging::get_many_bond_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_options_response& v);
    static ores::trading::messaging::get_many_bond_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_options_response& v);
    static ores::trading::messaging::get_many_bond_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_option_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_option_request& v);
    static ores::trading::messaging::put_bond_option_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_option_request& v);
    static ores::trading::messaging::put_bond_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_option_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_option_response& v);
    static ores::trading::messaging::put_bond_option_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_option_response& v);
    static ores::trading::messaging::put_bond_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_options_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_bond_options_request& v);
    static ores::trading::messaging::put_many_bond_options_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_options_request& v);
    static ores::trading::messaging::put_many_bond_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_options_response& v);
    static ores::trading::messaging::put_many_bond_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_options_response& v);
    static ores::trading::messaging::put_many_bond_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_option_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_option_request& v);
    static ores::trading::messaging::delete_bond_option_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_bond_option_request& v);
    static ores::trading::messaging::delete_bond_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_option_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_option_response& v);
    static ores::trading::messaging::delete_bond_option_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_option_response& v);
    static ores::trading::messaging::delete_bond_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_options_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_options_request& v);
    static ores::trading::messaging::delete_many_bond_options_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_options_request& v);
    static ores::trading::messaging::delete_many_bond_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_options_response& v);
    static ores::trading::messaging::delete_many_bond_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_options_response& v);
    static ores::trading::messaging::delete_many_bond_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_option_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_option_versions_request& v);
    static ores::trading::messaging::list_bond_option_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_option_versions_request& v);
    static ores::trading::messaging::list_bond_option_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_option_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_option_versions_response& v);
    static ores::trading::messaging::list_bond_option_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_option_versions_response& v);
    static ores::trading::messaging::list_bond_option_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_option_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_option_version_request& v);
    static ores::trading::messaging::get_bond_option_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_option_version_request& v);
    static ores::trading::messaging::get_bond_option_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_option_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_option_version_response& v);
    static ores::trading::messaging::get_bond_option_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_option_version_response& v);
    static ores::trading::messaging::get_bond_option_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
