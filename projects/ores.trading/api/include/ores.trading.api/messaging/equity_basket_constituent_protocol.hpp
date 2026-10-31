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
#ifndef ORES_TRADING_API_MESSAGING_EQUITY_BASKET_CONSTITUENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_EQUITY_BASKET_CONSTITUENT_PROTOCOL_HPP

#include "ores.trading.api/domain/equity_basket_constituent.hpp"
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

struct equity_basket_constituent_key {
    boost::uuids::uuid trade_id;
    int sequence_number;
};

struct equity_basket_constituent_write {
    boost::uuids::uuid trade_id;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string underlying_name;
    std::string underlying_kind;
    std::optional<double> weight;
    std::optional<ores::utility::decimal::decimal> initial_price;
};

struct equity_basket_constituent_change {
    equity_basket_constituent_write write;
    ores::utility::domain::precondition precondition;
};

struct equity_basket_constituent_removal {
    equity_basket_constituent_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct equity_basket_constituent_lookup {
    equity_basket_constituent_key key;
    std::optional<ores::trading::domain::equity_basket_constituent> equity_basket_constituent;
};

struct equity_basket_constituent_event {
    boost::uuids::uuid event_id;
    equity_basket_constituent_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct equity_basket_constituent_version_key {
    equity_basket_constituent_key equity_basket_constituent;
    std::uint32_t version;
};

struct equity_basket_constituent_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_equity_basket_constituents_request {
    using response_type = struct list_equity_basket_constituents_response;
    static constexpr std::string_view nats_subject = "trading.v1.equity_basket_constituents.list";
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

struct list_equity_basket_constituents_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_basket_constituent> equity_basket_constituents;
    std::uint64_t total;
};

struct get_equity_basket_constituent_request {
    using response_type = struct get_equity_basket_constituent_response;
    static constexpr std::string_view nats_subject = "trading.v1.equity_basket_constituents.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_basket_constituent_key key;
};

struct get_equity_basket_constituent_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::equity_basket_constituent> equity_basket_constituent;
};

struct get_many_equity_basket_constituents_request {
    using response_type = struct get_many_equity_basket_constituents_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_basket_constituents.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_basket_constituent_key> keys;
};

struct get_many_equity_basket_constituents_response {
    ores::utility::domain::result result;
    std::vector<equity_basket_constituent_lookup> entries;
};

struct put_equity_basket_constituent_request {
    using response_type = struct put_equity_basket_constituent_response;
    static constexpr std::string_view nats_subject = "trading.v1.equity_basket_constituents.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_basket_constituent_change change;
    ores::utility::domain::change_intent intent;
};

struct put_equity_basket_constituent_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::equity_basket_constituent> equity_basket_constituent;
};

struct put_many_equity_basket_constituents_request {
    using response_type = struct put_many_equity_basket_constituents_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_basket_constituents.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_basket_constituent_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_equity_basket_constituents_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_basket_constituent> equity_basket_constituents;
};

struct delete_equity_basket_constituent_request {
    using response_type = struct delete_equity_basket_constituent_response;
    static constexpr std::string_view nats_subject = "trading.v1.equity_basket_constituents.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_basket_constituent_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_equity_basket_constituent_response {
    ores::utility::domain::result result;
};

struct delete_many_equity_basket_constituents_request {
    using response_type = struct delete_many_equity_basket_constituents_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_basket_constituents.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<equity_basket_constituent_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_equity_basket_constituents_response {
    ores::utility::domain::result result;
};

struct list_equity_basket_constituent_versions_request {
    using response_type = struct list_equity_basket_constituent_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_basket_constituents_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_basket_constituent_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<equity_basket_constituent_versions_filter> filter;
};

struct list_equity_basket_constituent_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::equity_basket_constituent> versions;
    std::uint64_t total;
};

struct get_equity_basket_constituent_version_request {
    using response_type = struct get_equity_basket_constituent_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.equity_basket_constituents_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    equity_basket_constituent_version_key key;
};

struct get_equity_basket_constituent_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::equity_basket_constituent> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace equity_basket_constituent_event_subjects {
inline constexpr std::string_view created = "trading.v1.equity_basket_constituents_events.created";
inline constexpr std::string_view updated = "trading.v1.equity_basket_constituents_events.updated";
inline constexpr std::string_view deleted = "trading.v1.equity_basket_constituents_events.deleted";
}
/**
 * @brief The members of equity_basket_constituent_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &equity_basket_constituent_key::trade_id},
        utility::codec::field{"sequence_number", &equity_basket_constituent_key::sequence_number},
    };
}

/**
 * @brief The members of equity_basket_constituent_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &equity_basket_constituent_write::trade_id},
        utility::codec::field{"sequence_number", &equity_basket_constituent_write::sequence_number},
        utility::codec::field{"trade_activity_id",
                              &equity_basket_constituent_write::trade_activity_id},
        utility::codec::field{"underlying_name", &equity_basket_constituent_write::underlying_name},
        utility::codec::field{"underlying_kind", &equity_basket_constituent_write::underlying_kind},
        utility::codec::field{"weight", &equity_basket_constituent_write::weight},
        utility::codec::field{"initial_price", &equity_basket_constituent_write::initial_price},
    };
}

/**
 * @brief The members of equity_basket_constituent_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_change&) {
    return std::tuple{
        utility::codec::field{"write", &equity_basket_constituent_change::write},
        utility::codec::field{"precondition", &equity_basket_constituent_change::precondition},
    };
}

/**
 * @brief The members of equity_basket_constituent_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_removal&) {
    return std::tuple{
        utility::codec::field{"key", &equity_basket_constituent_removal::key},
        utility::codec::field{"precondition", &equity_basket_constituent_removal::precondition},
    };
}

/**
 * @brief The members of equity_basket_constituent_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &equity_basket_constituent_lookup::key},
        utility::codec::field{"equity_basket_constituent",
                              &equity_basket_constituent_lookup::equity_basket_constituent},
    };
}

/**
 * @brief The members of equity_basket_constituent_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &equity_basket_constituent_event::event_id},
        utility::codec::field{"key", &equity_basket_constituent_event::key},
        utility::codec::field{"action", &equity_basket_constituent_event::action},
        utility::codec::field{"version", &equity_basket_constituent_event::version},
        utility::codec::field{"occurred_at", &equity_basket_constituent_event::occurred_at},
        utility::codec::field{"correlation_id", &equity_basket_constituent_event::correlation_id},
    };
}

/**
 * @brief The members of equity_basket_constituent_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_version_key&) {
    return std::tuple{
        utility::codec::field{"equity_basket_constituent",
                              &equity_basket_constituent_version_key::equity_basket_constituent},
        utility::codec::field{"version", &equity_basket_constituent_version_key::version},
    };
}

/**
 * @brief The members of equity_basket_constituent_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const equity_basket_constituent_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &equity_basket_constituent_versions_filter::version},
        utility::codec::field{"from_version",
                              &equity_basket_constituent_versions_filter::from_version},
        utility::codec::field{"to_version", &equity_basket_constituent_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_equity_basket_constituents_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_basket_constituents_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_equity_basket_constituents_request::offset},
        utility::codec::field{"limit", &list_equity_basket_constituents_request::limit},
        utility::codec::field{"order", &list_equity_basket_constituents_request::order},
        utility::codec::field{"as_of", &list_equity_basket_constituents_request::as_of},
    };
}

/**
 * @brief The members of list_equity_basket_constituents_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_basket_constituents_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_equity_basket_constituents_response::result},
        utility::codec::field{
            "equity_basket_constituents",
            &list_equity_basket_constituents_response::equity_basket_constituents},
        utility::codec::field{"total", &list_equity_basket_constituents_response::total},
    };
}

/**
 * @brief The members of get_equity_basket_constituent_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_basket_constituent_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_equity_basket_constituent_request::key},
    };
}

/**
 * @brief The members of get_equity_basket_constituent_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_basket_constituent_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_equity_basket_constituent_response::result},
        utility::codec::field{"equity_basket_constituent",
                              &get_equity_basket_constituent_response::equity_basket_constituent},
    };
}

/**
 * @brief The members of get_many_equity_basket_constituents_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_equity_basket_constituents_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_equity_basket_constituents_request::keys},
    };
}

/**
 * @brief The members of get_many_equity_basket_constituents_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_equity_basket_constituents_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_equity_basket_constituents_response::result},
        utility::codec::field{"entries", &get_many_equity_basket_constituents_response::entries},
    };
}

/**
 * @brief The members of put_equity_basket_constituent_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_equity_basket_constituent_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_equity_basket_constituent_request::change},
        utility::codec::field{"intent", &put_equity_basket_constituent_request::intent},
    };
}

/**
 * @brief The members of put_equity_basket_constituent_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_equity_basket_constituent_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_equity_basket_constituent_response::result},
        utility::codec::field{"equity_basket_constituent",
                              &put_equity_basket_constituent_response::equity_basket_constituent},
    };
}

/**
 * @brief The members of put_many_equity_basket_constituents_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_equity_basket_constituents_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_equity_basket_constituents_request::changes},
        utility::codec::field{"intent", &put_many_equity_basket_constituents_request::intent},
    };
}

/**
 * @brief The members of put_many_equity_basket_constituents_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_equity_basket_constituents_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_equity_basket_constituents_response::result},
        utility::codec::field{
            "equity_basket_constituents",
            &put_many_equity_basket_constituents_response::equity_basket_constituents},
    };
}

/**
 * @brief The members of delete_equity_basket_constituent_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_equity_basket_constituent_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_equity_basket_constituent_request::removal},
        utility::codec::field{"intent", &delete_equity_basket_constituent_request::intent},
    };
}

/**
 * @brief The members of delete_equity_basket_constituent_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_equity_basket_constituent_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_equity_basket_constituent_response::result},
    };
}

/**
 * @brief The members of delete_many_equity_basket_constituents_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_equity_basket_constituents_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_equity_basket_constituents_request::removals},
        utility::codec::field{"intent", &delete_many_equity_basket_constituents_request::intent},
    };
}

/**
 * @brief The members of delete_many_equity_basket_constituents_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_equity_basket_constituents_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_equity_basket_constituents_response::result},
    };
}

/**
 * @brief The members of list_equity_basket_constituent_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_basket_constituent_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_equity_basket_constituent_versions_request::key},
        utility::codec::field{"offset", &list_equity_basket_constituent_versions_request::offset},
        utility::codec::field{"limit", &list_equity_basket_constituent_versions_request::limit},
        utility::codec::field{"order", &list_equity_basket_constituent_versions_request::order},
        utility::codec::field{"filter", &list_equity_basket_constituent_versions_request::filter},
    };
}

/**
 * @brief The members of list_equity_basket_constituent_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_equity_basket_constituent_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_equity_basket_constituent_versions_response::result},
        utility::codec::field{"versions",
                              &list_equity_basket_constituent_versions_response::versions},
        utility::codec::field{"total", &list_equity_basket_constituent_versions_response::total},
    };
}

/**
 * @brief The members of get_equity_basket_constituent_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_basket_constituent_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_equity_basket_constituent_version_request::key},
    };
}

/**
 * @brief The members of get_equity_basket_constituent_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_equity_basket_constituent_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_equity_basket_constituent_version_response::result},
        utility::codec::field{"version", &get_equity_basket_constituent_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::equity_basket_constituent_key& v);
    static ores::trading::messaging::equity_basket_constituent_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_key& v);
    static ores::trading::messaging::equity_basket_constituent_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_write& v);
    static ores::trading::messaging::equity_basket_constituent_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_write& v);
    static ores::trading::messaging::equity_basket_constituent_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_change& v);
    static ores::trading::messaging::equity_basket_constituent_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_change& v);
    static ores::trading::messaging::equity_basket_constituent_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_removal& v);
    static ores::trading::messaging::equity_basket_constituent_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_removal& v);
    static ores::trading::messaging::equity_basket_constituent_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_lookup& v);
    static ores::trading::messaging::equity_basket_constituent_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_lookup& v);
    static ores::trading::messaging::equity_basket_constituent_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_event& v);
    static ores::trading::messaging::equity_basket_constituent_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_event& v);
    static ores::trading::messaging::equity_basket_constituent_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_version_key& v);
    static ores::trading::messaging::equity_basket_constituent_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_version_key& v);
    static ores::trading::messaging::equity_basket_constituent_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::equity_basket_constituent_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::equity_basket_constituent_versions_filter& v);
    static ores::trading::messaging::equity_basket_constituent_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::equity_basket_constituent_versions_filter& v);
    static ores::trading::messaging::equity_basket_constituent_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_equity_basket_constituents_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_equity_basket_constituents_request& v);
    static ores::trading::messaging::list_equity_basket_constituents_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_equity_basket_constituents_request& v);
    static ores::trading::messaging::list_equity_basket_constituents_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_equity_basket_constituents_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_equity_basket_constituents_response& v);
    static ores::trading::messaging::list_equity_basket_constituents_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_equity_basket_constituents_response& v);
    static ores::trading::messaging::list_equity_basket_constituents_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_equity_basket_constituent_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_equity_basket_constituent_request& v);
    static ores::trading::messaging::get_equity_basket_constituent_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_equity_basket_constituent_request& v);
    static ores::trading::messaging::get_equity_basket_constituent_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_equity_basket_constituent_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_equity_basket_constituent_response& v);
    static ores::trading::messaging::get_equity_basket_constituent_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_equity_basket_constituent_response& v);
    static ores::trading::messaging::get_equity_basket_constituent_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_equity_basket_constituents_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::get_many_equity_basket_constituents_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::get_many_equity_basket_constituents_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_equity_basket_constituents_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::get_many_equity_basket_constituents_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::get_many_equity_basket_constituents_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_equity_basket_constituent_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_equity_basket_constituent_request& v);
    static ores::trading::messaging::put_equity_basket_constituent_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_equity_basket_constituent_request& v);
    static ores::trading::messaging::put_equity_basket_constituent_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_equity_basket_constituent_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_equity_basket_constituent_response& v);
    static ores::trading::messaging::put_equity_basket_constituent_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_equity_basket_constituent_response& v);
    static ores::trading::messaging::put_equity_basket_constituent_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_equity_basket_constituents_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::put_many_equity_basket_constituents_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::put_many_equity_basket_constituents_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_equity_basket_constituents_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::put_many_equity_basket_constituents_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::put_many_equity_basket_constituents_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_equity_basket_constituent_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_equity_basket_constituent_request& v);
    static ores::trading::messaging::delete_equity_basket_constituent_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_equity_basket_constituent_request& v);
    static ores::trading::messaging::delete_equity_basket_constituent_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_equity_basket_constituent_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_equity_basket_constituent_response& v);
    static ores::trading::messaging::delete_equity_basket_constituent_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_equity_basket_constituent_response& v);
    static ores::trading::messaging::delete_equity_basket_constituent_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_equity_basket_constituents_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::delete_many_equity_basket_constituents_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_equity_basket_constituents_request& v);
    static ores::trading::messaging::delete_many_equity_basket_constituents_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_equity_basket_constituents_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::delete_many_equity_basket_constituents_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_equity_basket_constituents_response& v);
    static ores::trading::messaging::delete_many_equity_basket_constituents_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_equity_basket_constituent_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_equity_basket_constituent_versions_request& v);
    static ores::trading::messaging::list_equity_basket_constituent_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_equity_basket_constituent_versions_request& v);
    static ores::trading::messaging::list_equity_basket_constituent_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_equity_basket_constituent_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_equity_basket_constituent_versions_response& v);
    static ores::trading::messaging::list_equity_basket_constituent_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_equity_basket_constituent_versions_response& v);
    static ores::trading::messaging::list_equity_basket_constituent_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_equity_basket_constituent_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_equity_basket_constituent_version_request& v);
    static ores::trading::messaging::get_equity_basket_constituent_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_equity_basket_constituent_version_request& v);
    static ores::trading::messaging::get_equity_basket_constituent_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_equity_basket_constituent_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_equity_basket_constituent_version_response& v);
    static ores::trading::messaging::get_equity_basket_constituent_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_equity_basket_constituent_version_response& v);
    static ores::trading::messaging::get_equity_basket_constituent_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
