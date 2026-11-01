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
#ifndef ORES_TRADING_API_MESSAGING_CALLABLE_SWAP_INSTRUMENT_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_CALLABLE_SWAP_INSTRUMENT_PROTOCOL_HPP

#include "ores.trading.api/domain/callable_swap_instrument.hpp"
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

struct callable_swap_instrument_key {
    boost::uuids::uuid trade_id;
};

struct callable_swap_instrument_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
};

struct callable_swap_instrument_change {
    callable_swap_instrument_write write;
    ores::utility::domain::precondition precondition;
};

struct callable_swap_instrument_removal {
    callable_swap_instrument_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct callable_swap_instrument_lookup {
    callable_swap_instrument_key key;
    std::optional<ores::trading::domain::callable_swap_instrument> callable_swap_instrument;
};

struct callable_swap_instruments_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct callable_swap_instrument_event {
    boost::uuids::uuid event_id;
    callable_swap_instrument_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct callable_swap_instrument_version_key {
    callable_swap_instrument_key callable_swap_instrument;
    std::uint32_t version;
};

struct callable_swap_instrument_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_callable_swap_instruments_request {
    using response_type = struct list_callable_swap_instruments_response;
    static constexpr std::string_view nats_subject = "trading.v1.callable_swap_instruments.list";
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
    std::optional<callable_swap_instruments_filter> filter;
    std::optional<std::string> as_of;
};

struct list_callable_swap_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::callable_swap_instrument> callable_swap_instruments;
    std::uint64_t total;
};

struct get_callable_swap_instrument_request {
    using response_type = struct get_callable_swap_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.callable_swap_instruments.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    callable_swap_instrument_key key;
};

struct get_callable_swap_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::callable_swap_instrument> callable_swap_instrument;
};

struct get_many_callable_swap_instruments_request {
    using response_type = struct get_many_callable_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<callable_swap_instrument_key> keys;
};

struct get_many_callable_swap_instruments_response {
    ores::utility::domain::result result;
    std::vector<callable_swap_instrument_lookup> entries;
};

struct put_callable_swap_instrument_request {
    using response_type = struct put_callable_swap_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.callable_swap_instruments.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    callable_swap_instrument_change change;
    ores::utility::domain::change_intent intent;
};

struct put_callable_swap_instrument_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::callable_swap_instrument> callable_swap_instrument;
};

struct put_many_callable_swap_instruments_request {
    using response_type = struct put_many_callable_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<callable_swap_instrument_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_callable_swap_instruments_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::callable_swap_instrument> callable_swap_instruments;
};

struct delete_callable_swap_instrument_request {
    using response_type = struct delete_callable_swap_instrument_response;
    static constexpr std::string_view nats_subject = "trading.v1.callable_swap_instruments.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    callable_swap_instrument_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_callable_swap_instrument_response {
    ores::utility::domain::result result;
};

struct delete_many_callable_swap_instruments_request {
    using response_type = struct delete_many_callable_swap_instruments_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<callable_swap_instrument_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_callable_swap_instruments_response {
    ores::utility::domain::result result;
};

struct list_callable_swap_instrument_versions_request {
    using response_type = struct list_callable_swap_instrument_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    callable_swap_instrument_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<callable_swap_instrument_versions_filter> filter;
};

struct list_callable_swap_instrument_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::callable_swap_instrument> versions;
    std::uint64_t total;
};

struct get_callable_swap_instrument_version_request {
    using response_type = struct get_callable_swap_instrument_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.callable_swap_instruments_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    callable_swap_instrument_version_key key;
};

struct get_callable_swap_instrument_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::callable_swap_instrument> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace callable_swap_instrument_event_subjects {
inline constexpr std::string_view created = "trading.v1.callable_swap_instruments_events.created";
inline constexpr std::string_view updated = "trading.v1.callable_swap_instruments_events.updated";
inline constexpr std::string_view deleted = "trading.v1.callable_swap_instruments_events.deleted";
}
/**
 * @brief The members of callable_swap_instrument_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &callable_swap_instrument_key::trade_id},
    };
}

/**
 * @brief The members of callable_swap_instrument_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &callable_swap_instrument_write::trade_id},
        utility::codec::field{"trade_activity_id",
                              &callable_swap_instrument_write::trade_activity_id},
    };
}

/**
 * @brief The members of callable_swap_instrument_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_change&) {
    return std::tuple{
        utility::codec::field{"write", &callable_swap_instrument_change::write},
        utility::codec::field{"precondition", &callable_swap_instrument_change::precondition},
    };
}

/**
 * @brief The members of callable_swap_instrument_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_removal&) {
    return std::tuple{
        utility::codec::field{"key", &callable_swap_instrument_removal::key},
        utility::codec::field{"precondition", &callable_swap_instrument_removal::precondition},
    };
}

/**
 * @brief The members of callable_swap_instrument_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &callable_swap_instrument_lookup::key},
        utility::codec::field{"callable_swap_instrument",
                              &callable_swap_instrument_lookup::callable_swap_instrument},
    };
}

/**
 * @brief The members of callable_swap_instruments_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instruments_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of",
                              &callable_swap_instruments_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of callable_swap_instrument_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &callable_swap_instrument_event::event_id},
        utility::codec::field{"key", &callable_swap_instrument_event::key},
        utility::codec::field{"action", &callable_swap_instrument_event::action},
        utility::codec::field{"version", &callable_swap_instrument_event::version},
        utility::codec::field{"occurred_at", &callable_swap_instrument_event::occurred_at},
        utility::codec::field{"correlation_id", &callable_swap_instrument_event::correlation_id},
    };
}

/**
 * @brief The members of callable_swap_instrument_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_version_key&) {
    return std::tuple{
        utility::codec::field{"callable_swap_instrument",
                              &callable_swap_instrument_version_key::callable_swap_instrument},
        utility::codec::field{"version", &callable_swap_instrument_version_key::version},
    };
}

/**
 * @brief The members of callable_swap_instrument_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const callable_swap_instrument_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &callable_swap_instrument_versions_filter::version},
        utility::codec::field{"from_version",
                              &callable_swap_instrument_versions_filter::from_version},
        utility::codec::field{"to_version", &callable_swap_instrument_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_callable_swap_instruments_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_callable_swap_instruments_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_callable_swap_instruments_request::offset},
        utility::codec::field{"limit", &list_callable_swap_instruments_request::limit},
        utility::codec::field{"order", &list_callable_swap_instruments_request::order},
        utility::codec::field{"filter", &list_callable_swap_instruments_request::filter},
        utility::codec::field{"as_of", &list_callable_swap_instruments_request::as_of},
    };
}

/**
 * @brief The members of list_callable_swap_instruments_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_callable_swap_instruments_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_callable_swap_instruments_response::result},
        utility::codec::field{"callable_swap_instruments",
                              &list_callable_swap_instruments_response::callable_swap_instruments},
        utility::codec::field{"total", &list_callable_swap_instruments_response::total},
    };
}

/**
 * @brief The members of get_callable_swap_instrument_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_callable_swap_instrument_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_callable_swap_instrument_request::key},
    };
}

/**
 * @brief The members of get_callable_swap_instrument_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_callable_swap_instrument_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_callable_swap_instrument_response::result},
        utility::codec::field{"callable_swap_instrument",
                              &get_callable_swap_instrument_response::callable_swap_instrument},
    };
}

/**
 * @brief The members of get_many_callable_swap_instruments_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_callable_swap_instruments_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_callable_swap_instruments_request::keys},
    };
}

/**
 * @brief The members of get_many_callable_swap_instruments_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_callable_swap_instruments_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_callable_swap_instruments_response::result},
        utility::codec::field{"entries", &get_many_callable_swap_instruments_response::entries},
    };
}

/**
 * @brief The members of put_callable_swap_instrument_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_callable_swap_instrument_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_callable_swap_instrument_request::change},
        utility::codec::field{"intent", &put_callable_swap_instrument_request::intent},
    };
}

/**
 * @brief The members of put_callable_swap_instrument_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_callable_swap_instrument_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_callable_swap_instrument_response::result},
        utility::codec::field{"callable_swap_instrument",
                              &put_callable_swap_instrument_response::callable_swap_instrument},
    };
}

/**
 * @brief The members of put_many_callable_swap_instruments_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_callable_swap_instruments_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_callable_swap_instruments_request::changes},
        utility::codec::field{"intent", &put_many_callable_swap_instruments_request::intent},
    };
}

/**
 * @brief The members of put_many_callable_swap_instruments_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_callable_swap_instruments_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_callable_swap_instruments_response::result},
        utility::codec::field{
            "callable_swap_instruments",
            &put_many_callable_swap_instruments_response::callable_swap_instruments},
    };
}

/**
 * @brief The members of delete_callable_swap_instrument_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_callable_swap_instrument_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_callable_swap_instrument_request::removal},
        utility::codec::field{"intent", &delete_callable_swap_instrument_request::intent},
    };
}

/**
 * @brief The members of delete_callable_swap_instrument_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_callable_swap_instrument_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_callable_swap_instrument_response::result},
    };
}

/**
 * @brief The members of delete_many_callable_swap_instruments_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_callable_swap_instruments_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_callable_swap_instruments_request::removals},
        utility::codec::field{"intent", &delete_many_callable_swap_instruments_request::intent},
    };
}

/**
 * @brief The members of delete_many_callable_swap_instruments_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_callable_swap_instruments_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_callable_swap_instruments_response::result},
    };
}

/**
 * @brief The members of list_callable_swap_instrument_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_callable_swap_instrument_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_callable_swap_instrument_versions_request::key},
        utility::codec::field{"offset", &list_callable_swap_instrument_versions_request::offset},
        utility::codec::field{"limit", &list_callable_swap_instrument_versions_request::limit},
        utility::codec::field{"order", &list_callable_swap_instrument_versions_request::order},
        utility::codec::field{"filter", &list_callable_swap_instrument_versions_request::filter},
    };
}

/**
 * @brief The members of list_callable_swap_instrument_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_callable_swap_instrument_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_callable_swap_instrument_versions_response::result},
        utility::codec::field{"versions",
                              &list_callable_swap_instrument_versions_response::versions},
        utility::codec::field{"total", &list_callable_swap_instrument_versions_response::total},
    };
}

/**
 * @brief The members of get_callable_swap_instrument_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_callable_swap_instrument_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_callable_swap_instrument_version_request::key},
    };
}

/**
 * @brief The members of get_callable_swap_instrument_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_callable_swap_instrument_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_callable_swap_instrument_version_response::result},
        utility::codec::field{"version", &get_callable_swap_instrument_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::callable_swap_instrument_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::callable_swap_instrument_key& v);
    static ores::trading::messaging::callable_swap_instrument_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_key& v);
    static ores::trading::messaging::callable_swap_instrument_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_write& v);
    static ores::trading::messaging::callable_swap_instrument_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_write& v);
    static ores::trading::messaging::callable_swap_instrument_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_change& v);
    static ores::trading::messaging::callable_swap_instrument_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_change& v);
    static ores::trading::messaging::callable_swap_instrument_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_removal& v);
    static ores::trading::messaging::callable_swap_instrument_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_removal& v);
    static ores::trading::messaging::callable_swap_instrument_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_lookup& v);
    static ores::trading::messaging::callable_swap_instrument_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_lookup& v);
    static ores::trading::messaging::callable_swap_instrument_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instruments_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instruments_filter& v);
    static ores::trading::messaging::callable_swap_instruments_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instruments_filter& v);
    static ores::trading::messaging::callable_swap_instruments_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_event& v);
    static ores::trading::messaging::callable_swap_instrument_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_event& v);
    static ores::trading::messaging::callable_swap_instrument_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_version_key& v);
    static ores::trading::messaging::callable_swap_instrument_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_version_key& v);
    static ores::trading::messaging::callable_swap_instrument_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::callable_swap_instrument_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::callable_swap_instrument_versions_filter& v);
    static ores::trading::messaging::callable_swap_instrument_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::callable_swap_instrument_versions_filter& v);
    static ores::trading::messaging::callable_swap_instrument_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_callable_swap_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_callable_swap_instruments_request& v);
    static ores::trading::messaging::list_callable_swap_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_callable_swap_instruments_request& v);
    static ores::trading::messaging::list_callable_swap_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_callable_swap_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_callable_swap_instruments_response& v);
    static ores::trading::messaging::list_callable_swap_instruments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_callable_swap_instruments_response& v);
    static ores::trading::messaging::list_callable_swap_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_callable_swap_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_callable_swap_instrument_request& v);
    static ores::trading::messaging::get_callable_swap_instrument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_callable_swap_instrument_request& v);
    static ores::trading::messaging::get_callable_swap_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_callable_swap_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_callable_swap_instrument_response& v);
    static ores::trading::messaging::get_callable_swap_instrument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_callable_swap_instrument_response& v);
    static ores::trading::messaging::get_callable_swap_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_callable_swap_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::get_many_callable_swap_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::get_many_callable_swap_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_callable_swap_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::get_many_callable_swap_instruments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::get_many_callable_swap_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_callable_swap_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_callable_swap_instrument_request& v);
    static ores::trading::messaging::put_callable_swap_instrument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_callable_swap_instrument_request& v);
    static ores::trading::messaging::put_callable_swap_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_callable_swap_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_callable_swap_instrument_response& v);
    static ores::trading::messaging::put_callable_swap_instrument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_callable_swap_instrument_response& v);
    static ores::trading::messaging::put_callable_swap_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_callable_swap_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::put_many_callable_swap_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::put_many_callable_swap_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_callable_swap_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::put_many_callable_swap_instruments_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::put_many_callable_swap_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_callable_swap_instrument_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_callable_swap_instrument_request& v);
    static ores::trading::messaging::delete_callable_swap_instrument_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_callable_swap_instrument_request& v);
    static ores::trading::messaging::delete_callable_swap_instrument_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_callable_swap_instrument_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_callable_swap_instrument_response& v);
    static ores::trading::messaging::delete_callable_swap_instrument_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_callable_swap_instrument_response& v);
    static ores::trading::messaging::delete_callable_swap_instrument_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_callable_swap_instruments_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::delete_many_callable_swap_instruments_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_callable_swap_instruments_request& v);
    static ores::trading::messaging::delete_many_callable_swap_instruments_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_callable_swap_instruments_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::delete_many_callable_swap_instruments_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_callable_swap_instruments_response& v);
    static ores::trading::messaging::delete_many_callable_swap_instruments_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_callable_swap_instrument_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_callable_swap_instrument_versions_request& v);
    static ores::trading::messaging::list_callable_swap_instrument_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_callable_swap_instrument_versions_request& v);
    static ores::trading::messaging::list_callable_swap_instrument_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_callable_swap_instrument_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_callable_swap_instrument_versions_response& v);
    static ores::trading::messaging::list_callable_swap_instrument_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_callable_swap_instrument_versions_response& v);
    static ores::trading::messaging::list_callable_swap_instrument_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_callable_swap_instrument_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_callable_swap_instrument_version_request& v);
    static ores::trading::messaging::get_callable_swap_instrument_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_callable_swap_instrument_version_request& v);
    static ores::trading::messaging::get_callable_swap_instrument_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_callable_swap_instrument_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_callable_swap_instrument_version_response& v);
    static ores::trading::messaging::get_callable_swap_instrument_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_callable_swap_instrument_version_response& v);
    static ores::trading::messaging::get_callable_swap_instrument_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
