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
#ifndef ORES_TRADING_API_MESSAGING_TRADE_AUTHORISATION_REQUEST_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_TRADE_AUTHORISATION_REQUEST_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_authorisation_request.hpp"
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

struct trade_authorisation_request_key {
    boost::uuids::uuid request_id;
};

struct trade_authorisation_request_write {
    boost::uuids::uuid request_id;
    boost::uuids::uuid trade_id;
    int trade_version;
};

struct trade_authorisation_request_change {
    trade_authorisation_request_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_authorisation_request_removal {
    trade_authorisation_request_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_authorisation_request_lookup {
    trade_authorisation_request_key key;
    std::optional<ores::trading::domain::trade_authorisation_request> trade_authorisation_request;
};

struct trade_authorisation_requests_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct trade_authorisation_request_event {
    boost::uuids::uuid event_id;
    trade_authorisation_request_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_authorisation_request_version_key {
    trade_authorisation_request_key trade_authorisation_request;
    std::uint32_t version;
};

struct trade_authorisation_request_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_authorisation_requests_request {
    using response_type = struct list_trade_authorisation_requests_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_authorisation_requests.list";
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
    std::optional<trade_authorisation_requests_filter> filter;
    std::optional<std::string> as_of;
};

struct list_trade_authorisation_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_authorisation_request> trade_authorisation_requests;
    std::uint64_t total;
};

struct get_trade_authorisation_request_request {
    using response_type = struct get_trade_authorisation_request_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_authorisation_requests.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_authorisation_request_key key;
};

struct get_trade_authorisation_request_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_authorisation_request> trade_authorisation_request;
};

struct get_many_trade_authorisation_requests_request {
    using response_type = struct get_many_trade_authorisation_requests_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_authorisation_request_key> keys;
};

struct get_many_trade_authorisation_requests_response {
    ores::utility::domain::result result;
    std::vector<trade_authorisation_request_lookup> entries;
};

struct put_trade_authorisation_request_request {
    using response_type = struct put_trade_authorisation_request_response;
    static constexpr std::string_view nats_subject = "trading.v1.trade_authorisation_requests.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_authorisation_request_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_authorisation_request_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_authorisation_request> trade_authorisation_request;
};

struct put_many_trade_authorisation_requests_request {
    using response_type = struct put_many_trade_authorisation_requests_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_authorisation_request_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_authorisation_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_authorisation_request> trade_authorisation_requests;
};

struct delete_trade_authorisation_request_request {
    using response_type = struct delete_trade_authorisation_request_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_authorisation_request_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_authorisation_request_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_authorisation_requests_request {
    using response_type = struct delete_many_trade_authorisation_requests_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_authorisation_request_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_authorisation_requests_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_trade_authorisation_requests_request {
    using response_type = struct list_by_trade_id_trade_authorisation_requests_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests.list_by_trade_id";
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
    std::optional<trade_authorisation_requests_filter> filter;
};

struct list_by_trade_id_trade_authorisation_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_authorisation_request> trade_authorisation_requests;
    std::uint64_t total;
};

struct list_trade_authorisation_request_versions_request {
    using response_type = struct list_trade_authorisation_request_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_authorisation_request_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_authorisation_request_versions_filter> filter;
};

struct list_trade_authorisation_request_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::trade_authorisation_request> versions;
    std::uint64_t total;
};

struct get_trade_authorisation_request_version_request {
    using response_type = struct get_trade_authorisation_request_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.trade_authorisation_requests_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_authorisation_request_version_key key;
};

struct get_trade_authorisation_request_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::trade_authorisation_request> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_authorisation_request_event_subjects {
inline constexpr std::string_view created =
    "trading.v1.trade_authorisation_requests_events.created";
inline constexpr std::string_view updated =
    "trading.v1.trade_authorisation_requests_events.updated";
inline constexpr std::string_view deleted =
    "trading.v1.trade_authorisation_requests_events.deleted";
}
/**
 * @brief The members of trade_authorisation_request_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_key&) {
    return std::tuple{
        utility::codec::field{"request_id", &trade_authorisation_request_key::request_id},
    };
}

/**
 * @brief The members of trade_authorisation_request_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_write&) {
    return std::tuple{
        utility::codec::field{"request_id", &trade_authorisation_request_write::request_id},
        utility::codec::field{"trade_id", &trade_authorisation_request_write::trade_id},
        utility::codec::field{"trade_version", &trade_authorisation_request_write::trade_version},
    };
}

/**
 * @brief The members of trade_authorisation_request_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_change&) {
    return std::tuple{
        utility::codec::field{"write", &trade_authorisation_request_change::write},
        utility::codec::field{"precondition", &trade_authorisation_request_change::precondition},
    };
}

/**
 * @brief The members of trade_authorisation_request_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_removal&) {
    return std::tuple{
        utility::codec::field{"key", &trade_authorisation_request_removal::key},
        utility::codec::field{"precondition", &trade_authorisation_request_removal::precondition},
    };
}

/**
 * @brief The members of trade_authorisation_request_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_authorisation_request_lookup::key},
        utility::codec::field{"trade_authorisation_request",
                              &trade_authorisation_request_lookup::trade_authorisation_request},
    };
}

/**
 * @brief The members of trade_authorisation_requests_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_requests_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &trade_authorisation_requests_filter::trade_id},
        utility::codec::field{"request_id_one_of",
                              &trade_authorisation_requests_filter::request_id_one_of},
        utility::codec::field{"trade_id_one_of",
                              &trade_authorisation_requests_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of trade_authorisation_request_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_authorisation_request_event::event_id},
        utility::codec::field{"key", &trade_authorisation_request_event::key},
        utility::codec::field{"action", &trade_authorisation_request_event::action},
        utility::codec::field{"version", &trade_authorisation_request_event::version},
        utility::codec::field{"occurred_at", &trade_authorisation_request_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_authorisation_request_event::correlation_id},
    };
}

/**
 * @brief The members of trade_authorisation_request_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_version_key&) {
    return std::tuple{
        utility::codec::field{
            "trade_authorisation_request",
            &trade_authorisation_request_version_key::trade_authorisation_request},
        utility::codec::field{"version", &trade_authorisation_request_version_key::version},
    };
}

/**
 * @brief The members of trade_authorisation_request_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_authorisation_request_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &trade_authorisation_request_versions_filter::version},
        utility::codec::field{"from_version",
                              &trade_authorisation_request_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &trade_authorisation_request_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_authorisation_requests_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_authorisation_requests_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_authorisation_requests_request::offset},
        utility::codec::field{"limit", &list_trade_authorisation_requests_request::limit},
        utility::codec::field{"order", &list_trade_authorisation_requests_request::order},
        utility::codec::field{"filter", &list_trade_authorisation_requests_request::filter},
        utility::codec::field{"as_of", &list_trade_authorisation_requests_request::as_of},
    };
}

/**
 * @brief The members of list_trade_authorisation_requests_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_authorisation_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_authorisation_requests_response::result},
        utility::codec::field{
            "trade_authorisation_requests",
            &list_trade_authorisation_requests_response::trade_authorisation_requests},
        utility::codec::field{"total", &list_trade_authorisation_requests_response::total},
    };
}

/**
 * @brief The members of get_trade_authorisation_request_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_authorisation_request_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_authorisation_request_request::key},
    };
}

/**
 * @brief The members of get_trade_authorisation_request_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_authorisation_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_authorisation_request_response::result},
        utility::codec::field{
            "trade_authorisation_request",
            &get_trade_authorisation_request_response::trade_authorisation_request},
    };
}

/**
 * @brief The members of get_many_trade_authorisation_requests_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_authorisation_requests_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_authorisation_requests_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_authorisation_requests_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_authorisation_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_authorisation_requests_response::result},
        utility::codec::field{"entries", &get_many_trade_authorisation_requests_response::entries},
    };
}

/**
 * @brief The members of put_trade_authorisation_request_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_authorisation_request_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_authorisation_request_request::change},
        utility::codec::field{"intent", &put_trade_authorisation_request_request::intent},
    };
}

/**
 * @brief The members of put_trade_authorisation_request_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_authorisation_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_authorisation_request_response::result},
        utility::codec::field{
            "trade_authorisation_request",
            &put_trade_authorisation_request_response::trade_authorisation_request},
    };
}

/**
 * @brief The members of put_many_trade_authorisation_requests_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_authorisation_requests_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_authorisation_requests_request::changes},
        utility::codec::field{"intent", &put_many_trade_authorisation_requests_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_authorisation_requests_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_authorisation_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_authorisation_requests_response::result},
        utility::codec::field{
            "trade_authorisation_requests",
            &put_many_trade_authorisation_requests_response::trade_authorisation_requests},
    };
}

/**
 * @brief The members of delete_trade_authorisation_request_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_authorisation_request_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_authorisation_request_request::removal},
        utility::codec::field{"intent", &delete_trade_authorisation_request_request::intent},
    };
}

/**
 * @brief The members of delete_trade_authorisation_request_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_authorisation_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_authorisation_request_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_authorisation_requests_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_authorisation_requests_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_trade_authorisation_requests_request::removals},
        utility::codec::field{"intent", &delete_many_trade_authorisation_requests_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_authorisation_requests_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_authorisation_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_authorisation_requests_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_authorisation_requests_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_trade_id_trade_authorisation_requests_request&) {
    return std::tuple{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_trade_authorisation_requests_request::trade_id},
        utility::codec::field{"scope",
                              &list_by_trade_id_trade_authorisation_requests_request::scope},
        utility::codec::field{"offset",
                              &list_by_trade_id_trade_authorisation_requests_request::offset},
        utility::codec::field{"limit",
                              &list_by_trade_id_trade_authorisation_requests_request::limit},
        utility::codec::field{"order",
                              &list_by_trade_id_trade_authorisation_requests_request::order},
        utility::codec::field{"filter",
                              &list_by_trade_id_trade_authorisation_requests_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_trade_authorisation_requests_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_trade_id_trade_authorisation_requests_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_trade_id_trade_authorisation_requests_response::result},
        utility::codec::field{
            "trade_authorisation_requests",
            &list_by_trade_id_trade_authorisation_requests_response::trade_authorisation_requests},
        utility::codec::field{"total",
                              &list_by_trade_id_trade_authorisation_requests_response::total},
    };
}

/**
 * @brief The members of list_trade_authorisation_request_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_authorisation_request_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_authorisation_request_versions_request::key},
        utility::codec::field{"offset", &list_trade_authorisation_request_versions_request::offset},
        utility::codec::field{"limit", &list_trade_authorisation_request_versions_request::limit},
        utility::codec::field{"order", &list_trade_authorisation_request_versions_request::order},
        utility::codec::field{"filter", &list_trade_authorisation_request_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_authorisation_request_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_trade_authorisation_request_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_trade_authorisation_request_versions_response::result},
        utility::codec::field{"versions",
                              &list_trade_authorisation_request_versions_response::versions},
        utility::codec::field{"total", &list_trade_authorisation_request_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_authorisation_request_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_authorisation_request_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_authorisation_request_version_request::key},
    };
}

/**
 * @brief The members of get_trade_authorisation_request_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_authorisation_request_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_authorisation_request_version_response::result},
        utility::codec::field{"version",
                              &get_trade_authorisation_request_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_key& v);
    static ores::trading::messaging::trade_authorisation_request_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_key& v);
    static ores::trading::messaging::trade_authorisation_request_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_write& v);
    static ores::trading::messaging::trade_authorisation_request_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_write& v);
    static ores::trading::messaging::trade_authorisation_request_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_change& v);
    static ores::trading::messaging::trade_authorisation_request_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_change& v);
    static ores::trading::messaging::trade_authorisation_request_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_removal& v);
    static ores::trading::messaging::trade_authorisation_request_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_removal& v);
    static ores::trading::messaging::trade_authorisation_request_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_lookup& v);
    static ores::trading::messaging::trade_authorisation_request_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_lookup& v);
    static ores::trading::messaging::trade_authorisation_request_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_requests_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_requests_filter& v);
    static ores::trading::messaging::trade_authorisation_requests_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_requests_filter& v);
    static ores::trading::messaging::trade_authorisation_requests_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_event& v);
    static ores::trading::messaging::trade_authorisation_request_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_event& v);
    static ores::trading::messaging::trade_authorisation_request_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_version_key& v);
    static ores::trading::messaging::trade_authorisation_request_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_version_key& v);
    static ores::trading::messaging::trade_authorisation_request_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::trade_authorisation_request_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::trade_authorisation_request_versions_filter& v);
    static ores::trading::messaging::trade_authorisation_request_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::trade_authorisation_request_versions_filter& v);
    static ores::trading::messaging::trade_authorisation_request_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_authorisation_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_authorisation_requests_request& v);
    static ores::trading::messaging::list_trade_authorisation_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_authorisation_requests_request& v);
    static ores::trading::messaging::list_trade_authorisation_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_authorisation_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_trade_authorisation_requests_response& v);
    static ores::trading::messaging::list_trade_authorisation_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_trade_authorisation_requests_response& v);
    static ores::trading::messaging::list_trade_authorisation_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_authorisation_request_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_authorisation_request_request& v);
    static ores::trading::messaging::get_trade_authorisation_request_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_authorisation_request_request& v);
    static ores::trading::messaging::get_trade_authorisation_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_authorisation_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_authorisation_request_response& v);
    static ores::trading::messaging::get_trade_authorisation_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_trade_authorisation_request_response& v);
    static ores::trading::messaging::get_trade_authorisation_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_authorisation_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::get_many_trade_authorisation_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::get_many_trade_authorisation_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_trade_authorisation_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::get_many_trade_authorisation_requests_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::get_many_trade_authorisation_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_trade_authorisation_request_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_trade_authorisation_request_request& v);
    static ores::trading::messaging::put_trade_authorisation_request_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_authorisation_request_request& v);
    static ores::trading::messaging::put_trade_authorisation_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_trade_authorisation_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_trade_authorisation_request_response& v);
    static ores::trading::messaging::put_trade_authorisation_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_trade_authorisation_request_response& v);
    static ores::trading::messaging::put_trade_authorisation_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_authorisation_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::put_many_trade_authorisation_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::put_many_trade_authorisation_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_trade_authorisation_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::put_many_trade_authorisation_requests_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::put_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::put_many_trade_authorisation_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_authorisation_request_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_authorisation_request_request& v);
    static ores::trading::messaging::delete_trade_authorisation_request_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_authorisation_request_request& v);
    static ores::trading::messaging::delete_trade_authorisation_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_trade_authorisation_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_trade_authorisation_request_response& v);
    static ores::trading::messaging::delete_trade_authorisation_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_trade_authorisation_request_response& v);
    static ores::trading::messaging::delete_trade_authorisation_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_authorisation_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::delete_many_trade_authorisation_requests_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_trade_authorisation_requests_request& v);
    static ores::trading::messaging::delete_many_trade_authorisation_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_trade_authorisation_requests_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::delete_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::delete_many_trade_authorisation_requests_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_trade_authorisation_requests_response& v);
    static ores::trading::messaging::delete_many_trade_authorisation_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_request& v);
    static ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_response& v);
    static ores::trading::messaging::list_by_trade_id_trade_authorisation_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_authorisation_request_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_trade_authorisation_request_versions_request& v);
    static ores::trading::messaging::list_trade_authorisation_request_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_trade_authorisation_request_versions_request& v);
    static ores::trading::messaging::list_trade_authorisation_request_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_trade_authorisation_request_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_trade_authorisation_request_versions_response& v);
    static ores::trading::messaging::list_trade_authorisation_request_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_trade_authorisation_request_versions_response& v);
    static ores::trading::messaging::list_trade_authorisation_request_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_authorisation_request_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_authorisation_request_version_request& v);
    static ores::trading::messaging::get_trade_authorisation_request_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_trade_authorisation_request_version_request& v);
    static ores::trading::messaging::get_trade_authorisation_request_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_trade_authorisation_request_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_trade_authorisation_request_version_response& v);
    static ores::trading::messaging::get_trade_authorisation_request_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_trade_authorisation_request_version_response& v);
    static ores::trading::messaging::get_trade_authorisation_request_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
