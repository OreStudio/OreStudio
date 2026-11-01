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
#ifndef ORES_TRADING_API_MESSAGING_BALANCE_GUARANTEED_SWAP_TRANCHE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BALANCE_GUARANTEED_SWAP_TRANCHE_PROTOCOL_HPP

#include "ores.trading.api/domain/balance_guaranteed_swap_tranche.hpp"
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

struct balance_guaranteed_swap_tranche_key {
    boost::uuids::uuid trade_id;
    int sequence_number;
};

struct balance_guaranteed_swap_tranche_write {
    boost::uuids::uuid trade_id;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::optional<std::string> description;
    std::string security_id;
    std::int64_t seniority;
};

struct balance_guaranteed_swap_tranche_change {
    balance_guaranteed_swap_tranche_write write;
    ores::utility::domain::precondition precondition;
};

struct balance_guaranteed_swap_tranche_removal {
    balance_guaranteed_swap_tranche_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct balance_guaranteed_swap_tranche_lookup {
    balance_guaranteed_swap_tranche_key key;
    std::optional<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranche;
};

struct balance_guaranteed_swap_tranches_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct balance_guaranteed_swap_tranche_event {
    boost::uuids::uuid event_id;
    balance_guaranteed_swap_tranche_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct balance_guaranteed_swap_tranche_version_key {
    balance_guaranteed_swap_tranche_key balance_guaranteed_swap_tranche;
    std::uint32_t version;
};

struct balance_guaranteed_swap_tranche_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_balance_guaranteed_swap_tranches_request {
    using response_type = struct list_balance_guaranteed_swap_tranches_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.list";
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
    std::optional<balance_guaranteed_swap_tranches_filter> filter;
    std::optional<std::string> as_of;
};

struct list_balance_guaranteed_swap_tranches_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranches;
    std::uint64_t total;
};

struct get_balance_guaranteed_swap_tranche_request {
    using response_type = struct get_balance_guaranteed_swap_tranche_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    balance_guaranteed_swap_tranche_key key;
};

struct get_balance_guaranteed_swap_tranche_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranche;
};

struct get_many_balance_guaranteed_swap_tranches_request {
    using response_type = struct get_many_balance_guaranteed_swap_tranches_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<balance_guaranteed_swap_tranche_key> keys;
};

struct get_many_balance_guaranteed_swap_tranches_response {
    ores::utility::domain::result result;
    std::vector<balance_guaranteed_swap_tranche_lookup> entries;
};

struct put_balance_guaranteed_swap_tranche_request {
    using response_type = struct put_balance_guaranteed_swap_tranche_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    balance_guaranteed_swap_tranche_change change;
    ores::utility::domain::change_intent intent;
};

struct put_balance_guaranteed_swap_tranche_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranche;
};

struct put_many_balance_guaranteed_swap_tranches_request {
    using response_type = struct put_many_balance_guaranteed_swap_tranches_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<balance_guaranteed_swap_tranche_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_balance_guaranteed_swap_tranches_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranches;
};

struct delete_balance_guaranteed_swap_tranche_request {
    using response_type = struct delete_balance_guaranteed_swap_tranche_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    balance_guaranteed_swap_tranche_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_balance_guaranteed_swap_tranche_response {
    ores::utility::domain::result result;
};

struct delete_many_balance_guaranteed_swap_tranches_request {
    using response_type = struct delete_many_balance_guaranteed_swap_tranches_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<balance_guaranteed_swap_tranche_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_balance_guaranteed_swap_tranches_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_balance_guaranteed_swap_tranches_request {
    using response_type = struct list_by_trade_id_balance_guaranteed_swap_tranches_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches.list_by_trade_id";
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
    std::optional<balance_guaranteed_swap_tranches_filter> filter;
};

struct list_by_trade_id_balance_guaranteed_swap_tranches_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::balance_guaranteed_swap_tranche>
        balance_guaranteed_swap_tranches;
    std::uint64_t total;
};

struct list_balance_guaranteed_swap_tranche_versions_request {
    using response_type = struct list_balance_guaranteed_swap_tranche_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    balance_guaranteed_swap_tranche_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<balance_guaranteed_swap_tranche_versions_filter> filter;
};

struct list_balance_guaranteed_swap_tranche_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::balance_guaranteed_swap_tranche> versions;
    std::uint64_t total;
};

struct get_balance_guaranteed_swap_tranche_version_request {
    using response_type = struct get_balance_guaranteed_swap_tranche_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.balance_guaranteed_swap_tranches_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    balance_guaranteed_swap_tranche_version_key key;
};

struct get_balance_guaranteed_swap_tranche_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::balance_guaranteed_swap_tranche> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace balance_guaranteed_swap_tranche_event_subjects {
inline constexpr std::string_view created =
    "trading.v1.balance_guaranteed_swap_tranches_events.created";
inline constexpr std::string_view updated =
    "trading.v1.balance_guaranteed_swap_tranches_events.updated";
inline constexpr std::string_view deleted =
    "trading.v1.balance_guaranteed_swap_tranches_events.deleted";
}
/**
 * @brief The members of balance_guaranteed_swap_tranche_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &balance_guaranteed_swap_tranche_key::trade_id},
        utility::codec::field{"sequence_number",
                              &balance_guaranteed_swap_tranche_key::sequence_number},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &balance_guaranteed_swap_tranche_write::trade_id},
        utility::codec::field{"sequence_number",
                              &balance_guaranteed_swap_tranche_write::sequence_number},
        utility::codec::field{"trade_activity_id",
                              &balance_guaranteed_swap_tranche_write::trade_activity_id},
        utility::codec::field{"description", &balance_guaranteed_swap_tranche_write::description},
        utility::codec::field{"security_id", &balance_guaranteed_swap_tranche_write::security_id},
        utility::codec::field{"seniority", &balance_guaranteed_swap_tranche_write::seniority},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_change&) {
    return std::tuple{
        utility::codec::field{"write", &balance_guaranteed_swap_tranche_change::write},
        utility::codec::field{"precondition",
                              &balance_guaranteed_swap_tranche_change::precondition},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_removal&) {
    return std::tuple{
        utility::codec::field{"key", &balance_guaranteed_swap_tranche_removal::key},
        utility::codec::field{"precondition",
                              &balance_guaranteed_swap_tranche_removal::precondition},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &balance_guaranteed_swap_tranche_lookup::key},
        utility::codec::field{
            "balance_guaranteed_swap_tranche",
            &balance_guaranteed_swap_tranche_lookup::balance_guaranteed_swap_tranche},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranches_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranches_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &balance_guaranteed_swap_tranches_filter::trade_id},
        utility::codec::field{"trade_id_one_of",
                              &balance_guaranteed_swap_tranches_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &balance_guaranteed_swap_tranche_event::event_id},
        utility::codec::field{"key", &balance_guaranteed_swap_tranche_event::key},
        utility::codec::field{"action", &balance_guaranteed_swap_tranche_event::action},
        utility::codec::field{"version", &balance_guaranteed_swap_tranche_event::version},
        utility::codec::field{"occurred_at", &balance_guaranteed_swap_tranche_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &balance_guaranteed_swap_tranche_event::correlation_id},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_version_key&) {
    return std::tuple{
        utility::codec::field{
            "balance_guaranteed_swap_tranche",
            &balance_guaranteed_swap_tranche_version_key::balance_guaranteed_swap_tranche},
        utility::codec::field{"version", &balance_guaranteed_swap_tranche_version_key::version},
    };
}

/**
 * @brief The members of balance_guaranteed_swap_tranche_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const balance_guaranteed_swap_tranche_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &balance_guaranteed_swap_tranche_versions_filter::version},
        utility::codec::field{"from_version",
                              &balance_guaranteed_swap_tranche_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &balance_guaranteed_swap_tranche_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_balance_guaranteed_swap_tranches_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_balance_guaranteed_swap_tranches_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_balance_guaranteed_swap_tranches_request::offset},
        utility::codec::field{"limit", &list_balance_guaranteed_swap_tranches_request::limit},
        utility::codec::field{"order", &list_balance_guaranteed_swap_tranches_request::order},
        utility::codec::field{"filter", &list_balance_guaranteed_swap_tranches_request::filter},
        utility::codec::field{"as_of", &list_balance_guaranteed_swap_tranches_request::as_of},
    };
}

/**
 * @brief The members of list_balance_guaranteed_swap_tranches_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_balance_guaranteed_swap_tranches_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_balance_guaranteed_swap_tranches_response::result},
        utility::codec::field{
            "balance_guaranteed_swap_tranches",
            &list_balance_guaranteed_swap_tranches_response::balance_guaranteed_swap_tranches},
        utility::codec::field{"total", &list_balance_guaranteed_swap_tranches_response::total},
    };
}

/**
 * @brief The members of get_balance_guaranteed_swap_tranche_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_balance_guaranteed_swap_tranche_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_balance_guaranteed_swap_tranche_request::key},
    };
}

/**
 * @brief The members of get_balance_guaranteed_swap_tranche_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_balance_guaranteed_swap_tranche_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_balance_guaranteed_swap_tranche_response::result},
        utility::codec::field{
            "balance_guaranteed_swap_tranche",
            &get_balance_guaranteed_swap_tranche_response::balance_guaranteed_swap_tranche},
    };
}

/**
 * @brief The members of get_many_balance_guaranteed_swap_tranches_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_balance_guaranteed_swap_tranches_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_balance_guaranteed_swap_tranches_request::keys},
    };
}

/**
 * @brief The members of get_many_balance_guaranteed_swap_tranches_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_balance_guaranteed_swap_tranches_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_balance_guaranteed_swap_tranches_response::result},
        utility::codec::field{"entries",
                              &get_many_balance_guaranteed_swap_tranches_response::entries},
    };
}

/**
 * @brief The members of put_balance_guaranteed_swap_tranche_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_balance_guaranteed_swap_tranche_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_balance_guaranteed_swap_tranche_request::change},
        utility::codec::field{"intent", &put_balance_guaranteed_swap_tranche_request::intent},
    };
}

/**
 * @brief The members of put_balance_guaranteed_swap_tranche_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_balance_guaranteed_swap_tranche_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_balance_guaranteed_swap_tranche_response::result},
        utility::codec::field{
            "balance_guaranteed_swap_tranche",
            &put_balance_guaranteed_swap_tranche_response::balance_guaranteed_swap_tranche},
    };
}

/**
 * @brief The members of put_many_balance_guaranteed_swap_tranches_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_balance_guaranteed_swap_tranches_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_balance_guaranteed_swap_tranches_request::changes},
        utility::codec::field{"intent", &put_many_balance_guaranteed_swap_tranches_request::intent},
    };
}

/**
 * @brief The members of put_many_balance_guaranteed_swap_tranches_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_balance_guaranteed_swap_tranches_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_balance_guaranteed_swap_tranches_response::result},
        utility::codec::field{
            "balance_guaranteed_swap_tranches",
            &put_many_balance_guaranteed_swap_tranches_response::balance_guaranteed_swap_tranches},
    };
}

/**
 * @brief The members of delete_balance_guaranteed_swap_tranche_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_balance_guaranteed_swap_tranche_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_balance_guaranteed_swap_tranche_request::removal},
        utility::codec::field{"intent", &delete_balance_guaranteed_swap_tranche_request::intent},
    };
}

/**
 * @brief The members of delete_balance_guaranteed_swap_tranche_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_balance_guaranteed_swap_tranche_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_balance_guaranteed_swap_tranche_response::result},
    };
}

/**
 * @brief The members of delete_many_balance_guaranteed_swap_tranches_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_balance_guaranteed_swap_tranches_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_balance_guaranteed_swap_tranches_request::removals},
        utility::codec::field{"intent",
                              &delete_many_balance_guaranteed_swap_tranches_request::intent},
    };
}

/**
 * @brief The members of delete_many_balance_guaranteed_swap_tranches_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_balance_guaranteed_swap_tranches_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_balance_guaranteed_swap_tranches_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_balance_guaranteed_swap_tranches_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_trade_id_balance_guaranteed_swap_tranches_request&) {
    return std::tuple{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::trade_id},
        utility::codec::field{"scope",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::scope},
        utility::codec::field{"offset",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::offset},
        utility::codec::field{"limit",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::limit},
        utility::codec::field{"order",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::order},
        utility::codec::field{"filter",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_balance_guaranteed_swap_tranches_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_trade_id_balance_guaranteed_swap_tranches_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_response::result},
        utility::codec::field{"balance_guaranteed_swap_tranches",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_response::
                                  balance_guaranteed_swap_tranches},
        utility::codec::field{"total",
                              &list_by_trade_id_balance_guaranteed_swap_tranches_response::total},
    };
}

/**
 * @brief The members of list_balance_guaranteed_swap_tranche_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_balance_guaranteed_swap_tranche_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_balance_guaranteed_swap_tranche_versions_request::key},
        utility::codec::field{"offset",
                              &list_balance_guaranteed_swap_tranche_versions_request::offset},
        utility::codec::field{"limit",
                              &list_balance_guaranteed_swap_tranche_versions_request::limit},
        utility::codec::field{"order",
                              &list_balance_guaranteed_swap_tranche_versions_request::order},
        utility::codec::field{"filter",
                              &list_balance_guaranteed_swap_tranche_versions_request::filter},
    };
}

/**
 * @brief The members of list_balance_guaranteed_swap_tranche_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_balance_guaranteed_swap_tranche_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_balance_guaranteed_swap_tranche_versions_response::result},
        utility::codec::field{"versions",
                              &list_balance_guaranteed_swap_tranche_versions_response::versions},
        utility::codec::field{"total",
                              &list_balance_guaranteed_swap_tranche_versions_response::total},
    };
}

/**
 * @brief The members of get_balance_guaranteed_swap_tranche_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_balance_guaranteed_swap_tranche_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_balance_guaranteed_swap_tranche_version_request::key},
    };
}

/**
 * @brief The members of get_balance_guaranteed_swap_tranche_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_balance_guaranteed_swap_tranche_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_balance_guaranteed_swap_tranche_version_response::result},
        utility::codec::field{"version",
                              &get_balance_guaranteed_swap_tranche_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_key& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_key& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_write& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_write& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_change& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_change& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_removal& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_removal& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_lookup& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_lookup& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranches_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranches_filter& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranches_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranches_filter& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranches_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_event& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_event& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_version_key& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::balance_guaranteed_swap_tranche_version_key& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::balance_guaranteed_swap_tranche_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::balance_guaranteed_swap_tranche_versions_filter& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::balance_guaranteed_swap_tranche_versions_filter& v);
    static ores::trading::messaging::balance_guaranteed_swap_tranche_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_balance_guaranteed_swap_tranches_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranches_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranches_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_balance_guaranteed_swap_tranches_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranches_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranches_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_balance_guaranteed_swap_tranche_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_balance_guaranteed_swap_tranche_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::get_many_balance_guaranteed_swap_tranches_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_balance_guaranteed_swap_tranche_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::put_balance_guaranteed_swap_tranche_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::put_balance_guaranteed_swap_tranche_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_balance_guaranteed_swap_tranche_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::put_balance_guaranteed_swap_tranche_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::put_balance_guaranteed_swap_tranche_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::put_many_balance_guaranteed_swap_tranches_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_balance_guaranteed_swap_tranche_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::delete_balance_guaranteed_swap_tranche_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_balance_guaranteed_swap_tranche_request& v);
    static ores::trading::messaging::delete_balance_guaranteed_swap_tranche_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_balance_guaranteed_swap_tranche_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::delete_balance_guaranteed_swap_tranche_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_balance_guaranteed_swap_tranche_response& v);
    static ores::trading::messaging::delete_balance_guaranteed_swap_tranche_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_request& v);
    static ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_response& v);
    static ores::trading::messaging::delete_many_balance_guaranteed_swap_tranches_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_request&
            v);
    static ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_request&
            v);
    static ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<
    ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_response&
            v);
    static ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_response&
            v);
    static ores::trading::messaging::list_by_trade_id_balance_guaranteed_swap_tranches_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_request& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_request& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_response& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_response& v);
    static ores::trading::messaging::list_balance_guaranteed_swap_tranche_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_request& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_request& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_response& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_response& v);
    static ores::trading::messaging::get_balance_guaranteed_swap_tranche_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
