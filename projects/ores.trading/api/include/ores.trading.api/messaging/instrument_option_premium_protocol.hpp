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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PREMIUM_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PREMIUM_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_option_premium_codec.hpp"
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

struct instrument_option_premium_key {
    boost::uuids::uuid trade_id;
    int sequence_number{};

    friend bool operator==(const instrument_option_premium_key&,
                           const instrument_option_premium_key&) = default;
};

struct instrument_option_premium_write {
    boost::uuids::uuid trade_id;
    int sequence_number{};
    boost::uuids::uuid trade_activity_id;
    ores::utility::decimal::decimal amount;
    std::string currency;
    std::chrono::year_month_day pay_date;
    bool has_settlement{};
    std::optional<std::string> settlement_pay_currency;
    std::optional<std::string> settlement_fx_index;
    std::optional<std::chrono::year_month_day> settlement_fixing_date;

    friend bool operator==(const instrument_option_premium_write&,
                           const instrument_option_premium_write&) = default;
};

struct instrument_option_premium_change {
    instrument_option_premium_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const instrument_option_premium_change&,
                           const instrument_option_premium_change&) = default;
};

struct instrument_option_premium_removal {
    instrument_option_premium_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const instrument_option_premium_removal&,
                           const instrument_option_premium_removal&) = default;
};

struct instrument_option_premium_lookup {
    instrument_option_premium_key key;
    std::optional<ores::trading::domain::instrument_option_premium> instrument_option_premium;

    friend bool operator==(const instrument_option_premium_lookup&,
                           const instrument_option_premium_lookup&) = default;
};

struct instrument_option_premiums_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const instrument_option_premiums_filter&,
                           const instrument_option_premiums_filter&) = default;
};

struct instrument_option_premium_event {
    boost::uuids::uuid event_id;
    instrument_option_premium_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const instrument_option_premium_event&,
                           const instrument_option_premium_event&) = default;
};

struct instrument_option_premium_version_key {
    instrument_option_premium_key instrument_option_premium;
    std::uint32_t version{};

    friend bool operator==(const instrument_option_premium_version_key&,
                           const instrument_option_premium_version_key&) = default;
};

struct instrument_option_premium_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const instrument_option_premium_versions_filter&,
                           const instrument_option_premium_versions_filter&) = default;
};

struct list_instrument_option_premiums_request {
    using response_type = struct list_instrument_option_premiums_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_option_premiums.list";
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
    std::optional<instrument_option_premiums_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_instrument_option_premiums_request&,
                           const list_instrument_option_premiums_request&) = default;
};

struct list_instrument_option_premiums_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option_premium> option_premiums;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_option_premiums_response&,
                           const list_instrument_option_premiums_response&) = default;
};

struct get_instrument_option_premium_request {
    using response_type = struct get_instrument_option_premium_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_option_premiums.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_premium_key key;

    friend bool operator==(const get_instrument_option_premium_request&,
                           const get_instrument_option_premium_request&) = default;
};

struct get_instrument_option_premium_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option_premium> instrument_option_premium;

    friend bool operator==(const get_instrument_option_premium_response&,
                           const get_instrument_option_premium_response&) = default;
};

struct get_many_instrument_option_premiums_request {
    using response_type = struct get_many_instrument_option_premiums_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_premium_key> keys;

    friend bool operator==(const get_many_instrument_option_premiums_request&,
                           const get_many_instrument_option_premiums_request&) = default;
};

struct get_many_instrument_option_premiums_response {
    ores::utility::domain::result result;
    std::vector<instrument_option_premium_lookup> entries;

    friend bool operator==(const get_many_instrument_option_premiums_response&,
                           const get_many_instrument_option_premiums_response&) = default;
};

struct put_instrument_option_premium_request {
    using response_type = struct put_instrument_option_premium_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_option_premiums.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_premium_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_instrument_option_premium_request&,
                           const put_instrument_option_premium_request&) = default;
};

struct put_instrument_option_premium_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option_premium> instrument_option_premium;

    friend bool operator==(const put_instrument_option_premium_response&,
                           const put_instrument_option_premium_response&) = default;
};

struct put_many_instrument_option_premiums_request {
    using response_type = struct put_many_instrument_option_premiums_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_premium_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_instrument_option_premiums_request&,
                           const put_many_instrument_option_premiums_request&) = default;
};

struct put_many_instrument_option_premiums_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option_premium> option_premiums;

    friend bool operator==(const put_many_instrument_option_premiums_response&,
                           const put_many_instrument_option_premiums_response&) = default;
};

struct delete_instrument_option_premium_request {
    using response_type = struct delete_instrument_option_premium_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_option_premiums.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_premium_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_instrument_option_premium_request&,
                           const delete_instrument_option_premium_request&) = default;
};

struct delete_instrument_option_premium_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_instrument_option_premium_response&,
                           const delete_instrument_option_premium_response&) = default;
};

struct delete_many_instrument_option_premiums_request {
    using response_type = struct delete_many_instrument_option_premiums_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_premium_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_instrument_option_premiums_request&,
                           const delete_many_instrument_option_premiums_request&) = default;
};

struct delete_many_instrument_option_premiums_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_instrument_option_premiums_response&,
                           const delete_many_instrument_option_premiums_response&) = default;
};

struct list_by_trade_id_instrument_option_premiums_request {
    using response_type = struct list_by_trade_id_instrument_option_premiums_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums.list_by_trade_id";
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
    std::optional<instrument_option_premiums_filter> filter;

    friend bool operator==(const list_by_trade_id_instrument_option_premiums_request&,
                           const list_by_trade_id_instrument_option_premiums_request&) = default;
};

struct list_by_trade_id_instrument_option_premiums_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option_premium> option_premiums;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_instrument_option_premiums_response&,
                           const list_by_trade_id_instrument_option_premiums_response&) = default;
};

struct list_instrument_option_premium_versions_request {
    using response_type = struct list_instrument_option_premium_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_premium_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_option_premium_versions_filter> filter;

    friend bool operator==(const list_instrument_option_premium_versions_request&,
                           const list_instrument_option_premium_versions_request&) = default;
};

struct list_instrument_option_premium_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option_premium> versions;
    std::uint64_t total{};

    friend bool operator==(const list_instrument_option_premium_versions_response&,
                           const list_instrument_option_premium_versions_response&) = default;
};

struct get_instrument_option_premium_version_request {
    using response_type = struct get_instrument_option_premium_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_option_premiums_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_premium_version_key key;

    friend bool operator==(const get_instrument_option_premium_version_request&,
                           const get_instrument_option_premium_version_request&) = default;
};

struct get_instrument_option_premium_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option_premium> version;

    friend bool operator==(const get_instrument_option_premium_version_response&,
                           const get_instrument_option_premium_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_option_premium_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.instrument_option_premiums.created";
inline constexpr std::string_view updated = "trading.v1.events.instrument_option_premiums.updated";
inline constexpr std::string_view deleted = "trading.v1.events.instrument_option_premiums.deleted";
}
/**
 * @brief The members of instrument_option_premium_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_option_premium_key::trade_id},
        utility::codec::field{"sequence_number", &instrument_option_premium_key::sequence_number},
    };
}

/**
 * @brief The members of instrument_option_premium_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_option_premium_write::trade_id},
        utility::codec::field{"sequence_number", &instrument_option_premium_write::sequence_number},
        utility::codec::field{"trade_activity_id",
                              &instrument_option_premium_write::trade_activity_id},
        utility::codec::field{"amount", &instrument_option_premium_write::amount},
        utility::codec::field{"currency", &instrument_option_premium_write::currency},
        utility::codec::field{"pay_date", &instrument_option_premium_write::pay_date},
        utility::codec::field{"has_settlement", &instrument_option_premium_write::has_settlement},
        utility::codec::field{"settlement_pay_currency",
                              &instrument_option_premium_write::settlement_pay_currency},
        utility::codec::field{"settlement_fx_index",
                              &instrument_option_premium_write::settlement_fx_index},
        utility::codec::field{"settlement_fixing_date",
                              &instrument_option_premium_write::settlement_fixing_date},
    };
}

/**
 * @brief The members of instrument_option_premium_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &instrument_option_premium_change::write},
        utility::codec::field{"precondition", &instrument_option_premium_change::precondition},
    };
}

/**
 * @brief The members of instrument_option_premium_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &instrument_option_premium_removal::key},
        utility::codec::field{"precondition", &instrument_option_premium_removal::precondition},
    };
}

/**
 * @brief The members of instrument_option_premium_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &instrument_option_premium_lookup::key},
        utility::codec::field{"instrument_option_premium",
                              &instrument_option_premium_lookup::instrument_option_premium},
    };
}

/**
 * @brief The members of instrument_option_premiums_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premiums_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &instrument_option_premiums_filter::trade_id},
        utility::codec::field{"trade_id_one_of",
                              &instrument_option_premiums_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of instrument_option_premium_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<instrument_option_premium_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &instrument_option_premium_event::event_id},
        utility::codec::field{"key", &instrument_option_premium_event::key},
        utility::codec::field{"action", &instrument_option_premium_event::action},
        utility::codec::field{"version", &instrument_option_premium_event::version},
        utility::codec::field{"occurred_at", &instrument_option_premium_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_option_premium_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_option_premium_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<instrument_option_premium_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"instrument_option_premium",
                              &instrument_option_premium_version_key::instrument_option_premium},
        utility::codec::field{"version", &instrument_option_premium_version_key::version},
    };
}

/**
 * @brief The members of instrument_option_premium_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<instrument_option_premium_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &instrument_option_premium_versions_filter::version},
        utility::codec::field{"from_version",
                              &instrument_option_premium_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_option_premium_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_option_premiums_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_option_premiums_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_instrument_option_premiums_request::offset},
        utility::codec::field{"limit", &list_instrument_option_premiums_request::limit},
        utility::codec::field{"order", &list_instrument_option_premiums_request::order},
        utility::codec::field{"filter", &list_instrument_option_premiums_request::filter},
        utility::codec::field{"as_of", &list_instrument_option_premiums_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_option_premiums_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_option_premiums_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_instrument_option_premiums_response::result},
        utility::codec::field{"option_premiums",
                              &list_instrument_option_premiums_response::option_premiums},
        utility::codec::field{"total", &list_instrument_option_premiums_response::total},
    };
}

/**
 * @brief The members of get_instrument_option_premium_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_option_premium_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_instrument_option_premium_request::key},
    };
}

/**
 * @brief The members of get_instrument_option_premium_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_option_premium_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_instrument_option_premium_response::result},
        utility::codec::field{"instrument_option_premium",
                              &get_instrument_option_premium_response::instrument_option_premium},
    };
}

/**
 * @brief The members of get_many_instrument_option_premiums_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_option_premiums_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_instrument_option_premiums_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_option_premiums_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_instrument_option_premiums_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_instrument_option_premiums_response::result},
        utility::codec::field{"entries", &get_many_instrument_option_premiums_response::entries},
    };
}

/**
 * @brief The members of put_instrument_option_premium_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_instrument_option_premium_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_instrument_option_premium_request::change},
        utility::codec::field{"intent", &put_instrument_option_premium_request::intent},
    };
}

/**
 * @brief The members of put_instrument_option_premium_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_instrument_option_premium_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_instrument_option_premium_response::result},
        utility::codec::field{"instrument_option_premium",
                              &put_instrument_option_premium_response::instrument_option_premium},
    };
}

/**
 * @brief The members of put_many_instrument_option_premiums_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_option_premiums_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_instrument_option_premiums_request::changes},
        utility::codec::field{"intent", &put_many_instrument_option_premiums_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_option_premiums_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_instrument_option_premiums_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_instrument_option_premiums_response::result},
        utility::codec::field{"option_premiums",
                              &put_many_instrument_option_premiums_response::option_premiums},
    };
}

/**
 * @brief The members of delete_instrument_option_premium_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_instrument_option_premium_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_instrument_option_premium_request::removal},
        utility::codec::field{"intent", &delete_instrument_option_premium_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_option_premium_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_instrument_option_premium_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_instrument_option_premium_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_option_premiums_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_option_premiums_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals",
                              &delete_many_instrument_option_premiums_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_option_premiums_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_option_premiums_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_instrument_option_premiums_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_instrument_option_premiums_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_option_premiums_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_option_premiums_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_instrument_option_premiums_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_instrument_option_premiums_request::scope},
        utility::codec::field{"offset",
                              &list_by_trade_id_instrument_option_premiums_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_instrument_option_premiums_request::limit},
        utility::codec::field{"order", &list_by_trade_id_instrument_option_premiums_request::order},
        utility::codec::field{"filter",
                              &list_by_trade_id_instrument_option_premiums_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_instrument_option_premiums_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_instrument_option_premiums_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_trade_id_instrument_option_premiums_response::result},
        utility::codec::field{
            "option_premiums",
            &list_by_trade_id_instrument_option_premiums_response::option_premiums},
        utility::codec::field{"total",
                              &list_by_trade_id_instrument_option_premiums_response::total},
    };
}

/**
 * @brief The members of list_instrument_option_premium_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_option_premium_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_instrument_option_premium_versions_request::key},
        utility::codec::field{"offset", &list_instrument_option_premium_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_option_premium_versions_request::limit},
        utility::codec::field{"order", &list_instrument_option_premium_versions_request::order},
        utility::codec::field{"filter", &list_instrument_option_premium_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_option_premium_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_instrument_option_premium_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_instrument_option_premium_versions_response::result},
        utility::codec::field{"versions",
                              &list_instrument_option_premium_versions_response::versions},
        utility::codec::field{"total", &list_instrument_option_premium_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_option_premium_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_option_premium_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_instrument_option_premium_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_option_premium_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_instrument_option_premium_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_instrument_option_premium_version_response::result},
        utility::codec::field{"version", &get_instrument_option_premium_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_premium_key& v);
    static ores::trading::messaging::instrument_option_premium_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_key& v);
    static ores::trading::messaging::instrument_option_premium_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_write& v);
    static ores::trading::messaging::instrument_option_premium_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_write& v);
    static ores::trading::messaging::instrument_option_premium_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_change& v);
    static ores::trading::messaging::instrument_option_premium_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_change& v);
    static ores::trading::messaging::instrument_option_premium_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_removal& v);
    static ores::trading::messaging::instrument_option_premium_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_removal& v);
    static ores::trading::messaging::instrument_option_premium_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_lookup& v);
    static ores::trading::messaging::instrument_option_premium_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_lookup& v);
    static ores::trading::messaging::instrument_option_premium_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premiums_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premiums_filter& v);
    static ores::trading::messaging::instrument_option_premiums_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premiums_filter& v);
    static ores::trading::messaging::instrument_option_premiums_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_event& v);
    static ores::trading::messaging::instrument_option_premium_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_event& v);
    static ores::trading::messaging::instrument_option_premium_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_version_key& v);
    static ores::trading::messaging::instrument_option_premium_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_version_key& v);
    static ores::trading::messaging::instrument_option_premium_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_premium_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_premium_versions_filter& v);
    static ores::trading::messaging::instrument_option_premium_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_premium_versions_filter& v);
    static ores::trading::messaging::instrument_option_premium_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_premiums_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_premiums_request& v);
    static ores::trading::messaging::list_instrument_option_premiums_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_option_premiums_request& v);
    static ores::trading::messaging::list_instrument_option_premiums_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_premiums_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_premiums_response& v);
    static ores::trading::messaging::list_instrument_option_premiums_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_option_premiums_response& v);
    static ores::trading::messaging::list_instrument_option_premiums_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_premium_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_premium_request& v);
    static ores::trading::messaging::get_instrument_option_premium_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_premium_request& v);
    static ores::trading::messaging::get_instrument_option_premium_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_premium_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_premium_response& v);
    static ores::trading::messaging::get_instrument_option_premium_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_premium_response& v);
    static ores::trading::messaging::get_instrument_option_premium_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_option_premiums_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::get_many_instrument_option_premiums_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::get_many_instrument_option_premiums_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_option_premiums_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::get_many_instrument_option_premiums_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::get_many_instrument_option_premiums_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_option_premium_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_option_premium_request& v);
    static ores::trading::messaging::put_instrument_option_premium_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_option_premium_request& v);
    static ores::trading::messaging::put_instrument_option_premium_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_option_premium_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_option_premium_response& v);
    static ores::trading::messaging::put_instrument_option_premium_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_option_premium_response& v);
    static ores::trading::messaging::put_instrument_option_premium_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_option_premiums_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::put_many_instrument_option_premiums_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::put_many_instrument_option_premiums_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_option_premiums_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::put_many_instrument_option_premiums_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::put_many_instrument_option_premiums_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_option_premium_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_option_premium_request& v);
    static ores::trading::messaging::delete_instrument_option_premium_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_option_premium_request& v);
    static ores::trading::messaging::delete_instrument_option_premium_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_option_premium_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_option_premium_response& v);
    static ores::trading::messaging::delete_instrument_option_premium_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_option_premium_response& v);
    static ores::trading::messaging::delete_instrument_option_premium_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_option_premiums_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::delete_many_instrument_option_premiums_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_instrument_option_premiums_request& v);
    static ores::trading::messaging::delete_many_instrument_option_premiums_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_option_premiums_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::delete_many_instrument_option_premiums_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::delete_many_instrument_option_premiums_response& v);
    static ores::trading::messaging::delete_many_instrument_option_premiums_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_premiums_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_option_premiums_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_option_premiums_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_option_premiums_request& v);
    static ores::trading::messaging::list_by_trade_id_instrument_option_premiums_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_premiums_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_option_premiums_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_option_premiums_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_option_premiums_response& v);
    static ores::trading::messaging::list_by_trade_id_instrument_option_premiums_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_premium_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_premium_versions_request& v);
    static ores::trading::messaging::list_instrument_option_premium_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_instrument_option_premium_versions_request& v);
    static ores::trading::messaging::list_instrument_option_premium_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_premium_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_premium_versions_response& v);
    static ores::trading::messaging::list_instrument_option_premium_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_instrument_option_premium_versions_response& v);
    static ores::trading::messaging::list_instrument_option_premium_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_premium_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_premium_version_request& v);
    static ores::trading::messaging::get_instrument_option_premium_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_premium_version_request& v);
    static ores::trading::messaging::get_instrument_option_premium_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_premium_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_premium_version_response& v);
    static ores::trading::messaging::get_instrument_option_premium_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::get_instrument_option_premium_version_response& v);
    static ores::trading::messaging::get_instrument_option_premium_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
