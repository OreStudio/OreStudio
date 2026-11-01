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
#ifndef ORES_TRADING_API_MESSAGING_BOND_LEG_AMORTIZATION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_LEG_AMORTIZATION_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_leg_amortization.hpp"
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

struct bond_leg_amortization_key {
    boost::uuids::uuid trade_id;
    std::string leg_role;
    int leg_number;
    int sequence_number;
};

struct bond_leg_amortization_write {
    boost::uuids::uuid trade_id;
    std::string leg_role;
    int leg_number;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string amortization_type;
    std::optional<ores::utility::decimal::decimal> value;
    std::optional<std::string> start_date;
    std::optional<std::string> end_date;
    std::optional<std::string> frequency;
    std::optional<bool> underflow;
};

struct bond_leg_amortization_change {
    bond_leg_amortization_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_leg_amortization_removal {
    bond_leg_amortization_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_leg_amortization_lookup {
    bond_leg_amortization_key key;
    std::optional<ores::trading::domain::bond_leg_amortization> bond_leg_amortization;
};

struct bond_leg_amortizations_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct bond_leg_amortization_event {
    boost::uuids::uuid event_id;
    bond_leg_amortization_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_leg_amortization_version_key {
    bond_leg_amortization_key bond_leg_amortization;
    std::uint32_t version;
};

struct bond_leg_amortization_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_leg_amortizations_request {
    using response_type = struct list_bond_leg_amortizations_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.list";
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
    std::optional<bond_leg_amortizations_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bond_leg_amortizations_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_amortization> bond_leg_amortizations;
    std::uint64_t total;
};

struct get_bond_leg_amortization_request {
    using response_type = struct get_bond_leg_amortization_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_amortization_key key;
};

struct get_bond_leg_amortization_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_leg_amortization> bond_leg_amortization;
};

struct get_many_bond_leg_amortizations_request {
    using response_type = struct get_many_bond_leg_amortizations_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_amortization_key> keys;
};

struct get_many_bond_leg_amortizations_response {
    ores::utility::domain::result result;
    std::vector<bond_leg_amortization_lookup> entries;
};

struct put_bond_leg_amortization_request {
    using response_type = struct put_bond_leg_amortization_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_amortization_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_leg_amortization_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_leg_amortization> bond_leg_amortization;
};

struct put_many_bond_leg_amortizations_request {
    using response_type = struct put_many_bond_leg_amortizations_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_amortization_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_leg_amortizations_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_amortization> bond_leg_amortizations;
};

struct delete_bond_leg_amortization_request {
    using response_type = struct delete_bond_leg_amortization_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_leg_amortizations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_amortization_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_leg_amortization_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_leg_amortizations_request {
    using response_type = struct delete_many_bond_leg_amortizations_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_leg_amortizations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_leg_amortization_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_leg_amortizations_response {
    ores::utility::domain::result result;
};

struct list_by_trade_id_bond_leg_amortizations_request {
    using response_type = struct list_by_trade_id_bond_leg_amortizations_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_leg_amortizations.list_by_trade_id";
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
    std::optional<bond_leg_amortizations_filter> filter;
};

struct list_by_trade_id_bond_leg_amortizations_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_amortization> bond_leg_amortizations;
    std::uint64_t total;
};

struct list_bond_leg_amortization_versions_request {
    using response_type = struct list_bond_leg_amortization_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_leg_amortizations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_amortization_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_leg_amortization_versions_filter> filter;
};

struct list_bond_leg_amortization_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_leg_amortization> versions;
    std::uint64_t total;
};

struct get_bond_leg_amortization_version_request {
    using response_type = struct get_bond_leg_amortization_version_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_leg_amortizations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_leg_amortization_version_key key;
};

struct get_bond_leg_amortization_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_leg_amortization> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_leg_amortization_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_leg_amortizations_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_leg_amortizations_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_leg_amortizations_events.deleted";
}
/**
 * @brief The members of bond_leg_amortization_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_leg_amortization_key::trade_id},
        utility::codec::field{"leg_role", &bond_leg_amortization_key::leg_role},
        utility::codec::field{"leg_number", &bond_leg_amortization_key::leg_number},
        utility::codec::field{"sequence_number", &bond_leg_amortization_key::sequence_number},
    };
}

/**
 * @brief The members of bond_leg_amortization_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_leg_amortization_write::trade_id},
        utility::codec::field{"leg_role", &bond_leg_amortization_write::leg_role},
        utility::codec::field{"leg_number", &bond_leg_amortization_write::leg_number},
        utility::codec::field{"sequence_number", &bond_leg_amortization_write::sequence_number},
        utility::codec::field{"trade_activity_id", &bond_leg_amortization_write::trade_activity_id},
        utility::codec::field{"amortization_type", &bond_leg_amortization_write::amortization_type},
        utility::codec::field{"value", &bond_leg_amortization_write::value},
        utility::codec::field{"start_date", &bond_leg_amortization_write::start_date},
        utility::codec::field{"end_date", &bond_leg_amortization_write::end_date},
        utility::codec::field{"frequency", &bond_leg_amortization_write::frequency},
        utility::codec::field{"underflow", &bond_leg_amortization_write::underflow},
    };
}

/**
 * @brief The members of bond_leg_amortization_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_leg_amortization_change::write},
        utility::codec::field{"precondition", &bond_leg_amortization_change::precondition},
    };
}

/**
 * @brief The members of bond_leg_amortization_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_leg_amortization_removal::key},
        utility::codec::field{"precondition", &bond_leg_amortization_removal::precondition},
    };
}

/**
 * @brief The members of bond_leg_amortization_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_leg_amortization_lookup::key},
        utility::codec::field{"bond_leg_amortization",
                              &bond_leg_amortization_lookup::bond_leg_amortization},
    };
}

/**
 * @brief The members of bond_leg_amortizations_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortizations_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_leg_amortizations_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &bond_leg_amortizations_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of bond_leg_amortization_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_leg_amortization_event::event_id},
        utility::codec::field{"key", &bond_leg_amortization_event::key},
        utility::codec::field{"action", &bond_leg_amortization_event::action},
        utility::codec::field{"version", &bond_leg_amortization_event::version},
        utility::codec::field{"occurred_at", &bond_leg_amortization_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_leg_amortization_event::correlation_id},
    };
}

/**
 * @brief The members of bond_leg_amortization_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_leg_amortization",
                              &bond_leg_amortization_version_key::bond_leg_amortization},
        utility::codec::field{"version", &bond_leg_amortization_version_key::version},
    };
}

/**
 * @brief The members of bond_leg_amortization_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_leg_amortization_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_leg_amortization_versions_filter::version},
        utility::codec::field{"from_version", &bond_leg_amortization_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_leg_amortization_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_leg_amortizations_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_leg_amortizations_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_leg_amortizations_request::offset},
        utility::codec::field{"limit", &list_bond_leg_amortizations_request::limit},
        utility::codec::field{"order", &list_bond_leg_amortizations_request::order},
        utility::codec::field{"filter", &list_bond_leg_amortizations_request::filter},
        utility::codec::field{"as_of", &list_bond_leg_amortizations_request::as_of},
    };
}

/**
 * @brief The members of list_bond_leg_amortizations_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_leg_amortizations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_leg_amortizations_response::result},
        utility::codec::field{"bond_leg_amortizations",
                              &list_bond_leg_amortizations_response::bond_leg_amortizations},
        utility::codec::field{"total", &list_bond_leg_amortizations_response::total},
    };
}

/**
 * @brief The members of get_bond_leg_amortization_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_leg_amortization_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_leg_amortization_request::key},
    };
}

/**
 * @brief The members of get_bond_leg_amortization_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_leg_amortization_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_leg_amortization_response::result},
        utility::codec::field{"bond_leg_amortization",
                              &get_bond_leg_amortization_response::bond_leg_amortization},
    };
}

/**
 * @brief The members of get_many_bond_leg_amortizations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_leg_amortizations_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_leg_amortizations_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_leg_amortizations_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_leg_amortizations_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_leg_amortizations_response::result},
        utility::codec::field{"entries", &get_many_bond_leg_amortizations_response::entries},
    };
}

/**
 * @brief The members of put_bond_leg_amortization_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_leg_amortization_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_leg_amortization_request::change},
        utility::codec::field{"intent", &put_bond_leg_amortization_request::intent},
    };
}

/**
 * @brief The members of put_bond_leg_amortization_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_leg_amortization_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_leg_amortization_response::result},
        utility::codec::field{"bond_leg_amortization",
                              &put_bond_leg_amortization_response::bond_leg_amortization},
    };
}

/**
 * @brief The members of put_many_bond_leg_amortizations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_leg_amortizations_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_leg_amortizations_request::changes},
        utility::codec::field{"intent", &put_many_bond_leg_amortizations_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_leg_amortizations_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_leg_amortizations_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_leg_amortizations_response::result},
        utility::codec::field{"bond_leg_amortizations",
                              &put_many_bond_leg_amortizations_response::bond_leg_amortizations},
    };
}

/**
 * @brief The members of delete_bond_leg_amortization_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_leg_amortization_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_leg_amortization_request::removal},
        utility::codec::field{"intent", &delete_bond_leg_amortization_request::intent},
    };
}

/**
 * @brief The members of delete_bond_leg_amortization_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_leg_amortization_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_leg_amortization_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_leg_amortizations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_leg_amortizations_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_leg_amortizations_request::removals},
        utility::codec::field{"intent", &delete_many_bond_leg_amortizations_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_leg_amortizations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_leg_amortizations_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_leg_amortizations_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_bond_leg_amortizations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_bond_leg_amortizations_request&) {
    return std::tuple{
        utility::codec::field{"trade_id",
                              &list_by_trade_id_bond_leg_amortizations_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_bond_leg_amortizations_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_bond_leg_amortizations_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_bond_leg_amortizations_request::limit},
        utility::codec::field{"order", &list_by_trade_id_bond_leg_amortizations_request::order},
        utility::codec::field{"filter", &list_by_trade_id_bond_leg_amortizations_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_bond_leg_amortizations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_trade_id_bond_leg_amortizations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_bond_leg_amortizations_response::result},
        utility::codec::field{
            "bond_leg_amortizations",
            &list_by_trade_id_bond_leg_amortizations_response::bond_leg_amortizations},
        utility::codec::field{"total", &list_by_trade_id_bond_leg_amortizations_response::total},
    };
}

/**
 * @brief The members of list_bond_leg_amortization_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_leg_amortization_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_leg_amortization_versions_request::key},
        utility::codec::field{"offset", &list_bond_leg_amortization_versions_request::offset},
        utility::codec::field{"limit", &list_bond_leg_amortization_versions_request::limit},
        utility::codec::field{"order", &list_bond_leg_amortization_versions_request::order},
        utility::codec::field{"filter", &list_bond_leg_amortization_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_leg_amortization_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_leg_amortization_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_leg_amortization_versions_response::result},
        utility::codec::field{"versions", &list_bond_leg_amortization_versions_response::versions},
        utility::codec::field{"total", &list_bond_leg_amortization_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_leg_amortization_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_leg_amortization_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_leg_amortization_version_request::key},
    };
}

/**
 * @brief The members of get_bond_leg_amortization_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_leg_amortization_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_leg_amortization_version_response::result},
        utility::codec::field{"version", &get_bond_leg_amortization_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_leg_amortization_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_key& v);
    static ores::trading::messaging::bond_leg_amortization_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_leg_amortization_key& v);
    static ores::trading::messaging::bond_leg_amortization_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_leg_amortization_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_write& v);
    static ores::trading::messaging::bond_leg_amortization_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_write& v);
    static ores::trading::messaging::bond_leg_amortization_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_leg_amortization_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_change& v);
    static ores::trading::messaging::bond_leg_amortization_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_change& v);
    static ores::trading::messaging::bond_leg_amortization_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_leg_amortization_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_removal& v);
    static ores::trading::messaging::bond_leg_amortization_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_removal& v);
    static ores::trading::messaging::bond_leg_amortization_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_leg_amortization_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_lookup& v);
    static ores::trading::messaging::bond_leg_amortization_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_lookup& v);
    static ores::trading::messaging::bond_leg_amortization_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_leg_amortizations_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortizations_filter& v);
    static ores::trading::messaging::bond_leg_amortizations_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortizations_filter& v);
    static ores::trading::messaging::bond_leg_amortizations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_leg_amortization_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_leg_amortization_event& v);
    static ores::trading::messaging::bond_leg_amortization_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_event& v);
    static ores::trading::messaging::bond_leg_amortization_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_leg_amortization_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::bond_leg_amortization_version_key& v);
    static ores::trading::messaging::bond_leg_amortization_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_version_key& v);
    static ores::trading::messaging::bond_leg_amortization_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_leg_amortization_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::bond_leg_amortization_versions_filter& v);
    static ores::trading::messaging::bond_leg_amortization_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_leg_amortization_versions_filter& v);
    static ores::trading::messaging::bond_leg_amortization_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_leg_amortizations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_leg_amortizations_request& v);
    static ores::trading::messaging::list_bond_leg_amortizations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_leg_amortizations_request& v);
    static ores::trading::messaging::list_bond_leg_amortizations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_leg_amortizations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_leg_amortizations_response& v);
    static ores::trading::messaging::list_bond_leg_amortizations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_leg_amortizations_response& v);
    static ores::trading::messaging::list_bond_leg_amortizations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_leg_amortization_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_leg_amortization_request& v);
    static ores::trading::messaging::get_bond_leg_amortization_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_leg_amortization_request& v);
    static ores::trading::messaging::get_bond_leg_amortization_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_leg_amortization_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_leg_amortization_response& v);
    static ores::trading::messaging::get_bond_leg_amortization_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_leg_amortization_response& v);
    static ores::trading::messaging::get_bond_leg_amortization_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_leg_amortizations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::get_many_bond_leg_amortizations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::get_many_bond_leg_amortizations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_leg_amortizations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::get_many_bond_leg_amortizations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::get_many_bond_leg_amortizations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_leg_amortization_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_leg_amortization_request& v);
    static ores::trading::messaging::put_bond_leg_amortization_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_leg_amortization_request& v);
    static ores::trading::messaging::put_bond_leg_amortization_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_leg_amortization_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_leg_amortization_response& v);
    static ores::trading::messaging::put_bond_leg_amortization_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_leg_amortization_response& v);
    static ores::trading::messaging::put_bond_leg_amortization_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_leg_amortizations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::put_many_bond_leg_amortizations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::put_many_bond_leg_amortizations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_leg_amortizations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::put_many_bond_leg_amortizations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::put_many_bond_leg_amortizations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_bond_leg_amortization_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_bond_leg_amortization_request& v);
    static ores::trading::messaging::delete_bond_leg_amortization_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_leg_amortization_request& v);
    static ores::trading::messaging::delete_bond_leg_amortization_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_bond_leg_amortization_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_bond_leg_amortization_response& v);
    static ores::trading::messaging::delete_bond_leg_amortization_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_leg_amortization_response& v);
    static ores::trading::messaging::delete_bond_leg_amortization_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_leg_amortizations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::delete_many_bond_leg_amortizations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_leg_amortizations_request& v);
    static ores::trading::messaging::delete_many_bond_leg_amortizations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_leg_amortizations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::delete_many_bond_leg_amortizations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_leg_amortizations_response& v);
    static ores::trading::messaging::delete_many_bond_leg_amortizations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_request& v);
    static ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_request& v);
    static ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_response& v);
    static ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_response& v);
    static ores::trading::messaging::list_by_trade_id_bond_leg_amortizations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_leg_amortization_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_leg_amortization_versions_request& v);
    static ores::trading::messaging::list_bond_leg_amortization_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_leg_amortization_versions_request& v);
    static ores::trading::messaging::list_bond_leg_amortization_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_leg_amortization_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_leg_amortization_versions_response& v);
    static ores::trading::messaging::list_bond_leg_amortization_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_leg_amortization_versions_response& v);
    static ores::trading::messaging::list_bond_leg_amortization_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_leg_amortization_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_leg_amortization_version_request& v);
    static ores::trading::messaging::get_bond_leg_amortization_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_leg_amortization_version_request& v);
    static ores::trading::messaging::get_bond_leg_amortization_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_leg_amortization_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_leg_amortization_version_response& v);
    static ores::trading::messaging::get_bond_leg_amortization_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_leg_amortization_version_response& v);
    static ores::trading::messaging::get_bond_leg_amortization_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
