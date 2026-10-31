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
#ifndef ORES_TRADING_API_MESSAGING_BOND_FORWARD_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_FORWARD_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_forward.hpp"
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

struct bond_forward_key {
    boost::uuids::uuid trade_id;
};

struct bond_forward_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::optional<std::string> long_in_forward;
    std::optional<std::string> forward_maturity_date;
    std::optional<std::string> forward_settlement_date;
    std::optional<std::string> settlement;
    std::optional<ores::utility::decimal::decimal> amount;
    std::optional<ores::utility::decimal::decimal> lock_rate;
    std::optional<ores::utility::decimal::decimal> dv01;
    std::optional<std::string> lock_rate_day_counter;
    std::optional<std::string> settlement_dirty;
    std::optional<ores::utility::decimal::decimal> premium_amount;
    std::optional<std::string> premium_date;
};

struct bond_forward_change {
    bond_forward_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_forward_removal {
    bond_forward_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_forward_lookup {
    bond_forward_key key;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct bond_forwards_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct bond_forward_event {
    boost::uuids::uuid event_id;
    bond_forward_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_forward_version_key {
    bond_forward_key bond_forward;
    std::uint32_t version;
};

struct bond_forward_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_forwards_request {
    using response_type = struct list_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.list";
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
    std::optional<bond_forwards_filter> filter;
    std::optional<std::string> as_of;
};

struct list_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> bond_forwards;
    std::uint64_t total;
};

struct get_bond_forward_request {
    using response_type = struct get_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_key key;
};

struct get_bond_forward_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct get_many_bond_forwards_request {
    using response_type = struct get_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_key> keys;
};

struct get_many_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<bond_forward_lookup> entries;
};

struct put_bond_forward_request {
    using response_type = struct put_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_forward_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> bond_forward;
};

struct put_many_bond_forwards_request {
    using response_type = struct put_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_forwards_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> bond_forwards;
};

struct delete_bond_forward_request {
    using response_type = struct delete_bond_forward_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_forward_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_forwards_request {
    using response_type = struct delete_many_bond_forwards_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_forward_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_forwards_response {
    ores::utility::domain::result result;
};

struct list_bond_forward_versions_request {
    using response_type = struct list_bond_forward_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_forward_versions_filter> filter;
};

struct list_bond_forward_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_forward> versions;
    std::uint64_t total;
};

struct get_bond_forward_version_request {
    using response_type = struct get_bond_forward_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_forwards_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_forward_version_key key;
};

struct get_bond_forward_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_forward> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_forward_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_forwards_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_forwards_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_forwards_events.deleted";
}
/**
 * @brief The members of bond_forward_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_forward_key::trade_id},
    };
}

/**
 * @brief The members of bond_forward_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &bond_forward_write::trade_id},
        utility::codec::field{"trade_activity_id", &bond_forward_write::trade_activity_id},
        utility::codec::field{"long_in_forward", &bond_forward_write::long_in_forward},
        utility::codec::field{"forward_maturity_date", &bond_forward_write::forward_maturity_date},
        utility::codec::field{"forward_settlement_date",
                              &bond_forward_write::forward_settlement_date},
        utility::codec::field{"settlement", &bond_forward_write::settlement},
        utility::codec::field{"amount", &bond_forward_write::amount},
        utility::codec::field{"lock_rate", &bond_forward_write::lock_rate},
        utility::codec::field{"dv01", &bond_forward_write::dv01},
        utility::codec::field{"lock_rate_day_counter", &bond_forward_write::lock_rate_day_counter},
        utility::codec::field{"settlement_dirty", &bond_forward_write::settlement_dirty},
        utility::codec::field{"premium_amount", &bond_forward_write::premium_amount},
        utility::codec::field{"premium_date", &bond_forward_write::premium_date},
    };
}

/**
 * @brief The members of bond_forward_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_forward_change::write},
        utility::codec::field{"precondition", &bond_forward_change::precondition},
    };
}

/**
 * @brief The members of bond_forward_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_forward_removal::key},
        utility::codec::field{"precondition", &bond_forward_removal::precondition},
    };
}

/**
 * @brief The members of bond_forward_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_forward_lookup::key},
        utility::codec::field{"bond_forward", &bond_forward_lookup::bond_forward},
    };
}

/**
 * @brief The members of bond_forwards_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forwards_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &bond_forwards_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of bond_forward_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_forward_event::event_id},
        utility::codec::field{"key", &bond_forward_event::key},
        utility::codec::field{"action", &bond_forward_event::action},
        utility::codec::field{"version", &bond_forward_event::version},
        utility::codec::field{"occurred_at", &bond_forward_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_forward_event::correlation_id},
    };
}

/**
 * @brief The members of bond_forward_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_forward", &bond_forward_version_key::bond_forward},
        utility::codec::field{"version", &bond_forward_version_key::version},
    };
}

/**
 * @brief The members of bond_forward_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_forward_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_forward_versions_filter::version},
        utility::codec::field{"from_version", &bond_forward_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_forward_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_forwards_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_forwards_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_forwards_request::offset},
        utility::codec::field{"limit", &list_bond_forwards_request::limit},
        utility::codec::field{"order", &list_bond_forwards_request::order},
        utility::codec::field{"filter", &list_bond_forwards_request::filter},
        utility::codec::field{"as_of", &list_bond_forwards_request::as_of},
    };
}

/**
 * @brief The members of list_bond_forwards_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_forwards_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_forwards_response::result},
        utility::codec::field{"bond_forwards", &list_bond_forwards_response::bond_forwards},
        utility::codec::field{"total", &list_bond_forwards_response::total},
    };
}

/**
 * @brief The members of get_bond_forward_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_forward_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_forward_request::key},
    };
}

/**
 * @brief The members of get_bond_forward_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_forward_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_forward_response::result},
        utility::codec::field{"bond_forward", &get_bond_forward_response::bond_forward},
    };
}

/**
 * @brief The members of get_many_bond_forwards_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_forwards_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_forwards_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_forwards_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_forwards_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_forwards_response::result},
        utility::codec::field{"entries", &get_many_bond_forwards_response::entries},
    };
}

/**
 * @brief The members of put_bond_forward_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_forward_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_forward_request::change},
        utility::codec::field{"intent", &put_bond_forward_request::intent},
    };
}

/**
 * @brief The members of put_bond_forward_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_forward_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_forward_response::result},
        utility::codec::field{"bond_forward", &put_bond_forward_response::bond_forward},
    };
}

/**
 * @brief The members of put_many_bond_forwards_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_forwards_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_forwards_request::changes},
        utility::codec::field{"intent", &put_many_bond_forwards_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_forwards_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_forwards_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_forwards_response::result},
        utility::codec::field{"bond_forwards", &put_many_bond_forwards_response::bond_forwards},
    };
}

/**
 * @brief The members of delete_bond_forward_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_forward_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_forward_request::removal},
        utility::codec::field{"intent", &delete_bond_forward_request::intent},
    };
}

/**
 * @brief The members of delete_bond_forward_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_forward_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_forward_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_forwards_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_forwards_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_forwards_request::removals},
        utility::codec::field{"intent", &delete_many_bond_forwards_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_forwards_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_forwards_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_forwards_response::result},
    };
}

/**
 * @brief The members of list_bond_forward_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_forward_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_forward_versions_request::key},
        utility::codec::field{"offset", &list_bond_forward_versions_request::offset},
        utility::codec::field{"limit", &list_bond_forward_versions_request::limit},
        utility::codec::field{"order", &list_bond_forward_versions_request::order},
        utility::codec::field{"filter", &list_bond_forward_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_forward_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_forward_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_forward_versions_response::result},
        utility::codec::field{"versions", &list_bond_forward_versions_response::versions},
        utility::codec::field{"total", &list_bond_forward_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_forward_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_forward_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_forward_version_request::key},
    };
}

/**
 * @brief The members of get_bond_forward_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_forward_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_forward_version_response::result},
        utility::codec::field{"version", &get_bond_forward_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_key& v);
    static ores::trading::messaging::bond_forward_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_key& v);
    static ores::trading::messaging::bond_forward_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_write& v);
    static ores::trading::messaging::bond_forward_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_write& v);
    static ores::trading::messaging::bond_forward_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_change& v);
    static ores::trading::messaging::bond_forward_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_change& v);
    static ores::trading::messaging::bond_forward_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_removal& v);
    static ores::trading::messaging::bond_forward_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_removal& v);
    static ores::trading::messaging::bond_forward_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_lookup& v);
    static ores::trading::messaging::bond_forward_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_lookup& v);
    static ores::trading::messaging::bond_forward_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forwards_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forwards_filter& v);
    static ores::trading::messaging::bond_forwards_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forwards_filter& v);
    static ores::trading::messaging::bond_forwards_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_event& v);
    static ores::trading::messaging::bond_forward_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_event& v);
    static ores::trading::messaging::bond_forward_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_version_key& v);
    static ores::trading::messaging::bond_forward_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_forward_version_key& v);
    static ores::trading::messaging::bond_forward_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_forward_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_forward_versions_filter& v);
    static ores::trading::messaging::bond_forward_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_forward_versions_filter& v);
    static ores::trading::messaging::bond_forward_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_forwards_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_forwards_request& v);
    static ores::trading::messaging::list_bond_forwards_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_bond_forwards_request& v);
    static ores::trading::messaging::list_bond_forwards_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_bond_forwards_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_bond_forwards_response& v);
    static ores::trading::messaging::list_bond_forwards_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_forwards_response& v);
    static ores::trading::messaging::list_bond_forwards_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_forward_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_forward_request& v);
    static ores::trading::messaging::get_bond_forward_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_forward_request& v);
    static ores::trading::messaging::get_bond_forward_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_bond_forward_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_bond_forward_response& v);
    static ores::trading::messaging::get_bond_forward_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_bond_forward_response& v);
    static ores::trading::messaging::get_bond_forward_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_forwards_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_forwards_request& v);
    static ores::trading::messaging::get_many_bond_forwards_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_forwards_request& v);
    static ores::trading::messaging::get_many_bond_forwards_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_forwards_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_forwards_response& v);
    static ores::trading::messaging::get_many_bond_forwards_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_forwards_response& v);
    static ores::trading::messaging::get_many_bond_forwards_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_forward_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_forward_request& v);
    static ores::trading::messaging::put_bond_forward_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_forward_request& v);
    static ores::trading::messaging::put_bond_forward_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_bond_forward_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_bond_forward_response& v);
    static ores::trading::messaging::put_bond_forward_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_bond_forward_response& v);
    static ores::trading::messaging::put_bond_forward_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_forwards_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_forwards_request& v);
    static ores::trading::messaging::put_many_bond_forwards_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_forwards_request& v);
    static ores::trading::messaging::put_many_bond_forwards_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_forwards_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_forwards_response& v);
    static ores::trading::messaging::put_many_bond_forwards_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_forwards_response& v);
    static ores::trading::messaging::put_many_bond_forwards_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_forward_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_forward_request& v);
    static ores::trading::messaging::delete_bond_forward_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_forward_request& v);
    static ores::trading::messaging::delete_bond_forward_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_bond_forward_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_bond_forward_response& v);
    static ores::trading::messaging::delete_bond_forward_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_forward_response& v);
    static ores::trading::messaging::delete_bond_forward_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_forwards_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_forwards_request& v);
    static ores::trading::messaging::delete_many_bond_forwards_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_forwards_request& v);
    static ores::trading::messaging::delete_many_bond_forwards_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_forwards_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_forwards_response& v);
    static ores::trading::messaging::delete_many_bond_forwards_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_forwards_response& v);
    static ores::trading::messaging::delete_many_bond_forwards_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_forward_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_forward_versions_request& v);
    static ores::trading::messaging::list_bond_forward_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_forward_versions_request& v);
    static ores::trading::messaging::list_bond_forward_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_forward_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_forward_versions_response& v);
    static ores::trading::messaging::list_bond_forward_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_forward_versions_response& v);
    static ores::trading::messaging::list_bond_forward_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_forward_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_forward_version_request& v);
    static ores::trading::messaging::get_bond_forward_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_forward_version_request& v);
    static ores::trading::messaging::get_bond_forward_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_forward_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_forward_version_response& v);
    static ores::trading::messaging::get_bond_forward_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_forward_version_response& v);
    static ores::trading::messaging::get_bond_forward_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
