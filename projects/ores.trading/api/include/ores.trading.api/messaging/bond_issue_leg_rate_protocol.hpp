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
#ifndef ORES_TRADING_API_MESSAGING_BOND_ISSUE_LEG_RATE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BOND_ISSUE_LEG_RATE_PROTOCOL_HPP

#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
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

struct bond_issue_leg_rate_key {
    boost::uuids::uuid issue_id;
    int leg_number;
};

struct bond_issue_leg_rate_write {
    boost::uuids::uuid issue_id;
    int leg_number;
    std::string rate_kind;
    std::optional<std::string> index;
    std::optional<bool> is_in_arrears;
    std::optional<std::int64_t> fixing_days;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> last_recent_period;
    std::optional<std::string> last_recent_period_calendar;
    std::optional<std::string> lookback;
    std::optional<std::int64_t> rate_cutoff;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> include_spread;
    std::optional<bool> is_not_resetting_xccy;
    std::optional<bool> naked_option;
    std::optional<bool> local_cap_floor;
    std::optional<bool> stub_use_original_curve;
    std::optional<bool> observation_shift;
    std::optional<std::string> front_stub_short_index;
    std::optional<std::string> front_stub_long_index;
    std::optional<std::string> front_stub_rounding_type;
    std::optional<std::int64_t> front_stub_rounding_precision;
    std::optional<std::string> back_stub_short_index;
    std::optional<std::string> back_stub_long_index;
    std::optional<std::string> back_stub_rounding_type;
    std::optional<std::int64_t> back_stub_rounding_precision;
};

struct bond_issue_leg_rate_change {
    bond_issue_leg_rate_write write;
    ores::utility::domain::precondition precondition;
};

struct bond_issue_leg_rate_removal {
    bond_issue_leg_rate_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct bond_issue_leg_rate_lookup {
    bond_issue_leg_rate_key key;
    std::optional<ores::trading::domain::bond_issue_leg_rate> bond_issue_leg_rate;
};

struct bond_issue_leg_rate_event {
    boost::uuids::uuid event_id;
    bond_issue_leg_rate_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct bond_issue_leg_rate_version_key {
    bond_issue_leg_rate_key bond_issue_leg_rate;
    std::uint32_t version;
};

struct bond_issue_leg_rate_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_bond_issue_leg_rates_request {
    using response_type = struct list_bond_issue_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.list";
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

struct list_bond_issue_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue_leg_rate> bond_issue_leg_rates;
    std::uint64_t total;
};

struct get_bond_issue_leg_rate_request {
    using response_type = struct get_bond_issue_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_leg_rate_key key;
};

struct get_bond_issue_leg_rate_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue_leg_rate> bond_issue_leg_rate;
};

struct get_many_bond_issue_leg_rates_request {
    using response_type = struct get_many_bond_issue_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_leg_rate_key> keys;
};

struct get_many_bond_issue_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<bond_issue_leg_rate_lookup> entries;
};

struct put_bond_issue_leg_rate_request {
    using response_type = struct put_bond_issue_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_leg_rate_change change;
    ores::utility::domain::change_intent intent;
};

struct put_bond_issue_leg_rate_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue_leg_rate> bond_issue_leg_rate;
};

struct put_many_bond_issue_leg_rates_request {
    using response_type = struct put_many_bond_issue_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_leg_rate_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_bond_issue_leg_rates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue_leg_rate> bond_issue_leg_rates;
};

struct delete_bond_issue_leg_rate_request {
    using response_type = struct delete_bond_issue_leg_rate_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_leg_rate_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_bond_issue_leg_rate_response {
    ores::utility::domain::result result;
};

struct delete_many_bond_issue_leg_rates_request {
    using response_type = struct delete_many_bond_issue_leg_rates_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bond_issue_leg_rate_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_bond_issue_leg_rates_response {
    ores::utility::domain::result result;
};

struct list_bond_issue_leg_rate_versions_request {
    using response_type = struct list_bond_issue_leg_rate_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.bond_issue_leg_rates_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_leg_rate_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bond_issue_leg_rate_versions_filter> filter;
};

struct list_bond_issue_leg_rate_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::bond_issue_leg_rate> versions;
    std::uint64_t total;
};

struct get_bond_issue_leg_rate_version_request {
    using response_type = struct get_bond_issue_leg_rate_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.bond_issue_leg_rates_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bond_issue_leg_rate_version_key key;
};

struct get_bond_issue_leg_rate_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::bond_issue_leg_rate> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bond_issue_leg_rate_event_subjects {
inline constexpr std::string_view created = "trading.v1.bond_issue_leg_rates_events.created";
inline constexpr std::string_view updated = "trading.v1.bond_issue_leg_rates_events.updated";
inline constexpr std::string_view deleted = "trading.v1.bond_issue_leg_rates_events.deleted";
}
/**
 * @brief The members of bond_issue_leg_rate_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_key&) {
    return std::tuple{
        utility::codec::field{"issue_id", &bond_issue_leg_rate_key::issue_id},
        utility::codec::field{"leg_number", &bond_issue_leg_rate_key::leg_number},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_write&) {
    return std::tuple{
        utility::codec::field{"issue_id", &bond_issue_leg_rate_write::issue_id},
        utility::codec::field{"leg_number", &bond_issue_leg_rate_write::leg_number},
        utility::codec::field{"rate_kind", &bond_issue_leg_rate_write::rate_kind},
        utility::codec::field{"index", &bond_issue_leg_rate_write::index},
        utility::codec::field{"is_in_arrears", &bond_issue_leg_rate_write::is_in_arrears},
        utility::codec::field{"fixing_days", &bond_issue_leg_rate_write::fixing_days},
        utility::codec::field{"fixing_calendar", &bond_issue_leg_rate_write::fixing_calendar},
        utility::codec::field{"last_recent_period", &bond_issue_leg_rate_write::last_recent_period},
        utility::codec::field{"last_recent_period_calendar",
                              &bond_issue_leg_rate_write::last_recent_period_calendar},
        utility::codec::field{"lookback", &bond_issue_leg_rate_write::lookback},
        utility::codec::field{"rate_cutoff", &bond_issue_leg_rate_write::rate_cutoff},
        utility::codec::field{"is_averaged", &bond_issue_leg_rate_write::is_averaged},
        utility::codec::field{"has_sub_periods", &bond_issue_leg_rate_write::has_sub_periods},
        utility::codec::field{"include_spread", &bond_issue_leg_rate_write::include_spread},
        utility::codec::field{"is_not_resetting_xccy",
                              &bond_issue_leg_rate_write::is_not_resetting_xccy},
        utility::codec::field{"naked_option", &bond_issue_leg_rate_write::naked_option},
        utility::codec::field{"local_cap_floor", &bond_issue_leg_rate_write::local_cap_floor},
        utility::codec::field{"stub_use_original_curve",
                              &bond_issue_leg_rate_write::stub_use_original_curve},
        utility::codec::field{"observation_shift", &bond_issue_leg_rate_write::observation_shift},
        utility::codec::field{"front_stub_short_index",
                              &bond_issue_leg_rate_write::front_stub_short_index},
        utility::codec::field{"front_stub_long_index",
                              &bond_issue_leg_rate_write::front_stub_long_index},
        utility::codec::field{"front_stub_rounding_type",
                              &bond_issue_leg_rate_write::front_stub_rounding_type},
        utility::codec::field{"front_stub_rounding_precision",
                              &bond_issue_leg_rate_write::front_stub_rounding_precision},
        utility::codec::field{"back_stub_short_index",
                              &bond_issue_leg_rate_write::back_stub_short_index},
        utility::codec::field{"back_stub_long_index",
                              &bond_issue_leg_rate_write::back_stub_long_index},
        utility::codec::field{"back_stub_rounding_type",
                              &bond_issue_leg_rate_write::back_stub_rounding_type},
        utility::codec::field{"back_stub_rounding_precision",
                              &bond_issue_leg_rate_write::back_stub_rounding_precision},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_change&) {
    return std::tuple{
        utility::codec::field{"write", &bond_issue_leg_rate_change::write},
        utility::codec::field{"precondition", &bond_issue_leg_rate_change::precondition},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_removal&) {
    return std::tuple{
        utility::codec::field{"key", &bond_issue_leg_rate_removal::key},
        utility::codec::field{"precondition", &bond_issue_leg_rate_removal::precondition},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &bond_issue_leg_rate_lookup::key},
        utility::codec::field{"bond_issue_leg_rate",
                              &bond_issue_leg_rate_lookup::bond_issue_leg_rate},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &bond_issue_leg_rate_event::event_id},
        utility::codec::field{"key", &bond_issue_leg_rate_event::key},
        utility::codec::field{"action", &bond_issue_leg_rate_event::action},
        utility::codec::field{"version", &bond_issue_leg_rate_event::version},
        utility::codec::field{"occurred_at", &bond_issue_leg_rate_event::occurred_at},
        utility::codec::field{"correlation_id", &bond_issue_leg_rate_event::correlation_id},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_version_key&) {
    return std::tuple{
        utility::codec::field{"bond_issue_leg_rate",
                              &bond_issue_leg_rate_version_key::bond_issue_leg_rate},
        utility::codec::field{"version", &bond_issue_leg_rate_version_key::version},
    };
}

/**
 * @brief The members of bond_issue_leg_rate_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const bond_issue_leg_rate_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &bond_issue_leg_rate_versions_filter::version},
        utility::codec::field{"from_version", &bond_issue_leg_rate_versions_filter::from_version},
        utility::codec::field{"to_version", &bond_issue_leg_rate_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bond_issue_leg_rates_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_issue_leg_rates_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_bond_issue_leg_rates_request::offset},
        utility::codec::field{"limit", &list_bond_issue_leg_rates_request::limit},
        utility::codec::field{"order", &list_bond_issue_leg_rates_request::order},
        utility::codec::field{"as_of", &list_bond_issue_leg_rates_request::as_of},
    };
}

/**
 * @brief The members of list_bond_issue_leg_rates_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_issue_leg_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_issue_leg_rates_response::result},
        utility::codec::field{"bond_issue_leg_rates",
                              &list_bond_issue_leg_rates_response::bond_issue_leg_rates},
        utility::codec::field{"total", &list_bond_issue_leg_rates_response::total},
    };
}

/**
 * @brief The members of get_bond_issue_leg_rate_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_issue_leg_rate_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_issue_leg_rate_request::key},
    };
}

/**
 * @brief The members of get_bond_issue_leg_rate_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_issue_leg_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_issue_leg_rate_response::result},
        utility::codec::field{"bond_issue_leg_rate",
                              &get_bond_issue_leg_rate_response::bond_issue_leg_rate},
    };
}

/**
 * @brief The members of get_many_bond_issue_leg_rates_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_issue_leg_rates_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_bond_issue_leg_rates_request::keys},
    };
}

/**
 * @brief The members of get_many_bond_issue_leg_rates_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_bond_issue_leg_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_bond_issue_leg_rates_response::result},
        utility::codec::field{"entries", &get_many_bond_issue_leg_rates_response::entries},
    };
}

/**
 * @brief The members of put_bond_issue_leg_rate_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_issue_leg_rate_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_bond_issue_leg_rate_request::change},
        utility::codec::field{"intent", &put_bond_issue_leg_rate_request::intent},
    };
}

/**
 * @brief The members of put_bond_issue_leg_rate_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_bond_issue_leg_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_bond_issue_leg_rate_response::result},
        utility::codec::field{"bond_issue_leg_rate",
                              &put_bond_issue_leg_rate_response::bond_issue_leg_rate},
    };
}

/**
 * @brief The members of put_many_bond_issue_leg_rates_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_issue_leg_rates_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_bond_issue_leg_rates_request::changes},
        utility::codec::field{"intent", &put_many_bond_issue_leg_rates_request::intent},
    };
}

/**
 * @brief The members of put_many_bond_issue_leg_rates_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_bond_issue_leg_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_bond_issue_leg_rates_response::result},
        utility::codec::field{"bond_issue_leg_rates",
                              &put_many_bond_issue_leg_rates_response::bond_issue_leg_rates},
    };
}

/**
 * @brief The members of delete_bond_issue_leg_rate_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_issue_leg_rate_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_bond_issue_leg_rate_request::removal},
        utility::codec::field{"intent", &delete_bond_issue_leg_rate_request::intent},
    };
}

/**
 * @brief The members of delete_bond_issue_leg_rate_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_bond_issue_leg_rate_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_bond_issue_leg_rate_response::result},
    };
}

/**
 * @brief The members of delete_many_bond_issue_leg_rates_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_issue_leg_rates_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_bond_issue_leg_rates_request::removals},
        utility::codec::field{"intent", &delete_many_bond_issue_leg_rates_request::intent},
    };
}

/**
 * @brief The members of delete_many_bond_issue_leg_rates_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_bond_issue_leg_rates_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_bond_issue_leg_rates_response::result},
    };
}

/**
 * @brief The members of list_bond_issue_leg_rate_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_issue_leg_rate_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_bond_issue_leg_rate_versions_request::key},
        utility::codec::field{"offset", &list_bond_issue_leg_rate_versions_request::offset},
        utility::codec::field{"limit", &list_bond_issue_leg_rate_versions_request::limit},
        utility::codec::field{"order", &list_bond_issue_leg_rate_versions_request::order},
        utility::codec::field{"filter", &list_bond_issue_leg_rate_versions_request::filter},
    };
}

/**
 * @brief The members of list_bond_issue_leg_rate_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_bond_issue_leg_rate_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_bond_issue_leg_rate_versions_response::result},
        utility::codec::field{"versions", &list_bond_issue_leg_rate_versions_response::versions},
        utility::codec::field{"total", &list_bond_issue_leg_rate_versions_response::total},
    };
}

/**
 * @brief The members of get_bond_issue_leg_rate_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_issue_leg_rate_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_bond_issue_leg_rate_version_request::key},
    };
}

/**
 * @brief The members of get_bond_issue_leg_rate_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_bond_issue_leg_rate_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_bond_issue_leg_rate_version_response::result},
        utility::codec::field{"version", &get_bond_issue_leg_rate_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_key& v);
    static ores::trading::messaging::bond_issue_leg_rate_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_key& v);
    static ores::trading::messaging::bond_issue_leg_rate_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_write& v);
    static ores::trading::messaging::bond_issue_leg_rate_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_write& v);
    static ores::trading::messaging::bond_issue_leg_rate_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_change& v);
    static ores::trading::messaging::bond_issue_leg_rate_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_change& v);
    static ores::trading::messaging::bond_issue_leg_rate_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_removal& v);
    static ores::trading::messaging::bond_issue_leg_rate_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_removal& v);
    static ores::trading::messaging::bond_issue_leg_rate_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_lookup& v);
    static ores::trading::messaging::bond_issue_leg_rate_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_lookup& v);
    static ores::trading::messaging::bond_issue_leg_rate_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::bond_issue_leg_rate_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::bond_issue_leg_rate_event& v);
    static ores::trading::messaging::bond_issue_leg_rate_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_event& v);
    static ores::trading::messaging::bond_issue_leg_rate_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_issue_leg_rate_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::bond_issue_leg_rate_version_key& v);
    static ores::trading::messaging::bond_issue_leg_rate_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_version_key& v);
    static ores::trading::messaging::bond_issue_leg_rate_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::bond_issue_leg_rate_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::bond_issue_leg_rate_versions_filter& v);
    static ores::trading::messaging::bond_issue_leg_rate_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::bond_issue_leg_rate_versions_filter& v);
    static ores::trading::messaging::bond_issue_leg_rate_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_issue_leg_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::list_bond_issue_leg_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::list_bond_issue_leg_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_issue_leg_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::list_bond_issue_leg_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::list_bond_issue_leg_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_issue_leg_rate_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_issue_leg_rate_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_issue_leg_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::get_many_bond_issue_leg_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::get_many_bond_issue_leg_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_bond_issue_leg_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::get_many_bond_issue_leg_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::get_many_bond_issue_leg_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_issue_leg_rate_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::put_bond_issue_leg_rate_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::put_bond_issue_leg_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_bond_issue_leg_rate_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::put_bond_issue_leg_rate_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::put_bond_issue_leg_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_issue_leg_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::put_many_bond_issue_leg_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::put_many_bond_issue_leg_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_bond_issue_leg_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::put_many_bond_issue_leg_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::put_many_bond_issue_leg_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_bond_issue_leg_rate_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::delete_bond_issue_leg_rate_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_issue_leg_rate_request& v);
    static ores::trading::messaging::delete_bond_issue_leg_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_bond_issue_leg_rate_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::delete_bond_issue_leg_rate_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_bond_issue_leg_rate_response& v);
    static ores::trading::messaging::delete_bond_issue_leg_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_issue_leg_rates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::delete_many_bond_issue_leg_rates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_issue_leg_rates_request& v);
    static ores::trading::messaging::delete_many_bond_issue_leg_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_bond_issue_leg_rates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::delete_many_bond_issue_leg_rates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_bond_issue_leg_rates_response& v);
    static ores::trading::messaging::delete_many_bond_issue_leg_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_issue_leg_rate_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_issue_leg_rate_versions_request& v);
    static ores::trading::messaging::list_bond_issue_leg_rate_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_issue_leg_rate_versions_request& v);
    static ores::trading::messaging::list_bond_issue_leg_rate_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_bond_issue_leg_rate_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_bond_issue_leg_rate_versions_response& v);
    static ores::trading::messaging::list_bond_issue_leg_rate_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_bond_issue_leg_rate_versions_response& v);
    static ores::trading::messaging::list_bond_issue_leg_rate_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_issue_leg_rate_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_issue_leg_rate_version_request& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_issue_leg_rate_version_request& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_bond_issue_leg_rate_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_bond_issue_leg_rate_version_response& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_bond_issue_leg_rate_version_response& v);
    static ores::trading::messaging::get_bond_issue_leg_rate_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
