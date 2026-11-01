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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_OPTION_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_option.hpp"
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

struct instrument_option_key {
    boost::uuids::uuid trade_id;
};

struct instrument_option_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string long_short;
    std::optional<std::string> option_type;
    std::optional<std::string> payoff_type;
    std::optional<std::string> payoff_type_2;
    std::optional<std::string> style;
    std::optional<std::string> notice_period;
    std::optional<std::string> notice_calendar;
    std::optional<std::string> notice_convention;
    std::optional<std::string> mid_coupon_exercise;
    std::optional<std::string> settlement;
    std::optional<std::string> settlement_method;
    std::optional<std::string> pay_off_at_expiry;
    std::optional<ores::utility::decimal::decimal> premium_amount;
    std::optional<std::string> premium_currency;
    std::optional<std::chrono::year_month_day> premium_pay_date;
    std::optional<std::string> exercise_fee_settlement_period;
    std::optional<std::string> exercise_fee_settlement_calendar;
    std::optional<std::string> exercise_fee_settlement_convention;
    std::optional<std::string> automatic_exercise;
    bool has_exercise_data;
    std::optional<std::chrono::year_month_day> exercise_date;
    std::optional<ores::utility::decimal::decimal> exercise_price;
    bool has_payment_data;
    std::optional<int> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> payment_convention;
    std::optional<std::string> payment_relative_to;
    bool has_settlement_data;
    std::optional<std::string> settlement_pay_currency;
    std::optional<std::string> settlement_fx_index;
    std::optional<std::chrono::year_month_day> settlement_fixing_date;
};

struct instrument_option_change {
    instrument_option_write write;
    ores::utility::domain::precondition precondition;
};

struct instrument_option_removal {
    instrument_option_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct instrument_option_lookup {
    instrument_option_key key;
    std::optional<ores::trading::domain::instrument_option> instrument_option;
};

struct instrument_options_filter {
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;
};

struct instrument_option_event {
    boost::uuids::uuid event_id;
    instrument_option_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct instrument_option_version_key {
    instrument_option_key instrument_option;
    std::uint32_t version;
};

struct instrument_option_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_instrument_options_request {
    using response_type = struct list_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.list";
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
    std::optional<instrument_options_filter> filter;
    std::optional<std::string> as_of;
};

struct list_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> instrument_options;
    std::uint64_t total;
};

struct get_instrument_option_request {
    using response_type = struct get_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_key key;
};

struct get_instrument_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option> instrument_option;
};

struct get_many_instrument_options_request {
    using response_type = struct get_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_key> keys;
};

struct get_many_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<instrument_option_lookup> entries;
};

struct put_instrument_option_request {
    using response_type = struct put_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_change change;
    ores::utility::domain::change_intent intent;
};

struct put_instrument_option_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option> instrument_option;
};

struct put_many_instrument_options_request {
    using response_type = struct put_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_instrument_options_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> instrument_options;
};

struct delete_instrument_option_request {
    using response_type = struct delete_instrument_option_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_instrument_option_response {
    ores::utility::domain::result result;
};

struct delete_many_instrument_options_request {
    using response_type = struct delete_many_instrument_options_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_option_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_instrument_options_response {
    ores::utility::domain::result result;
};

struct list_instrument_option_versions_request {
    using response_type = struct list_instrument_option_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_option_versions_filter> filter;
};

struct list_instrument_option_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_option> versions;
    std::uint64_t total;
};

struct get_instrument_option_version_request {
    using response_type = struct get_instrument_option_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_options_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_option_version_key key;
};

struct get_instrument_option_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_option> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_option_event_subjects {
inline constexpr std::string_view created = "trading.v1.instrument_options_events.created";
inline constexpr std::string_view updated = "trading.v1.instrument_options_events.updated";
inline constexpr std::string_view deleted = "trading.v1.instrument_options_events.deleted";
}
/**
 * @brief The members of instrument_option_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_key&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_option_key::trade_id},
    };
}

/**
 * @brief The members of instrument_option_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_write&) {
    return std::tuple{
        utility::codec::field{"trade_id", &instrument_option_write::trade_id},
        utility::codec::field{"trade_activity_id", &instrument_option_write::trade_activity_id},
        utility::codec::field{"long_short", &instrument_option_write::long_short},
        utility::codec::field{"option_type", &instrument_option_write::option_type},
        utility::codec::field{"payoff_type", &instrument_option_write::payoff_type},
        utility::codec::field{"payoff_type_2", &instrument_option_write::payoff_type_2},
        utility::codec::field{"style", &instrument_option_write::style},
        utility::codec::field{"notice_period", &instrument_option_write::notice_period},
        utility::codec::field{"notice_calendar", &instrument_option_write::notice_calendar},
        utility::codec::field{"notice_convention", &instrument_option_write::notice_convention},
        utility::codec::field{"mid_coupon_exercise", &instrument_option_write::mid_coupon_exercise},
        utility::codec::field{"settlement", &instrument_option_write::settlement},
        utility::codec::field{"settlement_method", &instrument_option_write::settlement_method},
        utility::codec::field{"pay_off_at_expiry", &instrument_option_write::pay_off_at_expiry},
        utility::codec::field{"premium_amount", &instrument_option_write::premium_amount},
        utility::codec::field{"premium_currency", &instrument_option_write::premium_currency},
        utility::codec::field{"premium_pay_date", &instrument_option_write::premium_pay_date},
        utility::codec::field{"exercise_fee_settlement_period",
                              &instrument_option_write::exercise_fee_settlement_period},
        utility::codec::field{"exercise_fee_settlement_calendar",
                              &instrument_option_write::exercise_fee_settlement_calendar},
        utility::codec::field{"exercise_fee_settlement_convention",
                              &instrument_option_write::exercise_fee_settlement_convention},
        utility::codec::field{"automatic_exercise", &instrument_option_write::automatic_exercise},
        utility::codec::field{"has_exercise_data", &instrument_option_write::has_exercise_data},
        utility::codec::field{"exercise_date", &instrument_option_write::exercise_date},
        utility::codec::field{"exercise_price", &instrument_option_write::exercise_price},
        utility::codec::field{"has_payment_data", &instrument_option_write::has_payment_data},
        utility::codec::field{"payment_lag", &instrument_option_write::payment_lag},
        utility::codec::field{"payment_calendar", &instrument_option_write::payment_calendar},
        utility::codec::field{"payment_convention", &instrument_option_write::payment_convention},
        utility::codec::field{"payment_relative_to", &instrument_option_write::payment_relative_to},
        utility::codec::field{"has_settlement_data", &instrument_option_write::has_settlement_data},
        utility::codec::field{"settlement_pay_currency",
                              &instrument_option_write::settlement_pay_currency},
        utility::codec::field{"settlement_fx_index", &instrument_option_write::settlement_fx_index},
        utility::codec::field{"settlement_fixing_date",
                              &instrument_option_write::settlement_fixing_date},
    };
}

/**
 * @brief The members of instrument_option_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_change&) {
    return std::tuple{
        utility::codec::field{"write", &instrument_option_change::write},
        utility::codec::field{"precondition", &instrument_option_change::precondition},
    };
}

/**
 * @brief The members of instrument_option_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_removal&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_option_removal::key},
        utility::codec::field{"precondition", &instrument_option_removal::precondition},
    };
}

/**
 * @brief The members of instrument_option_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &instrument_option_lookup::key},
        utility::codec::field{"instrument_option", &instrument_option_lookup::instrument_option},
    };
}

/**
 * @brief The members of instrument_options_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_options_filter&) {
    return std::tuple{
        utility::codec::field{"trade_id_one_of", &instrument_options_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of instrument_option_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &instrument_option_event::event_id},
        utility::codec::field{"key", &instrument_option_event::key},
        utility::codec::field{"action", &instrument_option_event::action},
        utility::codec::field{"version", &instrument_option_event::version},
        utility::codec::field{"occurred_at", &instrument_option_event::occurred_at},
        utility::codec::field{"correlation_id", &instrument_option_event::correlation_id},
    };
}

/**
 * @brief The members of instrument_option_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_version_key&) {
    return std::tuple{
        utility::codec::field{"instrument_option",
                              &instrument_option_version_key::instrument_option},
        utility::codec::field{"version", &instrument_option_version_key::version},
    };
}

/**
 * @brief The members of instrument_option_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const instrument_option_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &instrument_option_versions_filter::version},
        utility::codec::field{"from_version", &instrument_option_versions_filter::from_version},
        utility::codec::field{"to_version", &instrument_option_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_instrument_options_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_options_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_instrument_options_request::offset},
        utility::codec::field{"limit", &list_instrument_options_request::limit},
        utility::codec::field{"order", &list_instrument_options_request::order},
        utility::codec::field{"filter", &list_instrument_options_request::filter},
        utility::codec::field{"as_of", &list_instrument_options_request::as_of},
    };
}

/**
 * @brief The members of list_instrument_options_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_options_response::result},
        utility::codec::field{"instrument_options",
                              &list_instrument_options_response::instrument_options},
        utility::codec::field{"total", &list_instrument_options_response::total},
    };
}

/**
 * @brief The members of get_instrument_option_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_option_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_option_request::key},
    };
}

/**
 * @brief The members of get_instrument_option_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_option_response::result},
        utility::codec::field{"instrument_option",
                              &get_instrument_option_response::instrument_option},
    };
}

/**
 * @brief The members of get_many_instrument_options_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_options_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_instrument_options_request::keys},
    };
}

/**
 * @brief The members of get_many_instrument_options_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_instrument_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_instrument_options_response::result},
        utility::codec::field{"entries", &get_many_instrument_options_response::entries},
    };
}

/**
 * @brief The members of put_instrument_option_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_option_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_instrument_option_request::change},
        utility::codec::field{"intent", &put_instrument_option_request::intent},
    };
}

/**
 * @brief The members of put_instrument_option_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_instrument_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_instrument_option_response::result},
        utility::codec::field{"instrument_option",
                              &put_instrument_option_response::instrument_option},
    };
}

/**
 * @brief The members of put_many_instrument_options_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_options_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_instrument_options_request::changes},
        utility::codec::field{"intent", &put_many_instrument_options_request::intent},
    };
}

/**
 * @brief The members of put_many_instrument_options_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_instrument_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_instrument_options_response::result},
        utility::codec::field{"instrument_options",
                              &put_many_instrument_options_response::instrument_options},
    };
}

/**
 * @brief The members of delete_instrument_option_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_option_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_instrument_option_request::removal},
        utility::codec::field{"intent", &delete_instrument_option_request::intent},
    };
}

/**
 * @brief The members of delete_instrument_option_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_instrument_option_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_instrument_option_response::result},
    };
}

/**
 * @brief The members of delete_many_instrument_options_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_options_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_instrument_options_request::removals},
        utility::codec::field{"intent", &delete_many_instrument_options_request::intent},
    };
}

/**
 * @brief The members of delete_many_instrument_options_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_instrument_options_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_instrument_options_response::result},
    };
}

/**
 * @brief The members of list_instrument_option_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_option_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_instrument_option_versions_request::key},
        utility::codec::field{"offset", &list_instrument_option_versions_request::offset},
        utility::codec::field{"limit", &list_instrument_option_versions_request::limit},
        utility::codec::field{"order", &list_instrument_option_versions_request::order},
        utility::codec::field{"filter", &list_instrument_option_versions_request::filter},
    };
}

/**
 * @brief The members of list_instrument_option_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_instrument_option_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_instrument_option_versions_response::result},
        utility::codec::field{"versions", &list_instrument_option_versions_response::versions},
        utility::codec::field{"total", &list_instrument_option_versions_response::total},
    };
}

/**
 * @brief The members of get_instrument_option_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_option_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_instrument_option_version_request::key},
    };
}

/**
 * @brief The members of get_instrument_option_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_instrument_option_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_instrument_option_version_response::result},
        utility::codec::field{"version", &get_instrument_option_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_key& v);
    static ores::trading::messaging::instrument_option_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_key& v);
    static ores::trading::messaging::instrument_option_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_write& v);
    static ores::trading::messaging::instrument_option_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_write& v);
    static ores::trading::messaging::instrument_option_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_change& v);
    static ores::trading::messaging::instrument_option_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_change& v);
    static ores::trading::messaging::instrument_option_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_removal& v);
    static ores::trading::messaging::instrument_option_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_removal& v);
    static ores::trading::messaging::instrument_option_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_lookup& v);
    static ores::trading::messaging::instrument_option_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_lookup& v);
    static ores::trading::messaging::instrument_option_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_options_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_options_filter& v);
    static ores::trading::messaging::instrument_options_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_options_filter& v);
    static ores::trading::messaging::instrument_options_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::instrument_option_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_event& v);
    static ores::trading::messaging::instrument_option_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::instrument_option_event& v);
    static ores::trading::messaging::instrument_option_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::instrument_option_version_key& v);
    static ores::trading::messaging::instrument_option_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_version_key& v);
    static ores::trading::messaging::instrument_option_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::instrument_option_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::instrument_option_versions_filter& v);
    static ores::trading::messaging::instrument_option_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::instrument_option_versions_filter& v);
    static ores::trading::messaging::instrument_option_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_options_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_options_request& v);
    static ores::trading::messaging::list_instrument_options_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_options_request& v);
    static ores::trading::messaging::list_instrument_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_options_response& v);
    static ores::trading::messaging::list_instrument_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_options_response& v);
    static ores::trading::messaging::list_instrument_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_instrument_option_request& v);
    static ores::trading::messaging::get_instrument_option_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_request& v);
    static ores::trading::messaging::get_instrument_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_response& v);
    static ores::trading::messaging::get_instrument_option_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_response& v);
    static ores::trading::messaging::get_instrument_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_options_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_options_request& v);
    static ores::trading::messaging::get_many_instrument_options_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_options_request& v);
    static ores::trading::messaging::get_many_instrument_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_instrument_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_instrument_options_response& v);
    static ores::trading::messaging::get_many_instrument_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_instrument_options_response& v);
    static ores::trading::messaging::get_many_instrument_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_option_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_instrument_option_request& v);
    static ores::trading::messaging::put_instrument_option_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_option_request& v);
    static ores::trading::messaging::put_instrument_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_instrument_option_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_instrument_option_response& v);
    static ores::trading::messaging::put_instrument_option_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_instrument_option_response& v);
    static ores::trading::messaging::put_instrument_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_options_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_options_request& v);
    static ores::trading::messaging::put_many_instrument_options_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_options_request& v);
    static ores::trading::messaging::put_many_instrument_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_instrument_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_instrument_options_response& v);
    static ores::trading::messaging::put_many_instrument_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_instrument_options_response& v);
    static ores::trading::messaging::put_many_instrument_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_option_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_option_request& v);
    static ores::trading::messaging::delete_instrument_option_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_option_request& v);
    static ores::trading::messaging::delete_instrument_option_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_instrument_option_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_instrument_option_response& v);
    static ores::trading::messaging::delete_instrument_option_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_instrument_option_response& v);
    static ores::trading::messaging::delete_instrument_option_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_options_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_options_request& v);
    static ores::trading::messaging::delete_many_instrument_options_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_options_request& v);
    static ores::trading::messaging::delete_many_instrument_options_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_instrument_options_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_instrument_options_response& v);
    static ores::trading::messaging::delete_many_instrument_options_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_instrument_options_response& v);
    static ores::trading::messaging::delete_many_instrument_options_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_versions_request& v);
    static ores::trading::messaging::list_instrument_option_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_option_versions_request& v);
    static ores::trading::messaging::list_instrument_option_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_instrument_option_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_instrument_option_versions_response& v);
    static ores::trading::messaging::list_instrument_option_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_instrument_option_versions_response& v);
    static ores::trading::messaging::list_instrument_option_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_version_request& v);
    static ores::trading::messaging::get_instrument_option_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_version_request& v);
    static ores::trading::messaging::get_instrument_option_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_instrument_option_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_instrument_option_version_response& v);
    static ores::trading::messaging::get_instrument_option_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_instrument_option_version_response& v);
    static ores::trading::messaging::get_instrument_option_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
