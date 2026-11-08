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
#ifndef ORES_TRADING_API_MESSAGING_SWAP_LEG_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_SWAP_LEG_PROTOCOL_HPP

#include "ores.trading.api/domain/swap_leg_codec.hpp"
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

struct swap_leg_key {
    boost::uuids::uuid trade_id;
    int leg_number{};

    friend bool operator==(const swap_leg_key&, const swap_leg_key&) = default;
};

struct swap_leg_write {
    boost::uuids::uuid trade_id;
    int leg_number{};
    boost::uuids::uuid trade_activity_id;
    std::optional<bool> payer;
    std::string leg_type_code;
    std::string day_count_fraction_code;
    std::string business_day_convention_code;
    std::string payment_frequency_code;
    std::string floating_index_code;
    std::string currency;
    std::optional<std::string> leg_group;
    std::optional<int> fixing_days;
    std::optional<bool> is_in_arrears;
    std::optional<bool> include_spread;
    std::optional<bool> is_averaged;
    std::optional<bool> has_sub_periods;
    std::optional<bool> notional_initial_exchange;
    std::optional<bool> notional_final_exchange;
    std::optional<std::string> payment_lag;
    std::optional<std::string> payment_calendar;
    std::optional<std::string> commodity_price_type;
    std::optional<int> future_month_offset;
    std::optional<int> daily_expiry_offset;
    std::optional<std::string> pricing_calendar;
    std::optional<std::string> equity_return_type;
    std::optional<std::string> equity_name;
    std::optional<std::string> equity_underlying_type;
    std::optional<std::string> equity_underlying_identifier_type;
    std::optional<double> equity_quantity;
    std::optional<ores::utility::decimal::decimal> equity_initial_price;
    std::optional<std::string> equity_initial_price_currency;
    std::optional<ores::utility::decimal::decimal> equity_dividend_factor;
    std::optional<bool> equity_notional_reset;
    std::optional<std::string> equity_currency;
    std::optional<std::string> fx_index;
    std::optional<std::string> fx_reset_foreign_currency;
    std::optional<std::string> fx_reset_start_date;
    std::optional<ores::utility::decimal::decimal> fx_reset_foreign_amount;
    std::optional<std::string> fx_reset_fx_index;
    std::optional<int> fx_reset_fixing_days;
    std::optional<std::string> fx_reset_fixing_calendar;
    std::optional<std::string> fixing_calendar;
    std::optional<std::string> digital_call_position;
    std::optional<bool> digital_is_call_atm_included;
    std::optional<std::string> digital_put_position;
    std::optional<bool> digital_is_put_atm_included;
    std::optional<ores::utility::decimal::decimal> equity_margin_initial_margin_factor;
    std::optional<ores::utility::decimal::decimal> equity_margin_multiplier;
    std::optional<std::string> commodity_pay_relative_to;
    std::optional<std::string> commodity_quantity_frequency;
    std::optional<std::string> pricing_date_rule;
    std::optional<bool> indexings_from_asset_leg;
    std::optional<std::string> floating_index_code_2;
    std::optional<int> cms_duration;
    std::optional<bool> naked_option;
    std::optional<std::string> lookback;
    std::optional<int> rate_cutoff;
    std::optional<bool> local_cap_floor;
    std::optional<std::string> observation_lag;
    std::optional<double> cpi_base;
    std::optional<std::chrono::year_month_day> cpi_start_date;
    std::optional<std::string> cpi_interpolation;
    std::optional<bool> cpi_interpolated;
    std::optional<bool> subtract_inflation_notional;
    std::optional<std::string> zero_coupon_compounding;
    std::optional<bool> zero_coupon_subtract_notional;

    friend bool operator==(const swap_leg_write&, const swap_leg_write&) = default;
};

struct swap_leg_change {
    swap_leg_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const swap_leg_change&, const swap_leg_change&) = default;
};

struct swap_leg_removal {
    swap_leg_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const swap_leg_removal&, const swap_leg_removal&) = default;
};

struct swap_leg_lookup {
    swap_leg_key key;
    std::optional<ores::trading::domain::swap_leg> swap_leg;

    friend bool operator==(const swap_leg_lookup&, const swap_leg_lookup&) = default;
};

struct swap_legs_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const swap_legs_filter&, const swap_legs_filter&) = default;
};

struct swap_leg_event {
    boost::uuids::uuid event_id;
    swap_leg_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const swap_leg_event&, const swap_leg_event&) = default;
};

struct swap_leg_version_key {
    swap_leg_key swap_leg;
    std::uint32_t version{};

    friend bool operator==(const swap_leg_version_key&, const swap_leg_version_key&) = default;
};

struct swap_leg_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const swap_leg_versions_filter&,
                           const swap_leg_versions_filter&) = default;
};

struct list_swap_legs_request {
    using response_type = struct list_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.list";
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
    std::optional<swap_legs_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_swap_legs_request&, const list_swap_legs_request&) = default;
};

struct list_swap_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg> swap_legs;
    std::uint64_t total{};

    friend bool operator==(const list_swap_legs_response&,
                           const list_swap_legs_response&) = default;
};

struct get_swap_leg_request {
    using response_type = struct get_swap_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_key key;

    friend bool operator==(const get_swap_leg_request&, const get_swap_leg_request&) = default;
};

struct get_swap_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg> swap_leg;

    friend bool operator==(const get_swap_leg_response&, const get_swap_leg_response&) = default;
};

struct get_many_swap_legs_request {
    using response_type = struct get_many_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_key> keys;

    friend bool operator==(const get_many_swap_legs_request&,
                           const get_many_swap_legs_request&) = default;
};

struct get_many_swap_legs_response {
    ores::utility::domain::result result;
    std::vector<swap_leg_lookup> entries;

    friend bool operator==(const get_many_swap_legs_response&,
                           const get_many_swap_legs_response&) = default;
};

struct put_swap_leg_request {
    using response_type = struct put_swap_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_swap_leg_request&, const put_swap_leg_request&) = default;
};

struct put_swap_leg_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg> swap_leg;

    friend bool operator==(const put_swap_leg_response&, const put_swap_leg_response&) = default;
};

struct put_many_swap_legs_request {
    using response_type = struct put_many_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_swap_legs_request&,
                           const put_many_swap_legs_request&) = default;
};

struct put_many_swap_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg> swap_legs;

    friend bool operator==(const put_many_swap_legs_response&,
                           const put_many_swap_legs_response&) = default;
};

struct delete_swap_leg_request {
    using response_type = struct delete_swap_leg_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_swap_leg_request&,
                           const delete_swap_leg_request&) = default;
};

struct delete_swap_leg_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_swap_leg_response&,
                           const delete_swap_leg_response&) = default;
};

struct delete_many_swap_legs_request {
    using response_type = struct delete_many_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swap_leg_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_swap_legs_request&,
                           const delete_many_swap_legs_request&) = default;
};

struct delete_many_swap_legs_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_swap_legs_response&,
                           const delete_many_swap_legs_response&) = default;
};

struct list_by_trade_id_swap_legs_request {
    using response_type = struct list_by_trade_id_swap_legs_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs.list_by_trade_id";
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
    std::optional<swap_legs_filter> filter;

    friend bool operator==(const list_by_trade_id_swap_legs_request&,
                           const list_by_trade_id_swap_legs_request&) = default;
};

struct list_by_trade_id_swap_legs_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg> swap_legs;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_swap_legs_response&,
                           const list_by_trade_id_swap_legs_response&) = default;
};

struct list_swap_leg_versions_request {
    using response_type = struct list_swap_leg_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<swap_leg_versions_filter> filter;

    friend bool operator==(const list_swap_leg_versions_request&,
                           const list_swap_leg_versions_request&) = default;
};

struct list_swap_leg_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::swap_leg> versions;
    std::uint64_t total{};

    friend bool operator==(const list_swap_leg_versions_response&,
                           const list_swap_leg_versions_response&) = default;
};

struct get_swap_leg_version_request {
    using response_type = struct get_swap_leg_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.swap_legs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swap_leg_version_key key;

    friend bool operator==(const get_swap_leg_version_request&,
                           const get_swap_leg_version_request&) = default;
};

struct get_swap_leg_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::swap_leg> version;

    friend bool operator==(const get_swap_leg_version_response&,
                           const get_swap_leg_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace swap_leg_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.swap_legs.created";
inline constexpr std::string_view updated = "trading.v1.events.swap_legs.updated";
inline constexpr std::string_view deleted = "trading.v1.events.swap_legs.deleted";
}
/**
 * @brief The members of swap_leg_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_key>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_leg_key::trade_id},
        utility::codec::field{"leg_number", &swap_leg_key::leg_number},
    };
}

/**
 * @brief The members of swap_leg_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_write>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_leg_write::trade_id},
        utility::codec::field{"leg_number", &swap_leg_write::leg_number},
        utility::codec::field{"trade_activity_id", &swap_leg_write::trade_activity_id},
        utility::codec::field{"payer", &swap_leg_write::payer},
        utility::codec::field{"leg_type_code", &swap_leg_write::leg_type_code},
        utility::codec::field{"day_count_fraction_code", &swap_leg_write::day_count_fraction_code},
        utility::codec::field{"business_day_convention_code",
                              &swap_leg_write::business_day_convention_code},
        utility::codec::field{"payment_frequency_code", &swap_leg_write::payment_frequency_code},
        utility::codec::field{"floating_index_code", &swap_leg_write::floating_index_code},
        utility::codec::field{"currency", &swap_leg_write::currency},
        utility::codec::field{"leg_group", &swap_leg_write::leg_group},
        utility::codec::field{"fixing_days", &swap_leg_write::fixing_days},
        utility::codec::field{"is_in_arrears", &swap_leg_write::is_in_arrears},
        utility::codec::field{"include_spread", &swap_leg_write::include_spread},
        utility::codec::field{"is_averaged", &swap_leg_write::is_averaged},
        utility::codec::field{"has_sub_periods", &swap_leg_write::has_sub_periods},
        utility::codec::field{"notional_initial_exchange",
                              &swap_leg_write::notional_initial_exchange},
        utility::codec::field{"notional_final_exchange", &swap_leg_write::notional_final_exchange},
        utility::codec::field{"payment_lag", &swap_leg_write::payment_lag},
        utility::codec::field{"payment_calendar", &swap_leg_write::payment_calendar},
        utility::codec::field{"commodity_price_type", &swap_leg_write::commodity_price_type},
        utility::codec::field{"future_month_offset", &swap_leg_write::future_month_offset},
        utility::codec::field{"daily_expiry_offset", &swap_leg_write::daily_expiry_offset},
        utility::codec::field{"pricing_calendar", &swap_leg_write::pricing_calendar},
        utility::codec::field{"equity_return_type", &swap_leg_write::equity_return_type},
        utility::codec::field{"equity_name", &swap_leg_write::equity_name},
        utility::codec::field{"equity_underlying_type", &swap_leg_write::equity_underlying_type},
        utility::codec::field{"equity_underlying_identifier_type",
                              &swap_leg_write::equity_underlying_identifier_type},
        utility::codec::field{"equity_quantity", &swap_leg_write::equity_quantity},
        utility::codec::field{"equity_initial_price", &swap_leg_write::equity_initial_price},
        utility::codec::field{"equity_initial_price_currency",
                              &swap_leg_write::equity_initial_price_currency},
        utility::codec::field{"equity_dividend_factor", &swap_leg_write::equity_dividend_factor},
        utility::codec::field{"equity_notional_reset", &swap_leg_write::equity_notional_reset},
        utility::codec::field{"equity_currency", &swap_leg_write::equity_currency},
        utility::codec::field{"fx_index", &swap_leg_write::fx_index},
        utility::codec::field{"fx_reset_foreign_currency",
                              &swap_leg_write::fx_reset_foreign_currency},
        utility::codec::field{"fx_reset_start_date", &swap_leg_write::fx_reset_start_date},
        utility::codec::field{"fx_reset_foreign_amount", &swap_leg_write::fx_reset_foreign_amount},
        utility::codec::field{"fx_reset_fx_index", &swap_leg_write::fx_reset_fx_index},
        utility::codec::field{"fx_reset_fixing_days", &swap_leg_write::fx_reset_fixing_days},
        utility::codec::field{"fx_reset_fixing_calendar",
                              &swap_leg_write::fx_reset_fixing_calendar},
        utility::codec::field{"fixing_calendar", &swap_leg_write::fixing_calendar},
        utility::codec::field{"digital_call_position", &swap_leg_write::digital_call_position},
        utility::codec::field{"digital_is_call_atm_included",
                              &swap_leg_write::digital_is_call_atm_included},
        utility::codec::field{"digital_put_position", &swap_leg_write::digital_put_position},
        utility::codec::field{"digital_is_put_atm_included",
                              &swap_leg_write::digital_is_put_atm_included},
        utility::codec::field{"equity_margin_initial_margin_factor",
                              &swap_leg_write::equity_margin_initial_margin_factor},
        utility::codec::field{"equity_margin_multiplier",
                              &swap_leg_write::equity_margin_multiplier},
        utility::codec::field{"commodity_pay_relative_to",
                              &swap_leg_write::commodity_pay_relative_to},
        utility::codec::field{"commodity_quantity_frequency",
                              &swap_leg_write::commodity_quantity_frequency},
        utility::codec::field{"pricing_date_rule", &swap_leg_write::pricing_date_rule},
        utility::codec::field{"indexings_from_asset_leg",
                              &swap_leg_write::indexings_from_asset_leg},
        utility::codec::field{"floating_index_code_2", &swap_leg_write::floating_index_code_2},
        utility::codec::field{"cms_duration", &swap_leg_write::cms_duration},
        utility::codec::field{"naked_option", &swap_leg_write::naked_option},
        utility::codec::field{"lookback", &swap_leg_write::lookback},
        utility::codec::field{"rate_cutoff", &swap_leg_write::rate_cutoff},
        utility::codec::field{"local_cap_floor", &swap_leg_write::local_cap_floor},
        utility::codec::field{"observation_lag", &swap_leg_write::observation_lag},
        utility::codec::field{"cpi_base", &swap_leg_write::cpi_base},
        utility::codec::field{"cpi_start_date", &swap_leg_write::cpi_start_date},
        utility::codec::field{"cpi_interpolation", &swap_leg_write::cpi_interpolation},
        utility::codec::field{"cpi_interpolated", &swap_leg_write::cpi_interpolated},
        utility::codec::field{"subtract_inflation_notional",
                              &swap_leg_write::subtract_inflation_notional},
        utility::codec::field{"zero_coupon_compounding", &swap_leg_write::zero_coupon_compounding},
        utility::codec::field{"zero_coupon_subtract_notional",
                              &swap_leg_write::zero_coupon_subtract_notional},
    };
}

/**
 * @brief The members of swap_leg_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_change>) {
    return std::tuple{
        utility::codec::field{"write", &swap_leg_change::write},
        utility::codec::field{"precondition", &swap_leg_change::precondition},
    };
}

/**
 * @brief The members of swap_leg_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_removal>) {
    return std::tuple{
        utility::codec::field{"key", &swap_leg_removal::key},
        utility::codec::field{"precondition", &swap_leg_removal::precondition},
    };
}

/**
 * @brief The members of swap_leg_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &swap_leg_lookup::key},
        utility::codec::field{"swap_leg", &swap_leg_lookup::swap_leg},
    };
}

/**
 * @brief The members of swap_legs_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_legs_filter>) {
    return std::tuple{
        utility::codec::field{"trade_id", &swap_legs_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &swap_legs_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of swap_leg_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &swap_leg_event::event_id},
        utility::codec::field{"key", &swap_leg_event::key},
        utility::codec::field{"action", &swap_leg_event::action},
        utility::codec::field{"version", &swap_leg_event::version},
        utility::codec::field{"occurred_at", &swap_leg_event::occurred_at},
        utility::codec::field{"correlation_id", &swap_leg_event::correlation_id},
    };
}

/**
 * @brief The members of swap_leg_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_version_key>) {
    return std::tuple{
        utility::codec::field{"swap_leg", &swap_leg_version_key::swap_leg},
        utility::codec::field{"version", &swap_leg_version_key::version},
    };
}

/**
 * @brief The members of swap_leg_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<swap_leg_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &swap_leg_versions_filter::version},
        utility::codec::field{"from_version", &swap_leg_versions_filter::from_version},
        utility::codec::field{"to_version", &swap_leg_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_swap_legs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_legs_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_swap_legs_request::offset},
        utility::codec::field{"limit", &list_swap_legs_request::limit},
        utility::codec::field{"order", &list_swap_legs_request::order},
        utility::codec::field{"filter", &list_swap_legs_request::filter},
        utility::codec::field{"as_of", &list_swap_legs_request::as_of},
    };
}

/**
 * @brief The members of list_swap_legs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_swap_legs_response::result},
        utility::codec::field{"swap_legs", &list_swap_legs_response::swap_legs},
        utility::codec::field{"total", &list_swap_legs_response::total},
    };
}

/**
 * @brief The members of get_swap_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_swap_leg_request::key},
    };
}

/**
 * @brief The members of get_swap_leg_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_swap_leg_response::result},
        utility::codec::field{"swap_leg", &get_swap_leg_response::swap_leg},
    };
}

/**
 * @brief The members of get_many_swap_legs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_swap_legs_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_swap_legs_request::keys},
    };
}

/**
 * @brief The members of get_many_swap_legs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_swap_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_swap_legs_response::result},
        utility::codec::field{"entries", &get_many_swap_legs_response::entries},
    };
}

/**
 * @brief The members of put_swap_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_swap_leg_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_swap_leg_request::change},
        utility::codec::field{"intent", &put_swap_leg_request::intent},
    };
}

/**
 * @brief The members of put_swap_leg_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_swap_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_swap_leg_response::result},
        utility::codec::field{"swap_leg", &put_swap_leg_response::swap_leg},
    };
}

/**
 * @brief The members of put_many_swap_legs_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_swap_legs_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_swap_legs_request::changes},
        utility::codec::field{"intent", &put_many_swap_legs_request::intent},
    };
}

/**
 * @brief The members of put_many_swap_legs_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_swap_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_swap_legs_response::result},
        utility::codec::field{"swap_legs", &put_many_swap_legs_response::swap_legs},
    };
}

/**
 * @brief The members of delete_swap_leg_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_swap_leg_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_swap_leg_request::removal},
        utility::codec::field{"intent", &delete_swap_leg_request::intent},
    };
}

/**
 * @brief The members of delete_swap_leg_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_swap_leg_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_swap_leg_response::result},
    };
}

/**
 * @brief The members of delete_many_swap_legs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_swap_legs_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_swap_legs_request::removals},
        utility::codec::field{"intent", &delete_many_swap_legs_request::intent},
    };
}

/**
 * @brief The members of delete_many_swap_legs_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_swap_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_swap_legs_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_swap_legs_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_trade_id_swap_legs_request>) {
    return std::tuple{
        utility::codec::field{"trade_id", &list_by_trade_id_swap_legs_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_swap_legs_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_swap_legs_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_swap_legs_request::limit},
        utility::codec::field{"order", &list_by_trade_id_swap_legs_request::order},
        utility::codec::field{"filter", &list_by_trade_id_swap_legs_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_swap_legs_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_by_trade_id_swap_legs_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_by_trade_id_swap_legs_response::result},
        utility::codec::field{"swap_legs", &list_by_trade_id_swap_legs_response::swap_legs},
        utility::codec::field{"total", &list_by_trade_id_swap_legs_response::total},
    };
}

/**
 * @brief The members of list_swap_leg_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_leg_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_swap_leg_versions_request::key},
        utility::codec::field{"offset", &list_swap_leg_versions_request::offset},
        utility::codec::field{"limit", &list_swap_leg_versions_request::limit},
        utility::codec::field{"order", &list_swap_leg_versions_request::order},
        utility::codec::field{"filter", &list_swap_leg_versions_request::filter},
    };
}

/**
 * @brief The members of list_swap_leg_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_swap_leg_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_swap_leg_versions_response::result},
        utility::codec::field{"versions", &list_swap_leg_versions_response::versions},
        utility::codec::field{"total", &list_swap_leg_versions_response::total},
    };
}

/**
 * @brief The members of get_swap_leg_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_swap_leg_version_request::key},
    };
}

/**
 * @brief The members of get_swap_leg_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_swap_leg_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_swap_leg_version_response::result},
        utility::codec::field{"version", &get_swap_leg_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_key& v);
    static ores::trading::messaging::swap_leg_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_key& v);
    static ores::trading::messaging::swap_leg_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_write& v);
    static ores::trading::messaging::swap_leg_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_write& v);
    static ores::trading::messaging::swap_leg_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_change& v);
    static ores::trading::messaging::swap_leg_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_change& v);
    static ores::trading::messaging::swap_leg_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_removal& v);
    static ores::trading::messaging::swap_leg_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_removal& v);
    static ores::trading::messaging::swap_leg_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_lookup& v);
    static ores::trading::messaging::swap_leg_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_lookup& v);
    static ores::trading::messaging::swap_leg_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_legs_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_legs_filter& v);
    static ores::trading::messaging::swap_legs_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_legs_filter& v);
    static ores::trading::messaging::swap_legs_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_event& v);
    static ores::trading::messaging::swap_leg_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_event& v);
    static ores::trading::messaging::swap_leg_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_version_key& v);
    static ores::trading::messaging::swap_leg_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_version_key& v);
    static ores::trading::messaging::swap_leg_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::swap_leg_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::swap_leg_versions_filter& v);
    static ores::trading::messaging::swap_leg_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::swap_leg_versions_filter& v);
    static ores::trading::messaging::swap_leg_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_swap_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_swap_legs_request& v);
    static ores::trading::messaging::list_swap_legs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_swap_legs_request& v);
    static ores::trading::messaging::list_swap_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_swap_legs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_swap_legs_response& v);
    static ores::trading::messaging::list_swap_legs_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::list_swap_legs_response& v);
    static ores::trading::messaging::list_swap_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_swap_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_swap_leg_request& v);
    static ores::trading::messaging::get_swap_leg_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_swap_leg_request& v);
    static ores::trading::messaging::get_swap_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_swap_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_swap_leg_response& v);
    static ores::trading::messaging::get_swap_leg_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_swap_leg_response& v);
    static ores::trading::messaging::get_swap_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_swap_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_swap_legs_request& v);
    static ores::trading::messaging::get_many_swap_legs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_many_swap_legs_request& v);
    static ores::trading::messaging::get_many_swap_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_many_swap_legs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_many_swap_legs_response& v);
    static ores::trading::messaging::get_many_swap_legs_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_swap_legs_response& v);
    static ores::trading::messaging::get_many_swap_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_swap_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_swap_leg_request& v);
    static ores::trading::messaging::put_swap_leg_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_swap_leg_request& v);
    static ores::trading::messaging::put_swap_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_swap_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_swap_leg_response& v);
    static ores::trading::messaging::put_swap_leg_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_swap_leg_response& v);
    static ores::trading::messaging::put_swap_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_swap_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_swap_legs_request& v);
    static ores::trading::messaging::put_many_swap_legs_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_many_swap_legs_request& v);
    static ores::trading::messaging::put_many_swap_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_many_swap_legs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_many_swap_legs_response& v);
    static ores::trading::messaging::put_many_swap_legs_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_swap_legs_response& v);
    static ores::trading::messaging::put_many_swap_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_swap_leg_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_swap_leg_request& v);
    static ores::trading::messaging::delete_swap_leg_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_swap_leg_request& v);
    static ores::trading::messaging::delete_swap_leg_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_swap_leg_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_swap_leg_response& v);
    static ores::trading::messaging::delete_swap_leg_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::delete_swap_leg_response& v);
    static ores::trading::messaging::delete_swap_leg_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_swap_legs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_many_swap_legs_request& v);
    static ores::trading::messaging::delete_many_swap_legs_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_swap_legs_request& v);
    static ores::trading::messaging::delete_many_swap_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_swap_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_swap_legs_response& v);
    static ores::trading::messaging::delete_many_swap_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_swap_legs_response& v);
    static ores::trading::messaging::delete_many_swap_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_swap_legs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_swap_legs_request& v);
    static ores::trading::messaging::list_by_trade_id_swap_legs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_swap_legs_request& v);
    static ores::trading::messaging::list_by_trade_id_swap_legs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_swap_legs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_swap_legs_response& v);
    static ores::trading::messaging::list_by_trade_id_swap_legs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_swap_legs_response& v);
    static ores::trading::messaging::list_by_trade_id_swap_legs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_versions_request& v);
    static ores::trading::messaging::list_swap_leg_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_versions_request& v);
    static ores::trading::messaging::list_swap_leg_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_swap_leg_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_swap_leg_versions_response& v);
    static ores::trading::messaging::list_swap_leg_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_swap_leg_versions_response& v);
    static ores::trading::messaging::list_swap_leg_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_swap_leg_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_swap_leg_version_request& v);
    static ores::trading::messaging::get_swap_leg_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_version_request& v);
    static ores::trading::messaging::get_swap_leg_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_swap_leg_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_swap_leg_version_response& v);
    static ores::trading::messaging::get_swap_leg_version_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_swap_leg_version_response& v);
    static ores::trading::messaging::get_swap_leg_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
