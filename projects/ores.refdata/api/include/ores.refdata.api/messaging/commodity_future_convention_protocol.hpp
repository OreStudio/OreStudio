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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_CONVENTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_CONVENTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_future_convention.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct commodity_future_convention_key {
    std::string id;
};

struct commodity_future_convention_write {
    std::string id;
    std::string contract_frequency;
    std::string calendar;
    std::optional<std::string> expiry_calendar;
    std::optional<int> expiry_month_lag;
    std::optional<std::string> one_contract_month;
    std::optional<int> offset_days;
    std::optional<std::string> business_day_convention;
    std::optional<bool> adjust_before_offset;
    std::optional<bool> is_averaging;
    std::optional<std::string> valid_contract_months;
    std::optional<int> anchor_day_of_month;
    std::optional<int> anchor_calendar_days_before;
    std::optional<int> anchor_business_days_after;
    std::optional<int> anchor_nth_nth;
    std::optional<std::string> anchor_nth_weekday;
    std::optional<std::string> anchor_last_weekday;
    std::optional<std::string> anchor_weekly_day_of_the_week;
    std::optional<int> option_expiry_month_lag;
    std::optional<std::string> option_contract_frequency;
    std::optional<int> option_expiry_offset;
    std::optional<int> option_calendar_days_before;
    std::optional<int> option_min_business_days_before;
    std::optional<int> option_expiry_day;
    std::optional<int> option_nth_nth;
    std::optional<std::string> option_nth_weekday;
    std::optional<std::string> option_expiry_last_weekday_of_month;
    std::optional<std::string> option_expiry_weekly_day_of_the_week;
    std::optional<std::string> option_business_day_convention;
    std::optional<int> hours_per_day;
    std::optional<std::string> off_peak_index;
    std::optional<std::string> peak_index;
    std::optional<double> off_peak_hours;
    std::optional<std::string> peak_calendar;
    std::optional<std::string> index_name;
    std::optional<std::string> savings_time;
    std::optional<std::string> delivery_location;
    std::optional<bool> balance_of_the_month;
    std::optional<std::string> balance_of_the_month_pricing_calendar;
    std::optional<std::string> option_underlying_future_convention;
    std::optional<std::string> averaging_commodity_name;
    std::optional<std::string> averaging_period;
    std::optional<std::string> averaging_pricing_calendar;
    std::optional<std::string> averaging_conventions;
    std::optional<bool> averaging_use_business_days;
    std::optional<int> averaging_delivery_roll_days;
    std::optional<int> averaging_future_month_offset;
    std::optional<int> averaging_daily_expiry_offset;
    std::optional<std::string> prohibited_expiries;
    std::optional<std::string> future_continuation_mappings;
    std::optional<std::string> option_continuation_mappings;
    std::optional<std::string> oresmd_uri;
};

struct commodity_future_convention_change {
    commodity_future_convention_write write;
    ores::utility::domain::precondition precondition;
};

struct commodity_future_convention_removal {
    commodity_future_convention_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct commodity_future_convention_lookup {
    commodity_future_convention_key key;
    std::optional<ores::refdata::domain::commodity_future_convention> commodity_future_convention;
};

struct commodity_future_conventions_filter {
    std::optional<std::vector<std::string>> id_one_of;
};

struct commodity_future_convention_event {
    boost::uuids::uuid event_id;
    commodity_future_convention_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct commodity_future_convention_version_key {
    commodity_future_convention_key commodity_future_convention;
    std::uint32_t version;
};

struct commodity_future_convention_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_commodity_future_conventions_request {
    using response_type = struct list_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_future_conventions.list";
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
    std::optional<commodity_future_conventions_filter> filter;
    std::optional<std::string> as_of;
};

struct list_commodity_future_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_convention> commodity_future_conventions;
    std::uint64_t total;
};

struct get_commodity_future_convention_request {
    using response_type = struct get_commodity_future_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_future_conventions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_key key;
};

struct get_commodity_future_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_convention> commodity_future_convention;
};

struct get_many_commodity_future_conventions_request {
    using response_type = struct get_many_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_convention_key> keys;
};

struct get_many_commodity_future_conventions_response {
    ores::utility::domain::result result;
    std::vector<commodity_future_convention_lookup> entries;
};

struct put_commodity_future_convention_request {
    using response_type = struct put_commodity_future_convention_response;
    static constexpr std::string_view nats_subject = "refdata.v1.commodity_future_conventions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_change change;
    ores::utility::domain::change_intent intent;
};

struct put_commodity_future_convention_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_convention> commodity_future_convention;
};

struct put_many_commodity_future_conventions_request {
    using response_type = struct put_many_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_convention_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_commodity_future_conventions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_convention> commodity_future_conventions;
};

struct delete_commodity_future_convention_request {
    using response_type = struct delete_commodity_future_convention_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_commodity_future_convention_response {
    ores::utility::domain::result result;
};

struct delete_many_commodity_future_conventions_request {
    using response_type = struct delete_many_commodity_future_conventions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_convention_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_commodity_future_conventions_response {
    ores::utility::domain::result result;
};

struct list_commodity_future_convention_versions_request {
    using response_type = struct list_commodity_future_convention_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_future_convention_versions_filter> filter;
};

struct list_commodity_future_convention_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_convention> versions;
    std::uint64_t total;
};

struct get_commodity_future_convention_version_request {
    using response_type = struct get_commodity_future_convention_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_conventions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_convention_version_key key;
};

struct get_commodity_future_convention_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_convention> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_future_convention_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.commodity_future_conventions_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.commodity_future_conventions_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.commodity_future_conventions_events.deleted";
}
/**
 * @brief The members of commodity_future_convention_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_key&) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_convention_key::id},
    };
}

/**
 * @brief The members of commodity_future_convention_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_write&) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_convention_write::id},
        utility::codec::field{"contract_frequency",
                              &commodity_future_convention_write::contract_frequency},
        utility::codec::field{"calendar", &commodity_future_convention_write::calendar},
        utility::codec::field{"expiry_calendar",
                              &commodity_future_convention_write::expiry_calendar},
        utility::codec::field{"expiry_month_lag",
                              &commodity_future_convention_write::expiry_month_lag},
        utility::codec::field{"one_contract_month",
                              &commodity_future_convention_write::one_contract_month},
        utility::codec::field{"offset_days", &commodity_future_convention_write::offset_days},
        utility::codec::field{"business_day_convention",
                              &commodity_future_convention_write::business_day_convention},
        utility::codec::field{"adjust_before_offset",
                              &commodity_future_convention_write::adjust_before_offset},
        utility::codec::field{"is_averaging", &commodity_future_convention_write::is_averaging},
        utility::codec::field{"valid_contract_months",
                              &commodity_future_convention_write::valid_contract_months},
        utility::codec::field{"anchor_day_of_month",
                              &commodity_future_convention_write::anchor_day_of_month},
        utility::codec::field{"anchor_calendar_days_before",
                              &commodity_future_convention_write::anchor_calendar_days_before},
        utility::codec::field{"anchor_business_days_after",
                              &commodity_future_convention_write::anchor_business_days_after},
        utility::codec::field{"anchor_nth_nth", &commodity_future_convention_write::anchor_nth_nth},
        utility::codec::field{"anchor_nth_weekday",
                              &commodity_future_convention_write::anchor_nth_weekday},
        utility::codec::field{"anchor_last_weekday",
                              &commodity_future_convention_write::anchor_last_weekday},
        utility::codec::field{"anchor_weekly_day_of_the_week",
                              &commodity_future_convention_write::anchor_weekly_day_of_the_week},
        utility::codec::field{"option_expiry_month_lag",
                              &commodity_future_convention_write::option_expiry_month_lag},
        utility::codec::field{"option_contract_frequency",
                              &commodity_future_convention_write::option_contract_frequency},
        utility::codec::field{"option_expiry_offset",
                              &commodity_future_convention_write::option_expiry_offset},
        utility::codec::field{"option_calendar_days_before",
                              &commodity_future_convention_write::option_calendar_days_before},
        utility::codec::field{"option_min_business_days_before",
                              &commodity_future_convention_write::option_min_business_days_before},
        utility::codec::field{"option_expiry_day",
                              &commodity_future_convention_write::option_expiry_day},
        utility::codec::field{"option_nth_nth", &commodity_future_convention_write::option_nth_nth},
        utility::codec::field{"option_nth_weekday",
                              &commodity_future_convention_write::option_nth_weekday},
        utility::codec::field{
            "option_expiry_last_weekday_of_month",
            &commodity_future_convention_write::option_expiry_last_weekday_of_month},
        utility::codec::field{
            "option_expiry_weekly_day_of_the_week",
            &commodity_future_convention_write::option_expiry_weekly_day_of_the_week},
        utility::codec::field{"option_business_day_convention",
                              &commodity_future_convention_write::option_business_day_convention},
        utility::codec::field{"hours_per_day", &commodity_future_convention_write::hours_per_day},
        utility::codec::field{"off_peak_index", &commodity_future_convention_write::off_peak_index},
        utility::codec::field{"peak_index", &commodity_future_convention_write::peak_index},
        utility::codec::field{"off_peak_hours", &commodity_future_convention_write::off_peak_hours},
        utility::codec::field{"peak_calendar", &commodity_future_convention_write::peak_calendar},
        utility::codec::field{"index_name", &commodity_future_convention_write::index_name},
        utility::codec::field{"savings_time", &commodity_future_convention_write::savings_time},
        utility::codec::field{"delivery_location",
                              &commodity_future_convention_write::delivery_location},
        utility::codec::field{"balance_of_the_month",
                              &commodity_future_convention_write::balance_of_the_month},
        utility::codec::field{
            "balance_of_the_month_pricing_calendar",
            &commodity_future_convention_write::balance_of_the_month_pricing_calendar},
        utility::codec::field{
            "option_underlying_future_convention",
            &commodity_future_convention_write::option_underlying_future_convention},
        utility::codec::field{"averaging_commodity_name",
                              &commodity_future_convention_write::averaging_commodity_name},
        utility::codec::field{"averaging_period",
                              &commodity_future_convention_write::averaging_period},
        utility::codec::field{"averaging_pricing_calendar",
                              &commodity_future_convention_write::averaging_pricing_calendar},
        utility::codec::field{"averaging_conventions",
                              &commodity_future_convention_write::averaging_conventions},
        utility::codec::field{"averaging_use_business_days",
                              &commodity_future_convention_write::averaging_use_business_days},
        utility::codec::field{"averaging_delivery_roll_days",
                              &commodity_future_convention_write::averaging_delivery_roll_days},
        utility::codec::field{"averaging_future_month_offset",
                              &commodity_future_convention_write::averaging_future_month_offset},
        utility::codec::field{"averaging_daily_expiry_offset",
                              &commodity_future_convention_write::averaging_daily_expiry_offset},
        utility::codec::field{"prohibited_expiries",
                              &commodity_future_convention_write::prohibited_expiries},
        utility::codec::field{"future_continuation_mappings",
                              &commodity_future_convention_write::future_continuation_mappings},
        utility::codec::field{"option_continuation_mappings",
                              &commodity_future_convention_write::option_continuation_mappings},
        utility::codec::field{"oresmd_uri", &commodity_future_convention_write::oresmd_uri},
    };
}

/**
 * @brief The members of commodity_future_convention_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_change&) {
    return std::tuple{
        utility::codec::field{"write", &commodity_future_convention_change::write},
        utility::codec::field{"precondition", &commodity_future_convention_change::precondition},
    };
}

/**
 * @brief The members of commodity_future_convention_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_removal&) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_convention_removal::key},
        utility::codec::field{"precondition", &commodity_future_convention_removal::precondition},
    };
}

/**
 * @brief The members of commodity_future_convention_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_convention_lookup::key},
        utility::codec::field{"commodity_future_convention",
                              &commodity_future_convention_lookup::commodity_future_convention},
    };
}

/**
 * @brief The members of commodity_future_conventions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_conventions_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &commodity_future_conventions_filter::id_one_of},
    };
}

/**
 * @brief The members of commodity_future_convention_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &commodity_future_convention_event::event_id},
        utility::codec::field{"key", &commodity_future_convention_event::key},
        utility::codec::field{"action", &commodity_future_convention_event::action},
        utility::codec::field{"version", &commodity_future_convention_event::version},
        utility::codec::field{"occurred_at", &commodity_future_convention_event::occurred_at},
        utility::codec::field{"correlation_id", &commodity_future_convention_event::correlation_id},
    };
}

/**
 * @brief The members of commodity_future_convention_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_version_key&) {
    return std::tuple{
        utility::codec::field{
            "commodity_future_convention",
            &commodity_future_convention_version_key::commodity_future_convention},
        utility::codec::field{"version", &commodity_future_convention_version_key::version},
    };
}

/**
 * @brief The members of commodity_future_convention_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const commodity_future_convention_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_convention_versions_filter::version},
        utility::codec::field{"from_version",
                              &commodity_future_convention_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &commodity_future_convention_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_commodity_future_conventions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_commodity_future_conventions_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_commodity_future_conventions_request::offset},
        utility::codec::field{"limit", &list_commodity_future_conventions_request::limit},
        utility::codec::field{"order", &list_commodity_future_conventions_request::order},
        utility::codec::field{"filter", &list_commodity_future_conventions_request::filter},
        utility::codec::field{"as_of", &list_commodity_future_conventions_request::as_of},
    };
}

/**
 * @brief The members of list_commodity_future_conventions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_commodity_future_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_commodity_future_conventions_response::result},
        utility::codec::field{
            "commodity_future_conventions",
            &list_commodity_future_conventions_response::commodity_future_conventions},
        utility::codec::field{"total", &list_commodity_future_conventions_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_convention_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_convention_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_convention_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_convention_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_commodity_future_convention_response::result},
        utility::codec::field{
            "commodity_future_convention",
            &get_commodity_future_convention_response::commodity_future_convention},
    };
}

/**
 * @brief The members of get_many_commodity_future_conventions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_commodity_future_conventions_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_commodity_future_conventions_request::keys},
    };
}

/**
 * @brief The members of get_many_commodity_future_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_commodity_future_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_commodity_future_conventions_response::result},
        utility::codec::field{"entries", &get_many_commodity_future_conventions_response::entries},
    };
}

/**
 * @brief The members of put_commodity_future_convention_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_commodity_future_convention_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_commodity_future_convention_request::change},
        utility::codec::field{"intent", &put_commodity_future_convention_request::intent},
    };
}

/**
 * @brief The members of put_commodity_future_convention_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_commodity_future_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_commodity_future_convention_response::result},
        utility::codec::field{
            "commodity_future_convention",
            &put_commodity_future_convention_response::commodity_future_convention},
    };
}

/**
 * @brief The members of put_many_commodity_future_conventions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_commodity_future_conventions_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_commodity_future_conventions_request::changes},
        utility::codec::field{"intent", &put_many_commodity_future_conventions_request::intent},
    };
}

/**
 * @brief The members of put_many_commodity_future_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_commodity_future_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_commodity_future_conventions_response::result},
        utility::codec::field{
            "commodity_future_conventions",
            &put_many_commodity_future_conventions_response::commodity_future_conventions},
    };
}

/**
 * @brief The members of delete_commodity_future_convention_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_commodity_future_convention_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_commodity_future_convention_request::removal},
        utility::codec::field{"intent", &delete_commodity_future_convention_request::intent},
    };
}

/**
 * @brief The members of delete_commodity_future_convention_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_commodity_future_convention_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_commodity_future_convention_response::result},
    };
}

/**
 * @brief The members of delete_many_commodity_future_conventions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_commodity_future_conventions_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_commodity_future_conventions_request::removals},
        utility::codec::field{"intent", &delete_many_commodity_future_conventions_request::intent},
    };
}

/**
 * @brief The members of delete_many_commodity_future_conventions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_commodity_future_conventions_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_commodity_future_conventions_response::result},
    };
}

/**
 * @brief The members of list_commodity_future_convention_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_commodity_future_convention_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_commodity_future_convention_versions_request::key},
        utility::codec::field{"offset", &list_commodity_future_convention_versions_request::offset},
        utility::codec::field{"limit", &list_commodity_future_convention_versions_request::limit},
        utility::codec::field{"order", &list_commodity_future_convention_versions_request::order},
        utility::codec::field{"filter", &list_commodity_future_convention_versions_request::filter},
    };
}

/**
 * @brief The members of list_commodity_future_convention_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_commodity_future_convention_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_commodity_future_convention_versions_response::result},
        utility::codec::field{"versions",
                              &list_commodity_future_convention_versions_response::versions},
        utility::codec::field{"total", &list_commodity_future_convention_versions_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_convention_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_convention_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_convention_version_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_convention_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_commodity_future_convention_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_commodity_future_convention_version_response::result},
        utility::codec::field{"version",
                              &get_commodity_future_convention_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_key& v);
    static ores::refdata::messaging::commodity_future_convention_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_key& v);
    static ores::refdata::messaging::commodity_future_convention_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_write& v);
    static ores::refdata::messaging::commodity_future_convention_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_write& v);
    static ores::refdata::messaging::commodity_future_convention_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_change& v);
    static ores::refdata::messaging::commodity_future_convention_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_change& v);
    static ores::refdata::messaging::commodity_future_convention_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_removal& v);
    static ores::refdata::messaging::commodity_future_convention_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_removal& v);
    static ores::refdata::messaging::commodity_future_convention_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_lookup& v);
    static ores::refdata::messaging::commodity_future_convention_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_lookup& v);
    static ores::refdata::messaging::commodity_future_convention_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_conventions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_conventions_filter& v);
    static ores::refdata::messaging::commodity_future_conventions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_conventions_filter& v);
    static ores::refdata::messaging::commodity_future_conventions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_event& v);
    static ores::refdata::messaging::commodity_future_convention_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_event& v);
    static ores::refdata::messaging::commodity_future_convention_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_version_key& v);
    static ores::refdata::messaging::commodity_future_convention_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_version_key& v);
    static ores::refdata::messaging::commodity_future_convention_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_convention_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_convention_versions_filter& v);
    static ores::refdata::messaging::commodity_future_convention_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_convention_versions_filter& v);
    static ores::refdata::messaging::commodity_future_convention_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_conventions_request& v);
    static ores::refdata::messaging::list_commodity_future_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_future_conventions_request& v);
    static ores::refdata::messaging::list_commodity_future_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_conventions_response& v);
    static ores::refdata::messaging::list_commodity_future_conventions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_future_conventions_response& v);
    static ores::refdata::messaging::list_commodity_future_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_convention_request& v);
    static ores::refdata::messaging::get_commodity_future_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_convention_request& v);
    static ores::refdata::messaging::get_commodity_future_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_convention_response& v);
    static ores::refdata::messaging::get_commodity_future_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_convention_response& v);
    static ores::refdata::messaging::get_commodity_future_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::get_many_commodity_future_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::get_many_commodity_future_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::get_many_commodity_future_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::get_many_commodity_future_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_convention_request& v);
    static ores::refdata::messaging::put_commodity_future_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_convention_request& v);
    static ores::refdata::messaging::put_commodity_future_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_convention_response& v);
    static ores::refdata::messaging::put_commodity_future_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_convention_response& v);
    static ores::refdata::messaging::put_commodity_future_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::put_many_commodity_future_conventions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::put_many_commodity_future_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::put_many_commodity_future_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::put_many_commodity_future_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_convention_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_convention_request& v);
    static ores::refdata::messaging::delete_commodity_future_convention_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_commodity_future_convention_request& v);
    static ores::refdata::messaging::delete_commodity_future_convention_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_convention_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_convention_response& v);
    static ores::refdata::messaging::delete_commodity_future_convention_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_commodity_future_convention_response& v);
    static ores::refdata::messaging::delete_commodity_future_convention_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_conventions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_conventions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_conventions_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_conventions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_conventions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_conventions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_conventions_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_conventions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_convention_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_convention_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_convention_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_convention_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_convention_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_convention_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_convention_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_convention_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_convention_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_convention_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_convention_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_convention_version_request& v);
    static ores::refdata::messaging::get_commodity_future_convention_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_convention_version_request& v);
    static ores::refdata::messaging::get_commodity_future_convention_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_convention_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_convention_version_response& v);
    static ores::refdata::messaging::get_commodity_future_convention_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_convention_version_response& v);
    static ores::refdata::messaging::get_commodity_future_convention_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
