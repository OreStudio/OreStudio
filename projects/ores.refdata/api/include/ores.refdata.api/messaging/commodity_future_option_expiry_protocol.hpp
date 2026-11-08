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
#ifndef ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_OPTION_EXPIRY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COMMODITY_FUTURE_OPTION_EXPIRY_PROTOCOL_HPP

#include "ores.refdata.api/domain/commodity_future_option_expiry_codec.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct commodity_future_option_expiry_key {
    boost::uuids::uuid id;

    friend bool operator==(const commodity_future_option_expiry_key&,
                           const commodity_future_option_expiry_key&) = default;
};

struct commodity_future_option_expiry_write {
    boost::uuids::uuid id;
    boost::uuids::uuid convention_id;
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
    std::optional<std::string> option_underlying_future_convention;
    std::optional<std::string> option_continuation_mappings;

    friend bool operator==(const commodity_future_option_expiry_write&,
                           const commodity_future_option_expiry_write&) = default;
};

struct commodity_future_option_expiry_change {
    commodity_future_option_expiry_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const commodity_future_option_expiry_change&,
                           const commodity_future_option_expiry_change&) = default;
};

struct commodity_future_option_expiry_removal {
    commodity_future_option_expiry_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const commodity_future_option_expiry_removal&,
                           const commodity_future_option_expiry_removal&) = default;
};

struct commodity_future_option_expiry_lookup {
    commodity_future_option_expiry_key key;
    std::optional<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiry;

    friend bool operator==(const commodity_future_option_expiry_lookup&,
                           const commodity_future_option_expiry_lookup&) = default;
};

struct commodity_future_option_expiries_filter {
    std::optional<boost::uuids::uuid> convention_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> convention_id_one_of;

    friend bool operator==(const commodity_future_option_expiries_filter&,
                           const commodity_future_option_expiries_filter&) = default;
};

struct commodity_future_option_expiry_event {
    boost::uuids::uuid event_id;
    commodity_future_option_expiry_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const commodity_future_option_expiry_event&,
                           const commodity_future_option_expiry_event&) = default;
};

struct commodity_future_option_expiry_version_key {
    commodity_future_option_expiry_key commodity_future_option_expiry;
    std::uint32_t version{};

    friend bool operator==(const commodity_future_option_expiry_version_key&,
                           const commodity_future_option_expiry_version_key&) = default;
};

struct commodity_future_option_expiry_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const commodity_future_option_expiry_versions_filter&,
                           const commodity_future_option_expiry_versions_filter&) = default;
};

struct list_commodity_future_option_expiries_request {
    using response_type = struct list_commodity_future_option_expiries_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.list";
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
    std::optional<commodity_future_option_expiries_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_commodity_future_option_expiries_request&,
                           const list_commodity_future_option_expiries_request&) = default;
};

struct list_commodity_future_option_expiries_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiries;
    std::uint64_t total{};

    friend bool operator==(const list_commodity_future_option_expiries_response&,
                           const list_commodity_future_option_expiries_response&) = default;
};

struct get_commodity_future_option_expiry_request {
    using response_type = struct get_commodity_future_option_expiry_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_option_expiry_key key;

    friend bool operator==(const get_commodity_future_option_expiry_request&,
                           const get_commodity_future_option_expiry_request&) = default;
};

struct get_commodity_future_option_expiry_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiry;

    friend bool operator==(const get_commodity_future_option_expiry_response&,
                           const get_commodity_future_option_expiry_response&) = default;
};

struct get_many_commodity_future_option_expiries_request {
    using response_type = struct get_many_commodity_future_option_expiries_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_option_expiry_key> keys;

    friend bool operator==(const get_many_commodity_future_option_expiries_request&,
                           const get_many_commodity_future_option_expiries_request&) = default;
};

struct get_many_commodity_future_option_expiries_response {
    ores::utility::domain::result result;
    std::vector<commodity_future_option_expiry_lookup> entries;

    friend bool operator==(const get_many_commodity_future_option_expiries_response&,
                           const get_many_commodity_future_option_expiries_response&) = default;
};

struct put_commodity_future_option_expiry_request {
    using response_type = struct put_commodity_future_option_expiry_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_option_expiry_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_commodity_future_option_expiry_request&,
                           const put_commodity_future_option_expiry_request&) = default;
};

struct put_commodity_future_option_expiry_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiry;

    friend bool operator==(const put_commodity_future_option_expiry_response&,
                           const put_commodity_future_option_expiry_response&) = default;
};

struct put_many_commodity_future_option_expiries_request {
    using response_type = struct put_many_commodity_future_option_expiries_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_option_expiry_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_commodity_future_option_expiries_request&,
                           const put_many_commodity_future_option_expiries_request&) = default;
};

struct put_many_commodity_future_option_expiries_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiries;

    friend bool operator==(const put_many_commodity_future_option_expiries_response&,
                           const put_many_commodity_future_option_expiries_response&) = default;
};

struct delete_commodity_future_option_expiry_request {
    using response_type = struct delete_commodity_future_option_expiry_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_option_expiry_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_commodity_future_option_expiry_request&,
                           const delete_commodity_future_option_expiry_request&) = default;
};

struct delete_commodity_future_option_expiry_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_commodity_future_option_expiry_response&,
                           const delete_commodity_future_option_expiry_response&) = default;
};

struct delete_many_commodity_future_option_expiries_request {
    using response_type = struct delete_many_commodity_future_option_expiries_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<commodity_future_option_expiry_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_commodity_future_option_expiries_request&,
                           const delete_many_commodity_future_option_expiries_request&) = default;
};

struct delete_many_commodity_future_option_expiries_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_commodity_future_option_expiries_response&,
                           const delete_many_commodity_future_option_expiries_response&) = default;
};

struct list_by_convention_id_commodity_future_option_expiries_request {
    using response_type = struct list_by_convention_id_commodity_future_option_expiries_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries.list_by_convention_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid convention_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_future_option_expiries_filter> filter;

    friend bool
    operator==(const list_by_convention_id_commodity_future_option_expiries_request&,
               const list_by_convention_id_commodity_future_option_expiries_request&) = default;
};

struct list_by_convention_id_commodity_future_option_expiries_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_option_expiry>
        commodity_future_option_expiries;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_convention_id_commodity_future_option_expiries_response&,
               const list_by_convention_id_commodity_future_option_expiries_response&) = default;
};

struct list_commodity_future_option_expiry_versions_request {
    using response_type = struct list_commodity_future_option_expiry_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_option_expiry_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<commodity_future_option_expiry_versions_filter> filter;

    friend bool operator==(const list_commodity_future_option_expiry_versions_request&,
                           const list_commodity_future_option_expiry_versions_request&) = default;
};

struct list_commodity_future_option_expiry_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::commodity_future_option_expiry> versions;
    std::uint64_t total{};

    friend bool operator==(const list_commodity_future_option_expiry_versions_response&,
                           const list_commodity_future_option_expiry_versions_response&) = default;
};

struct get_commodity_future_option_expiry_version_request {
    using response_type = struct get_commodity_future_option_expiry_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.commodity_future_option_expiries_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    commodity_future_option_expiry_version_key key;

    friend bool operator==(const get_commodity_future_option_expiry_version_request&,
                           const get_commodity_future_option_expiry_version_request&) = default;
};

struct get_commodity_future_option_expiry_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::commodity_future_option_expiry> version;

    friend bool operator==(const get_commodity_future_option_expiry_version_response&,
                           const get_commodity_future_option_expiry_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace commodity_future_option_expiry_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.events.commodity_future_option_expiries.created";
inline constexpr std::string_view updated =
    "refdata.v1.events.commodity_future_option_expiries.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.events.commodity_future_option_expiries.deleted";
}
/**
 * @brief The members of commodity_future_option_expiry_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_future_option_expiry_key>) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_option_expiry_key::id},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_future_option_expiry_write>) {
    return std::tuple{
        utility::codec::field{"id", &commodity_future_option_expiry_write::id},
        utility::codec::field{"convention_id",
                              &commodity_future_option_expiry_write::convention_id},
        utility::codec::field{"option_expiry_month_lag",
                              &commodity_future_option_expiry_write::option_expiry_month_lag},
        utility::codec::field{"option_contract_frequency",
                              &commodity_future_option_expiry_write::option_contract_frequency},
        utility::codec::field{"option_expiry_offset",
                              &commodity_future_option_expiry_write::option_expiry_offset},
        utility::codec::field{"option_calendar_days_before",
                              &commodity_future_option_expiry_write::option_calendar_days_before},
        utility::codec::field{
            "option_min_business_days_before",
            &commodity_future_option_expiry_write::option_min_business_days_before},
        utility::codec::field{"option_expiry_day",
                              &commodity_future_option_expiry_write::option_expiry_day},
        utility::codec::field{"option_nth_nth",
                              &commodity_future_option_expiry_write::option_nth_nth},
        utility::codec::field{"option_nth_weekday",
                              &commodity_future_option_expiry_write::option_nth_weekday},
        utility::codec::field{
            "option_expiry_last_weekday_of_month",
            &commodity_future_option_expiry_write::option_expiry_last_weekday_of_month},
        utility::codec::field{
            "option_expiry_weekly_day_of_the_week",
            &commodity_future_option_expiry_write::option_expiry_weekly_day_of_the_week},
        utility::codec::field{
            "option_business_day_convention",
            &commodity_future_option_expiry_write::option_business_day_convention},
        utility::codec::field{
            "option_underlying_future_convention",
            &commodity_future_option_expiry_write::option_underlying_future_convention},
        utility::codec::field{"option_continuation_mappings",
                              &commodity_future_option_expiry_write::option_continuation_mappings},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiry_change>) {
    return std::tuple{
        utility::codec::field{"write", &commodity_future_option_expiry_change::write},
        utility::codec::field{"precondition", &commodity_future_option_expiry_change::precondition},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiry_removal>) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_option_expiry_removal::key},
        utility::codec::field{"precondition",
                              &commodity_future_option_expiry_removal::precondition},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiry_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &commodity_future_option_expiry_lookup::key},
        utility::codec::field{
            "commodity_future_option_expiry",
            &commodity_future_option_expiry_lookup::commodity_future_option_expiry},
    };
}

/**
 * @brief The members of commodity_future_option_expiries_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiries_filter>) {
    return std::tuple{
        utility::codec::field{"convention_id",
                              &commodity_future_option_expiries_filter::convention_id},
        utility::codec::field{"id_one_of", &commodity_future_option_expiries_filter::id_one_of},
        utility::codec::field{"convention_id_one_of",
                              &commodity_future_option_expiries_filter::convention_id_one_of},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<commodity_future_option_expiry_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &commodity_future_option_expiry_event::event_id},
        utility::codec::field{"key", &commodity_future_option_expiry_event::key},
        utility::codec::field{"action", &commodity_future_option_expiry_event::action},
        utility::codec::field{"version", &commodity_future_option_expiry_event::version},
        utility::codec::field{"occurred_at", &commodity_future_option_expiry_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &commodity_future_option_expiry_event::correlation_id},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiry_version_key>) {
    return std::tuple{
        utility::codec::field{
            "commodity_future_option_expiry",
            &commodity_future_option_expiry_version_key::commodity_future_option_expiry},
        utility::codec::field{"version", &commodity_future_option_expiry_version_key::version},
    };
}

/**
 * @brief The members of commodity_future_option_expiry_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<commodity_future_option_expiry_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &commodity_future_option_expiry_versions_filter::version},
        utility::codec::field{"from_version",
                              &commodity_future_option_expiry_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &commodity_future_option_expiry_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_commodity_future_option_expiries_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_future_option_expiries_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_commodity_future_option_expiries_request::offset},
        utility::codec::field{"limit", &list_commodity_future_option_expiries_request::limit},
        utility::codec::field{"order", &list_commodity_future_option_expiries_request::order},
        utility::codec::field{"filter", &list_commodity_future_option_expiries_request::filter},
        utility::codec::field{"as_of", &list_commodity_future_option_expiries_request::as_of},
    };
}

/**
 * @brief The members of list_commodity_future_option_expiries_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_future_option_expiries_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_commodity_future_option_expiries_response::result},
        utility::codec::field{
            "commodity_future_option_expiries",
            &list_commodity_future_option_expiries_response::commodity_future_option_expiries},
        utility::codec::field{"total", &list_commodity_future_option_expiries_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_option_expiry_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_future_option_expiry_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_option_expiry_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_option_expiry_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_future_option_expiry_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_commodity_future_option_expiry_response::result},
        utility::codec::field{
            "commodity_future_option_expiry",
            &get_commodity_future_option_expiry_response::commodity_future_option_expiry},
    };
}

/**
 * @brief The members of get_many_commodity_future_option_expiries_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_commodity_future_option_expiries_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_commodity_future_option_expiries_request::keys},
    };
}

/**
 * @brief The members of get_many_commodity_future_option_expiries_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_commodity_future_option_expiries_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_commodity_future_option_expiries_response::result},
        utility::codec::field{"entries",
                              &get_many_commodity_future_option_expiries_response::entries},
    };
}

/**
 * @brief The members of put_commodity_future_option_expiry_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_commodity_future_option_expiry_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_commodity_future_option_expiry_request::change},
        utility::codec::field{"intent", &put_commodity_future_option_expiry_request::intent},
    };
}

/**
 * @brief The members of put_commodity_future_option_expiry_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_commodity_future_option_expiry_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_commodity_future_option_expiry_response::result},
        utility::codec::field{
            "commodity_future_option_expiry",
            &put_commodity_future_option_expiry_response::commodity_future_option_expiry},
    };
}

/**
 * @brief The members of put_many_commodity_future_option_expiries_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_commodity_future_option_expiries_request>) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_commodity_future_option_expiries_request::changes},
        utility::codec::field{"intent", &put_many_commodity_future_option_expiries_request::intent},
    };
}

/**
 * @brief The members of put_many_commodity_future_option_expiries_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_commodity_future_option_expiries_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_commodity_future_option_expiries_response::result},
        utility::codec::field{
            "commodity_future_option_expiries",
            &put_many_commodity_future_option_expiries_response::commodity_future_option_expiries},
    };
}

/**
 * @brief The members of delete_commodity_future_option_expiry_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_commodity_future_option_expiry_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_commodity_future_option_expiry_request::removal},
        utility::codec::field{"intent", &delete_commodity_future_option_expiry_request::intent},
    };
}

/**
 * @brief The members of delete_commodity_future_option_expiry_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_commodity_future_option_expiry_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_commodity_future_option_expiry_response::result},
    };
}

/**
 * @brief The members of delete_many_commodity_future_option_expiries_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_commodity_future_option_expiries_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_commodity_future_option_expiries_request::removals},
        utility::codec::field{"intent",
                              &delete_many_commodity_future_option_expiries_request::intent},
    };
}

/**
 * @brief The members of delete_many_commodity_future_option_expiries_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_commodity_future_option_expiries_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_commodity_future_option_expiries_response::result},
    };
}

/**
 * @brief The members of list_by_convention_id_commodity_future_option_expiries_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_convention_id_commodity_future_option_expiries_request>) {
    return std::tuple{
        utility::codec::field{
            "convention_id",
            &list_by_convention_id_commodity_future_option_expiries_request::convention_id},
        utility::codec::field{
            "scope", &list_by_convention_id_commodity_future_option_expiries_request::scope},
        utility::codec::field{
            "offset", &list_by_convention_id_commodity_future_option_expiries_request::offset},
        utility::codec::field{
            "limit", &list_by_convention_id_commodity_future_option_expiries_request::limit},
        utility::codec::field{
            "order", &list_by_convention_id_commodity_future_option_expiries_request::order},
        utility::codec::field{
            "filter", &list_by_convention_id_commodity_future_option_expiries_request::filter},
    };
}

/**
 * @brief The members of list_by_convention_id_commodity_future_option_expiries_response, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_convention_id_commodity_future_option_expiries_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_convention_id_commodity_future_option_expiries_response::result},
        utility::codec::field{"commodity_future_option_expiries",
                              &list_by_convention_id_commodity_future_option_expiries_response::
                                  commodity_future_option_expiries},
        utility::codec::field{
            "total", &list_by_convention_id_commodity_future_option_expiries_response::total},
    };
}

/**
 * @brief The members of list_commodity_future_option_expiry_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_future_option_expiry_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_commodity_future_option_expiry_versions_request::key},
        utility::codec::field{"offset",
                              &list_commodity_future_option_expiry_versions_request::offset},
        utility::codec::field{"limit",
                              &list_commodity_future_option_expiry_versions_request::limit},
        utility::codec::field{"order",
                              &list_commodity_future_option_expiry_versions_request::order},
        utility::codec::field{"filter",
                              &list_commodity_future_option_expiry_versions_request::filter},
    };
}

/**
 * @brief The members of list_commodity_future_option_expiry_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_commodity_future_option_expiry_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_commodity_future_option_expiry_versions_response::result},
        utility::codec::field{"versions",
                              &list_commodity_future_option_expiry_versions_response::versions},
        utility::codec::field{"total",
                              &list_commodity_future_option_expiry_versions_response::total},
    };
}

/**
 * @brief The members of get_commodity_future_option_expiry_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_future_option_expiry_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_commodity_future_option_expiry_version_request::key},
    };
}

/**
 * @brief The members of get_commodity_future_option_expiry_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_commodity_future_option_expiry_version_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_commodity_future_option_expiry_version_response::result},
        utility::codec::field{"version",
                              &get_commodity_future_option_expiry_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_key& v);
    static ores::refdata::messaging::commodity_future_option_expiry_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_key& v);
    static ores::refdata::messaging::commodity_future_option_expiry_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_write& v);
    static ores::refdata::messaging::commodity_future_option_expiry_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_write& v);
    static ores::refdata::messaging::commodity_future_option_expiry_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_change& v);
    static ores::refdata::messaging::commodity_future_option_expiry_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_change& v);
    static ores::refdata::messaging::commodity_future_option_expiry_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_removal& v);
    static ores::refdata::messaging::commodity_future_option_expiry_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_removal& v);
    static ores::refdata::messaging::commodity_future_option_expiry_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_lookup& v);
    static ores::refdata::messaging::commodity_future_option_expiry_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_lookup& v);
    static ores::refdata::messaging::commodity_future_option_expiry_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiries_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiries_filter& v);
    static ores::refdata::messaging::commodity_future_option_expiries_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiries_filter& v);
    static ores::refdata::messaging::commodity_future_option_expiries_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_event& v);
    static ores::refdata::messaging::commodity_future_option_expiry_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_event& v);
    static ores::refdata::messaging::commodity_future_option_expiry_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_version_key& v);
    static ores::refdata::messaging::commodity_future_option_expiry_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::commodity_future_option_expiry_version_key& v);
    static ores::refdata::messaging::commodity_future_option_expiry_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::commodity_future_option_expiry_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::commodity_future_option_expiry_versions_filter& v);
    static ores::refdata::messaging::commodity_future_option_expiry_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::commodity_future_option_expiry_versions_filter& v);
    static ores::refdata::messaging::commodity_future_option_expiry_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_option_expiries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::list_commodity_future_option_expiries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::list_commodity_future_option_expiries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_option_expiries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::list_commodity_future_option_expiries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::list_commodity_future_option_expiries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_option_expiry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_option_expiry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_option_expiries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::get_many_commodity_future_option_expiries_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::get_many_commodity_future_option_expiries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_commodity_future_option_expiries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::get_many_commodity_future_option_expiries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::get_many_commodity_future_option_expiries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_option_expiry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::put_commodity_future_option_expiry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::put_commodity_future_option_expiry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_commodity_future_option_expiry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::put_commodity_future_option_expiry_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::put_commodity_future_option_expiry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_option_expiries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::put_many_commodity_future_option_expiries_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::put_many_commodity_future_option_expiries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_commodity_future_option_expiries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::put_many_commodity_future_option_expiries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::put_many_commodity_future_option_expiries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_option_expiry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::delete_commodity_future_option_expiry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_commodity_future_option_expiry_request& v);
    static ores::refdata::messaging::delete_commodity_future_option_expiry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_commodity_future_option_expiry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::delete_commodity_future_option_expiry_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_commodity_future_option_expiry_response& v);
    static ores::refdata::messaging::delete_commodity_future_option_expiry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_option_expiries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_option_expiries_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::delete_many_commodity_future_option_expiries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_commodity_future_option_expiries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_option_expiries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::delete_many_commodity_future_option_expiries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_commodity_future_option_expiries_request& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_convention_id_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_id_commodity_future_option_expiries_response& v);
    static ores::refdata::messaging::list_by_convention_id_commodity_future_option_expiries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_option_expiry_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_option_expiry_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_option_expiry_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_option_expiry_versions_request& v);
    static ores::refdata::messaging::list_commodity_future_option_expiry_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_commodity_future_option_expiry_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_commodity_future_option_expiry_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_option_expiry_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_commodity_future_option_expiry_versions_response& v);
    static ores::refdata::messaging::list_commodity_future_option_expiry_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_option_expiry_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_commodity_future_option_expiry_version_request& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_option_expiry_version_request& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_commodity_future_option_expiry_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_commodity_future_option_expiry_version_response& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_commodity_future_option_expiry_version_response& v);
    static ores::refdata::messaging::get_commodity_future_option_expiry_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
