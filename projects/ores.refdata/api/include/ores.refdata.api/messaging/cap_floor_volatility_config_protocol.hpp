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
#ifndef ORES_REFDATA_API_MESSAGING_CAP_FLOOR_VOLATILITY_CONFIG_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CAP_FLOOR_VOLATILITY_CONFIG_PROTOCOL_HPP

#include "ores.refdata.api/domain/cap_floor_volatility_config.hpp"
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

struct cap_floor_volatility_config_key {
    boost::uuids::uuid id;
};

struct cap_floor_volatility_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::optional<std::string> volatility_type;
    std::optional<std::string> output_volatility_type;
    std::optional<double> model_shift;
    std::optional<double> output_shift;
    std::optional<std::string> extrapolation;
    std::optional<std::string> interpolation_method;
    std::optional<std::string> include_atm;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> business_day_convention;
    std::optional<std::string> tenors;
    std::optional<std::string> strikes;
    std::optional<std::string> optional_quotes;
    std::optional<std::string> ibor_index;
    std::optional<std::string> index;
    std::optional<std::string> rate_computation_period;
    std::optional<int> on_cap_settlement_days;
    std::optional<std::string> discount_curve;
    std::optional<std::string> atm_tenors;
    std::optional<int> settlement_days;
    std::optional<std::string> interpolate_on;
    std::optional<std::string> time_interpolation;
    std::optional<std::string> strike_interpolation;
    std::optional<std::string> input_type;
    std::optional<std::string> quote_includes_index_name;
    std::optional<std::string> flat_first_period;
    std::optional<std::string> use_effecive_volatility;
    std::optional<std::string> use_effective_volatility;
};

struct cap_floor_volatility_config_change {
    cap_floor_volatility_config_write write;
    ores::utility::domain::precondition precondition;
};

struct cap_floor_volatility_config_removal {
    cap_floor_volatility_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct cap_floor_volatility_config_lookup {
    cap_floor_volatility_config_key key;
    std::optional<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatility_config;
};

struct cap_floor_volatility_configs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct cap_floor_volatility_config_event {
    boost::uuids::uuid event_id;
    cap_floor_volatility_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct cap_floor_volatility_config_version_key {
    cap_floor_volatility_config_key cap_floor_volatility_config;
    std::uint32_t version;
};

struct cap_floor_volatility_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_cap_floor_volatility_configs_request {
    using response_type = struct list_cap_floor_volatility_configs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cap_floor_volatility_configs.list";
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
    std::optional<cap_floor_volatility_configs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_cap_floor_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatility_configs;
    std::uint64_t total;
};

struct get_cap_floor_volatility_config_request {
    using response_type = struct get_cap_floor_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cap_floor_volatility_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cap_floor_volatility_config_key key;
};

struct get_cap_floor_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatility_config;
};

struct get_many_cap_floor_volatility_configs_request {
    using response_type = struct get_many_cap_floor_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cap_floor_volatility_config_key> keys;
};

struct get_many_cap_floor_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<cap_floor_volatility_config_lookup> entries;
};

struct put_cap_floor_volatility_config_request {
    using response_type = struct put_cap_floor_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.cap_floor_volatility_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cap_floor_volatility_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_cap_floor_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatility_config;
};

struct put_many_cap_floor_volatility_configs_request {
    using response_type = struct put_many_cap_floor_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cap_floor_volatility_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_cap_floor_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cap_floor_volatility_config> cap_floor_volatility_configs;
};

struct delete_cap_floor_volatility_config_request {
    using response_type = struct delete_cap_floor_volatility_config_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cap_floor_volatility_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_cap_floor_volatility_config_response {
    ores::utility::domain::result result;
};

struct delete_many_cap_floor_volatility_configs_request {
    using response_type = struct delete_many_cap_floor_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cap_floor_volatility_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_cap_floor_volatility_configs_response {
    ores::utility::domain::result result;
};

struct list_cap_floor_volatility_config_versions_request {
    using response_type = struct list_cap_floor_volatility_config_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cap_floor_volatility_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cap_floor_volatility_config_versions_filter> filter;
};

struct list_cap_floor_volatility_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::cap_floor_volatility_config> versions;
    std::uint64_t total;
};

struct get_cap_floor_volatility_config_version_request {
    using response_type = struct get_cap_floor_volatility_config_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.cap_floor_volatility_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cap_floor_volatility_config_version_key key;
};

struct get_cap_floor_volatility_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::cap_floor_volatility_config> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cap_floor_volatility_config_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.cap_floor_volatility_configs_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.cap_floor_volatility_configs_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.cap_floor_volatility_configs_events.deleted";
}
/**
 * @brief The members of cap_floor_volatility_config_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_key&) {
    return std::tuple{
        utility::codec::field{"id", &cap_floor_volatility_config_key::id},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_write&) {
    return std::tuple{
        utility::codec::field{"id", &cap_floor_volatility_config_write::id},
        utility::codec::field{"curve_definition_id",
                              &cap_floor_volatility_config_write::curve_definition_id},
        utility::codec::field{"volatility_type",
                              &cap_floor_volatility_config_write::volatility_type},
        utility::codec::field{"output_volatility_type",
                              &cap_floor_volatility_config_write::output_volatility_type},
        utility::codec::field{"model_shift", &cap_floor_volatility_config_write::model_shift},
        utility::codec::field{"output_shift", &cap_floor_volatility_config_write::output_shift},
        utility::codec::field{"extrapolation", &cap_floor_volatility_config_write::extrapolation},
        utility::codec::field{"interpolation_method",
                              &cap_floor_volatility_config_write::interpolation_method},
        utility::codec::field{"include_atm", &cap_floor_volatility_config_write::include_atm},
        utility::codec::field{"day_counter", &cap_floor_volatility_config_write::day_counter},
        utility::codec::field{"calendar", &cap_floor_volatility_config_write::calendar},
        utility::codec::field{"business_day_convention",
                              &cap_floor_volatility_config_write::business_day_convention},
        utility::codec::field{"tenors", &cap_floor_volatility_config_write::tenors},
        utility::codec::field{"strikes", &cap_floor_volatility_config_write::strikes},
        utility::codec::field{"optional_quotes",
                              &cap_floor_volatility_config_write::optional_quotes},
        utility::codec::field{"ibor_index", &cap_floor_volatility_config_write::ibor_index},
        utility::codec::field{"index", &cap_floor_volatility_config_write::index},
        utility::codec::field{"rate_computation_period",
                              &cap_floor_volatility_config_write::rate_computation_period},
        utility::codec::field{"on_cap_settlement_days",
                              &cap_floor_volatility_config_write::on_cap_settlement_days},
        utility::codec::field{"discount_curve", &cap_floor_volatility_config_write::discount_curve},
        utility::codec::field{"atm_tenors", &cap_floor_volatility_config_write::atm_tenors},
        utility::codec::field{"settlement_days",
                              &cap_floor_volatility_config_write::settlement_days},
        utility::codec::field{"interpolate_on", &cap_floor_volatility_config_write::interpolate_on},
        utility::codec::field{"time_interpolation",
                              &cap_floor_volatility_config_write::time_interpolation},
        utility::codec::field{"strike_interpolation",
                              &cap_floor_volatility_config_write::strike_interpolation},
        utility::codec::field{"input_type", &cap_floor_volatility_config_write::input_type},
        utility::codec::field{"quote_includes_index_name",
                              &cap_floor_volatility_config_write::quote_includes_index_name},
        utility::codec::field{"flat_first_period",
                              &cap_floor_volatility_config_write::flat_first_period},
        utility::codec::field{"use_effecive_volatility",
                              &cap_floor_volatility_config_write::use_effecive_volatility},
        utility::codec::field{"use_effective_volatility",
                              &cap_floor_volatility_config_write::use_effective_volatility},
        utility::codec::field{"has_proxy_config",
                              &cap_floor_volatility_config_write::has_proxy_config},
        utility::codec::field{"proxy_source_curve_id",
                              &cap_floor_volatility_config_write::proxy_source_curve_id},
        utility::codec::field{"proxy_source_index",
                              &cap_floor_volatility_config_write::proxy_source_index},
        utility::codec::field{
            "proxy_source_rate_computation_period",
            &cap_floor_volatility_config_write::proxy_source_rate_computation_period},
        utility::codec::field{"proxy_target_index",
                              &cap_floor_volatility_config_write::proxy_target_index},
        utility::codec::field{
            "proxy_target_rate_computation_period",
            &cap_floor_volatility_config_write::proxy_target_rate_computation_period},
        utility::codec::field{
            "proxy_target_on_cap_settlement_days",
            &cap_floor_volatility_config_write::proxy_target_on_cap_settlement_days},
        utility::codec::field{"proxy_scaling_factor",
                              &cap_floor_volatility_config_write::proxy_scaling_factor},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_change&) {
    return std::tuple{
        utility::codec::field{"write", &cap_floor_volatility_config_change::write},
        utility::codec::field{"precondition", &cap_floor_volatility_config_change::precondition},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_removal&) {
    return std::tuple{
        utility::codec::field{"key", &cap_floor_volatility_config_removal::key},
        utility::codec::field{"precondition", &cap_floor_volatility_config_removal::precondition},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &cap_floor_volatility_config_lookup::key},
        utility::codec::field{"cap_floor_volatility_config",
                              &cap_floor_volatility_config_lookup::cap_floor_volatility_config},
    };
}

/**
 * @brief The members of cap_floor_volatility_configs_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_configs_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &cap_floor_volatility_configs_filter::id_one_of},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &cap_floor_volatility_config_event::event_id},
        utility::codec::field{"key", &cap_floor_volatility_config_event::key},
        utility::codec::field{"action", &cap_floor_volatility_config_event::action},
        utility::codec::field{"version", &cap_floor_volatility_config_event::version},
        utility::codec::field{"occurred_at", &cap_floor_volatility_config_event::occurred_at},
        utility::codec::field{"correlation_id", &cap_floor_volatility_config_event::correlation_id},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_version_key&) {
    return std::tuple{
        utility::codec::field{
            "cap_floor_volatility_config",
            &cap_floor_volatility_config_version_key::cap_floor_volatility_config},
        utility::codec::field{"version", &cap_floor_volatility_config_version_key::version},
    };
}

/**
 * @brief The members of cap_floor_volatility_config_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cap_floor_volatility_config_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &cap_floor_volatility_config_versions_filter::version},
        utility::codec::field{"from_version",
                              &cap_floor_volatility_config_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &cap_floor_volatility_config_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cap_floor_volatility_configs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cap_floor_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_cap_floor_volatility_configs_request::offset},
        utility::codec::field{"limit", &list_cap_floor_volatility_configs_request::limit},
        utility::codec::field{"order", &list_cap_floor_volatility_configs_request::order},
        utility::codec::field{"filter", &list_cap_floor_volatility_configs_request::filter},
        utility::codec::field{"as_of", &list_cap_floor_volatility_configs_request::as_of},
    };
}

/**
 * @brief The members of list_cap_floor_volatility_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cap_floor_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_cap_floor_volatility_configs_response::result},
        utility::codec::field{
            "cap_floor_volatility_configs",
            &list_cap_floor_volatility_configs_response::cap_floor_volatility_configs},
        utility::codec::field{"total", &list_cap_floor_volatility_configs_response::total},
    };
}

/**
 * @brief The members of get_cap_floor_volatility_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cap_floor_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cap_floor_volatility_config_request::key},
    };
}

/**
 * @brief The members of get_cap_floor_volatility_config_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cap_floor_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cap_floor_volatility_config_response::result},
        utility::codec::field{
            "cap_floor_volatility_config",
            &get_cap_floor_volatility_config_response::cap_floor_volatility_config},
    };
}

/**
 * @brief The members of get_many_cap_floor_volatility_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cap_floor_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_cap_floor_volatility_configs_request::keys},
    };
}

/**
 * @brief The members of get_many_cap_floor_volatility_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cap_floor_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_cap_floor_volatility_configs_response::result},
        utility::codec::field{"entries", &get_many_cap_floor_volatility_configs_response::entries},
    };
}

/**
 * @brief The members of put_cap_floor_volatility_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cap_floor_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_cap_floor_volatility_config_request::change},
        utility::codec::field{"intent", &put_cap_floor_volatility_config_request::intent},
    };
}

/**
 * @brief The members of put_cap_floor_volatility_config_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cap_floor_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_cap_floor_volatility_config_response::result},
        utility::codec::field{
            "cap_floor_volatility_config",
            &put_cap_floor_volatility_config_response::cap_floor_volatility_config},
    };
}

/**
 * @brief The members of put_many_cap_floor_volatility_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cap_floor_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_cap_floor_volatility_configs_request::changes},
        utility::codec::field{"intent", &put_many_cap_floor_volatility_configs_request::intent},
    };
}

/**
 * @brief The members of put_many_cap_floor_volatility_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cap_floor_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_cap_floor_volatility_configs_response::result},
        utility::codec::field{
            "cap_floor_volatility_configs",
            &put_many_cap_floor_volatility_configs_response::cap_floor_volatility_configs},
    };
}

/**
 * @brief The members of delete_cap_floor_volatility_config_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cap_floor_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_cap_floor_volatility_config_request::removal},
        utility::codec::field{"intent", &delete_cap_floor_volatility_config_request::intent},
    };
}

/**
 * @brief The members of delete_cap_floor_volatility_config_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cap_floor_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_cap_floor_volatility_config_response::result},
    };
}

/**
 * @brief The members of delete_many_cap_floor_volatility_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cap_floor_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_cap_floor_volatility_configs_request::removals},
        utility::codec::field{"intent", &delete_many_cap_floor_volatility_configs_request::intent},
    };
}

/**
 * @brief The members of delete_many_cap_floor_volatility_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cap_floor_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_cap_floor_volatility_configs_response::result},
    };
}

/**
 * @brief The members of list_cap_floor_volatility_config_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cap_floor_volatility_config_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_cap_floor_volatility_config_versions_request::key},
        utility::codec::field{"offset", &list_cap_floor_volatility_config_versions_request::offset},
        utility::codec::field{"limit", &list_cap_floor_volatility_config_versions_request::limit},
        utility::codec::field{"order", &list_cap_floor_volatility_config_versions_request::order},
        utility::codec::field{"filter", &list_cap_floor_volatility_config_versions_request::filter},
    };
}

/**
 * @brief The members of list_cap_floor_volatility_config_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_cap_floor_volatility_config_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_cap_floor_volatility_config_versions_response::result},
        utility::codec::field{"versions",
                              &list_cap_floor_volatility_config_versions_response::versions},
        utility::codec::field{"total", &list_cap_floor_volatility_config_versions_response::total},
    };
}

/**
 * @brief The members of get_cap_floor_volatility_config_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cap_floor_volatility_config_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cap_floor_volatility_config_version_request::key},
    };
}

/**
 * @brief The members of get_cap_floor_volatility_config_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cap_floor_volatility_config_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cap_floor_volatility_config_version_response::result},
        utility::codec::field{"version",
                              &get_cap_floor_volatility_config_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_key& v);
    static ores::refdata::messaging::cap_floor_volatility_config_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_key& v);
    static ores::refdata::messaging::cap_floor_volatility_config_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_write& v);
    static ores::refdata::messaging::cap_floor_volatility_config_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_write& v);
    static ores::refdata::messaging::cap_floor_volatility_config_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_change& v);
    static ores::refdata::messaging::cap_floor_volatility_config_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_change& v);
    static ores::refdata::messaging::cap_floor_volatility_config_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_removal& v);
    static ores::refdata::messaging::cap_floor_volatility_config_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_removal& v);
    static ores::refdata::messaging::cap_floor_volatility_config_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_lookup& v);
    static ores::refdata::messaging::cap_floor_volatility_config_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_lookup& v);
    static ores::refdata::messaging::cap_floor_volatility_config_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_configs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_configs_filter& v);
    static ores::refdata::messaging::cap_floor_volatility_configs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_configs_filter& v);
    static ores::refdata::messaging::cap_floor_volatility_configs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_event& v);
    static ores::refdata::messaging::cap_floor_volatility_config_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_event& v);
    static ores::refdata::messaging::cap_floor_volatility_config_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_version_key& v);
    static ores::refdata::messaging::cap_floor_volatility_config_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_version_key& v);
    static ores::refdata::messaging::cap_floor_volatility_config_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::cap_floor_volatility_config_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::cap_floor_volatility_config_versions_filter& v);
    static ores::refdata::messaging::cap_floor_volatility_config_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::cap_floor_volatility_config_versions_filter& v);
    static ores::refdata::messaging::cap_floor_volatility_config_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cap_floor_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::list_cap_floor_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::list_cap_floor_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cap_floor_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::list_cap_floor_volatility_configs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::list_cap_floor_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cap_floor_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cap_floor_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cap_floor_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::get_many_cap_floor_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::get_many_cap_floor_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_cap_floor_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::get_many_cap_floor_volatility_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::get_many_cap_floor_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cap_floor_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::put_cap_floor_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::put_cap_floor_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_cap_floor_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::put_cap_floor_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::put_cap_floor_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cap_floor_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::put_many_cap_floor_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::put_many_cap_floor_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_cap_floor_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::put_many_cap_floor_volatility_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::put_many_cap_floor_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cap_floor_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::delete_cap_floor_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cap_floor_volatility_config_request& v);
    static ores::refdata::messaging::delete_cap_floor_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_cap_floor_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::delete_cap_floor_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_cap_floor_volatility_config_response& v);
    static ores::refdata::messaging::delete_cap_floor_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cap_floor_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::delete_many_cap_floor_volatility_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_cap_floor_volatility_configs_request& v);
    static ores::refdata::messaging::delete_many_cap_floor_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_cap_floor_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::delete_many_cap_floor_volatility_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_cap_floor_volatility_configs_response& v);
    static ores::refdata::messaging::delete_many_cap_floor_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cap_floor_volatility_config_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_cap_floor_volatility_config_versions_request& v);
    static ores::refdata::messaging::list_cap_floor_volatility_config_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_cap_floor_volatility_config_versions_request& v);
    static ores::refdata::messaging::list_cap_floor_volatility_config_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_cap_floor_volatility_config_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_cap_floor_volatility_config_versions_response& v);
    static ores::refdata::messaging::list_cap_floor_volatility_config_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_cap_floor_volatility_config_versions_response& v);
    static ores::refdata::messaging::list_cap_floor_volatility_config_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cap_floor_volatility_config_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cap_floor_volatility_config_version_request& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_cap_floor_volatility_config_version_request& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_cap_floor_volatility_config_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_cap_floor_volatility_config_version_response& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_cap_floor_volatility_config_version_response& v);
    static ores::refdata::messaging::get_cap_floor_volatility_config_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
