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
#ifndef ORES_REFDATA_API_MESSAGING_SWAPTION_VOLATILITY_CONFIG_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_SWAPTION_VOLATILITY_CONFIG_PROTOCOL_HPP

#include "ores.refdata.api/domain/swaption_volatility_config.hpp"
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

struct swaption_volatility_config_key {
    boost::uuids::uuid id;
};

struct swaption_volatility_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    std::optional<std::string> dimension;
    std::optional<std::string> volatility_type;
    std::optional<std::string> interpolation;
    std::optional<std::string> extrapolation;
    std::optional<std::string> output_volatility_type;
    std::optional<std::string> model_shift;
    std::optional<std::string> output_shift;
    std::optional<std::string> day_counter;
    std::optional<std::string> calendar;
    std::optional<std::string> business_day_convention;
    std::optional<std::string> option_tenors;
    std::optional<std::string> swap_tenors;
    std::optional<std::string> short_swap_index_base;
    std::optional<std::string> swap_index_base;
    std::optional<std::string> smile_option_tenors;
    std::optional<std::string> smile_swap_tenors;
    std::optional<std::string> smile_spreads;
    std::optional<std::string> quote_tag;
    bool has_proxy_config;
    std::optional<std::string> proxy_source_curve_id;
    std::optional<std::string> proxy_source_short_swap_index_base;
    std::optional<std::string> proxy_source_swap_index_base;
    std::optional<std::string> proxy_target_short_swap_index_base;
    std::optional<std::string> proxy_target_swap_index_base;
};

struct swaption_volatility_config_change {
    swaption_volatility_config_write write;
    ores::utility::domain::precondition precondition;
};

struct swaption_volatility_config_removal {
    swaption_volatility_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct swaption_volatility_config_lookup {
    swaption_volatility_config_key key;
    std::optional<ores::refdata::domain::swaption_volatility_config> swaption_volatility_config;
};

struct swaption_volatility_configs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct swaption_volatility_config_event {
    boost::uuids::uuid event_id;
    swaption_volatility_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct swaption_volatility_config_version_key {
    swaption_volatility_config_key swaption_volatility_config;
    std::uint32_t version;
};

struct swaption_volatility_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_swaption_volatility_configs_request {
    using response_type = struct list_swaption_volatility_configs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.swaption_volatility_configs.list";
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
    std::optional<swaption_volatility_configs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_swaption_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::swaption_volatility_config> swaption_volatility_configs;
    std::uint64_t total;
};

struct get_swaption_volatility_config_request {
    using response_type = struct get_swaption_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.swaption_volatility_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swaption_volatility_config_key key;
};

struct get_swaption_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::swaption_volatility_config> swaption_volatility_config;
};

struct get_many_swaption_volatility_configs_request {
    using response_type = struct get_many_swaption_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swaption_volatility_config_key> keys;
};

struct get_many_swaption_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<swaption_volatility_config_lookup> entries;
};

struct put_swaption_volatility_config_request {
    using response_type = struct put_swaption_volatility_config_response;
    static constexpr std::string_view nats_subject = "refdata.v1.swaption_volatility_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swaption_volatility_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_swaption_volatility_config_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::swaption_volatility_config> swaption_volatility_config;
};

struct put_many_swaption_volatility_configs_request {
    using response_type = struct put_many_swaption_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swaption_volatility_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_swaption_volatility_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::swaption_volatility_config> swaption_volatility_configs;
};

struct delete_swaption_volatility_config_request {
    using response_type = struct delete_swaption_volatility_config_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swaption_volatility_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_swaption_volatility_config_response {
    ores::utility::domain::result result;
};

struct delete_many_swaption_volatility_configs_request {
    using response_type = struct delete_many_swaption_volatility_configs_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<swaption_volatility_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_swaption_volatility_configs_response {
    ores::utility::domain::result result;
};

struct list_swaption_volatility_config_versions_request {
    using response_type = struct list_swaption_volatility_config_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swaption_volatility_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<swaption_volatility_config_versions_filter> filter;
};

struct list_swaption_volatility_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::swaption_volatility_config> versions;
    std::uint64_t total;
};

struct get_swaption_volatility_config_version_request {
    using response_type = struct get_swaption_volatility_config_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.swaption_volatility_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    swaption_volatility_config_version_key key;
};

struct get_swaption_volatility_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::swaption_volatility_config> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace swaption_volatility_config_event_subjects {
inline constexpr std::string_view created = "refdata.v1.swaption_volatility_configs_events.created";
inline constexpr std::string_view updated = "refdata.v1.swaption_volatility_configs_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.swaption_volatility_configs_events.deleted";
}
/**
 * @brief The members of swaption_volatility_config_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_key&) {
    return std::tuple{
        utility::codec::field{"id", &swaption_volatility_config_key::id},
    };
}

/**
 * @brief The members of swaption_volatility_config_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_write&) {
    return std::tuple{
        utility::codec::field{"id", &swaption_volatility_config_write::id},
        utility::codec::field{"curve_definition_id",
                              &swaption_volatility_config_write::curve_definition_id},
        utility::codec::field{"dimension", &swaption_volatility_config_write::dimension},
        utility::codec::field{"volatility_type",
                              &swaption_volatility_config_write::volatility_type},
        utility::codec::field{"interpolation", &swaption_volatility_config_write::interpolation},
        utility::codec::field{"extrapolation", &swaption_volatility_config_write::extrapolation},
        utility::codec::field{"output_volatility_type",
                              &swaption_volatility_config_write::output_volatility_type},
        utility::codec::field{"model_shift", &swaption_volatility_config_write::model_shift},
        utility::codec::field{"output_shift", &swaption_volatility_config_write::output_shift},
        utility::codec::field{"day_counter", &swaption_volatility_config_write::day_counter},
        utility::codec::field{"calendar", &swaption_volatility_config_write::calendar},
        utility::codec::field{"business_day_convention",
                              &swaption_volatility_config_write::business_day_convention},
        utility::codec::field{"option_tenors", &swaption_volatility_config_write::option_tenors},
        utility::codec::field{"swap_tenors", &swaption_volatility_config_write::swap_tenors},
        utility::codec::field{"short_swap_index_base",
                              &swaption_volatility_config_write::short_swap_index_base},
        utility::codec::field{"swap_index_base",
                              &swaption_volatility_config_write::swap_index_base},
        utility::codec::field{"smile_option_tenors",
                              &swaption_volatility_config_write::smile_option_tenors},
        utility::codec::field{"smile_swap_tenors",
                              &swaption_volatility_config_write::smile_swap_tenors},
        utility::codec::field{"smile_spreads", &swaption_volatility_config_write::smile_spreads},
        utility::codec::field{"quote_tag", &swaption_volatility_config_write::quote_tag},
        utility::codec::field{"has_proxy_config",
                              &swaption_volatility_config_write::has_proxy_config},
        utility::codec::field{"proxy_source_curve_id",
                              &swaption_volatility_config_write::proxy_source_curve_id},
        utility::codec::field{
            "proxy_source_short_swap_index_base",
            &swaption_volatility_config_write::proxy_source_short_swap_index_base},
        utility::codec::field{"proxy_source_swap_index_base",
                              &swaption_volatility_config_write::proxy_source_swap_index_base},
        utility::codec::field{
            "proxy_target_short_swap_index_base",
            &swaption_volatility_config_write::proxy_target_short_swap_index_base},
        utility::codec::field{"proxy_target_swap_index_base",
                              &swaption_volatility_config_write::proxy_target_swap_index_base},
    };
}

/**
 * @brief The members of swaption_volatility_config_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_change&) {
    return std::tuple{
        utility::codec::field{"write", &swaption_volatility_config_change::write},
        utility::codec::field{"precondition", &swaption_volatility_config_change::precondition},
    };
}

/**
 * @brief The members of swaption_volatility_config_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_removal&) {
    return std::tuple{
        utility::codec::field{"key", &swaption_volatility_config_removal::key},
        utility::codec::field{"precondition", &swaption_volatility_config_removal::precondition},
    };
}

/**
 * @brief The members of swaption_volatility_config_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &swaption_volatility_config_lookup::key},
        utility::codec::field{"swaption_volatility_config",
                              &swaption_volatility_config_lookup::swaption_volatility_config},
    };
}

/**
 * @brief The members of swaption_volatility_configs_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_configs_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &swaption_volatility_configs_filter::id_one_of},
    };
}

/**
 * @brief The members of swaption_volatility_config_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &swaption_volatility_config_event::event_id},
        utility::codec::field{"key", &swaption_volatility_config_event::key},
        utility::codec::field{"action", &swaption_volatility_config_event::action},
        utility::codec::field{"version", &swaption_volatility_config_event::version},
        utility::codec::field{"occurred_at", &swaption_volatility_config_event::occurred_at},
        utility::codec::field{"correlation_id", &swaption_volatility_config_event::correlation_id},
    };
}

/**
 * @brief The members of swaption_volatility_config_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_version_key&) {
    return std::tuple{
        utility::codec::field{"swaption_volatility_config",
                              &swaption_volatility_config_version_key::swaption_volatility_config},
        utility::codec::field{"version", &swaption_volatility_config_version_key::version},
    };
}

/**
 * @brief The members of swaption_volatility_config_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const swaption_volatility_config_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &swaption_volatility_config_versions_filter::version},
        utility::codec::field{"from_version",
                              &swaption_volatility_config_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &swaption_volatility_config_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_swaption_volatility_configs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_swaption_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_swaption_volatility_configs_request::offset},
        utility::codec::field{"limit", &list_swaption_volatility_configs_request::limit},
        utility::codec::field{"order", &list_swaption_volatility_configs_request::order},
        utility::codec::field{"filter", &list_swaption_volatility_configs_request::filter},
        utility::codec::field{"as_of", &list_swaption_volatility_configs_request::as_of},
    };
}

/**
 * @brief The members of list_swaption_volatility_configs_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_swaption_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_swaption_volatility_configs_response::result},
        utility::codec::field{
            "swaption_volatility_configs",
            &list_swaption_volatility_configs_response::swaption_volatility_configs},
        utility::codec::field{"total", &list_swaption_volatility_configs_response::total},
    };
}

/**
 * @brief The members of get_swaption_volatility_config_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_swaption_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_swaption_volatility_config_request::key},
    };
}

/**
 * @brief The members of get_swaption_volatility_config_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_swaption_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_swaption_volatility_config_response::result},
        utility::codec::field{"swaption_volatility_config",
                              &get_swaption_volatility_config_response::swaption_volatility_config},
    };
}

/**
 * @brief The members of get_many_swaption_volatility_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_swaption_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_swaption_volatility_configs_request::keys},
    };
}

/**
 * @brief The members of get_many_swaption_volatility_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_swaption_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_swaption_volatility_configs_response::result},
        utility::codec::field{"entries", &get_many_swaption_volatility_configs_response::entries},
    };
}

/**
 * @brief The members of put_swaption_volatility_config_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_swaption_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_swaption_volatility_config_request::change},
        utility::codec::field{"intent", &put_swaption_volatility_config_request::intent},
    };
}

/**
 * @brief The members of put_swaption_volatility_config_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_swaption_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_swaption_volatility_config_response::result},
        utility::codec::field{"swaption_volatility_config",
                              &put_swaption_volatility_config_response::swaption_volatility_config},
    };
}

/**
 * @brief The members of put_many_swaption_volatility_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_swaption_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_swaption_volatility_configs_request::changes},
        utility::codec::field{"intent", &put_many_swaption_volatility_configs_request::intent},
    };
}

/**
 * @brief The members of put_many_swaption_volatility_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_swaption_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_swaption_volatility_configs_response::result},
        utility::codec::field{
            "swaption_volatility_configs",
            &put_many_swaption_volatility_configs_response::swaption_volatility_configs},
    };
}

/**
 * @brief The members of delete_swaption_volatility_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_swaption_volatility_config_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_swaption_volatility_config_request::removal},
        utility::codec::field{"intent", &delete_swaption_volatility_config_request::intent},
    };
}

/**
 * @brief The members of delete_swaption_volatility_config_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_swaption_volatility_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_swaption_volatility_config_response::result},
    };
}

/**
 * @brief The members of delete_many_swaption_volatility_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_swaption_volatility_configs_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_swaption_volatility_configs_request::removals},
        utility::codec::field{"intent", &delete_many_swaption_volatility_configs_request::intent},
    };
}

/**
 * @brief The members of delete_many_swaption_volatility_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_swaption_volatility_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_swaption_volatility_configs_response::result},
    };
}

/**
 * @brief The members of list_swaption_volatility_config_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_swaption_volatility_config_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_swaption_volatility_config_versions_request::key},
        utility::codec::field{"offset", &list_swaption_volatility_config_versions_request::offset},
        utility::codec::field{"limit", &list_swaption_volatility_config_versions_request::limit},
        utility::codec::field{"order", &list_swaption_volatility_config_versions_request::order},
        utility::codec::field{"filter", &list_swaption_volatility_config_versions_request::filter},
    };
}

/**
 * @brief The members of list_swaption_volatility_config_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_swaption_volatility_config_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_swaption_volatility_config_versions_response::result},
        utility::codec::field{"versions",
                              &list_swaption_volatility_config_versions_response::versions},
        utility::codec::field{"total", &list_swaption_volatility_config_versions_response::total},
    };
}

/**
 * @brief The members of get_swaption_volatility_config_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_swaption_volatility_config_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_swaption_volatility_config_version_request::key},
    };
}

/**
 * @brief The members of get_swaption_volatility_config_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_swaption_volatility_config_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_swaption_volatility_config_version_response::result},
        utility::codec::field{"version", &get_swaption_volatility_config_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_key& v);
    static ores::refdata::messaging::swaption_volatility_config_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_key& v);
    static ores::refdata::messaging::swaption_volatility_config_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_write& v);
    static ores::refdata::messaging::swaption_volatility_config_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_write& v);
    static ores::refdata::messaging::swaption_volatility_config_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_change& v);
    static ores::refdata::messaging::swaption_volatility_config_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_change& v);
    static ores::refdata::messaging::swaption_volatility_config_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_removal& v);
    static ores::refdata::messaging::swaption_volatility_config_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_removal& v);
    static ores::refdata::messaging::swaption_volatility_config_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_lookup& v);
    static ores::refdata::messaging::swaption_volatility_config_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_lookup& v);
    static ores::refdata::messaging::swaption_volatility_config_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_configs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_configs_filter& v);
    static ores::refdata::messaging::swaption_volatility_configs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_configs_filter& v);
    static ores::refdata::messaging::swaption_volatility_configs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_event& v);
    static ores::refdata::messaging::swaption_volatility_config_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_event& v);
    static ores::refdata::messaging::swaption_volatility_config_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_version_key& v);
    static ores::refdata::messaging::swaption_volatility_config_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_version_key& v);
    static ores::refdata::messaging::swaption_volatility_config_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::swaption_volatility_config_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::swaption_volatility_config_versions_filter& v);
    static ores::refdata::messaging::swaption_volatility_config_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::swaption_volatility_config_versions_filter& v);
    static ores::refdata::messaging::swaption_volatility_config_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_swaption_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::list_swaption_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::list_swaption_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_swaption_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::list_swaption_volatility_configs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::list_swaption_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_swaption_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_swaption_volatility_config_request& v);
    static ores::refdata::messaging::get_swaption_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_swaption_volatility_config_request& v);
    static ores::refdata::messaging::get_swaption_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_swaption_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_swaption_volatility_config_response& v);
    static ores::refdata::messaging::get_swaption_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_swaption_volatility_config_response& v);
    static ores::refdata::messaging::get_swaption_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_swaption_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::get_many_swaption_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::get_many_swaption_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_swaption_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::get_many_swaption_volatility_configs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::get_many_swaption_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_swaption_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_swaption_volatility_config_request& v);
    static ores::refdata::messaging::put_swaption_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_swaption_volatility_config_request& v);
    static ores::refdata::messaging::put_swaption_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_swaption_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_swaption_volatility_config_response& v);
    static ores::refdata::messaging::put_swaption_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_swaption_volatility_config_response& v);
    static ores::refdata::messaging::put_swaption_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_swaption_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::put_many_swaption_volatility_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::put_many_swaption_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_swaption_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::put_many_swaption_volatility_configs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::put_many_swaption_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_swaption_volatility_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_swaption_volatility_config_request& v);
    static ores::refdata::messaging::delete_swaption_volatility_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_swaption_volatility_config_request& v);
    static ores::refdata::messaging::delete_swaption_volatility_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_swaption_volatility_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_swaption_volatility_config_response& v);
    static ores::refdata::messaging::delete_swaption_volatility_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_swaption_volatility_config_response& v);
    static ores::refdata::messaging::delete_swaption_volatility_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_swaption_volatility_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::delete_many_swaption_volatility_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_swaption_volatility_configs_request& v);
    static ores::refdata::messaging::delete_many_swaption_volatility_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_swaption_volatility_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::delete_many_swaption_volatility_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_swaption_volatility_configs_response& v);
    static ores::refdata::messaging::delete_many_swaption_volatility_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_swaption_volatility_config_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_swaption_volatility_config_versions_request& v);
    static ores::refdata::messaging::list_swaption_volatility_config_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_swaption_volatility_config_versions_request& v);
    static ores::refdata::messaging::list_swaption_volatility_config_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_swaption_volatility_config_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_swaption_volatility_config_versions_response& v);
    static ores::refdata::messaging::list_swaption_volatility_config_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_swaption_volatility_config_versions_response& v);
    static ores::refdata::messaging::list_swaption_volatility_config_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_swaption_volatility_config_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_swaption_volatility_config_version_request& v);
    static ores::refdata::messaging::get_swaption_volatility_config_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_swaption_volatility_config_version_request& v);
    static ores::refdata::messaging::get_swaption_volatility_config_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_swaption_volatility_config_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_swaption_volatility_config_version_response& v);
    static ores::refdata::messaging::get_swaption_volatility_config_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_swaption_volatility_config_version_response& v);
    static ores::refdata::messaging::get_swaption_volatility_config_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
