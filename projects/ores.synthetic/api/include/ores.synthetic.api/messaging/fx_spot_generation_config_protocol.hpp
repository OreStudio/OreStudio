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
#ifndef ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_GENERATION_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FX_SPOT_GENERATION_CONFIG_PROTOCOL_HPP

#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct fx_spot_generation_config_key {
    boost::uuids::uuid id;

    friend bool operator==(const fx_spot_generation_config_key&,
                           const fx_spot_generation_config_key&) = default;
};

struct fx_spot_generation_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid config_id;
    std::string base_currency_code;
    std::string quote_currency_code;
    std::string source_name;
    std::string oresmd_uri;
    std::string price_source;
    std::optional<int> calibration_window_days;
    double gmm_initial_price;
    int ticks_per_hour;
    std::string process_type;
    bool enabled;
    bool auto_start;
    std::optional<boost::uuids::uuid> folder_id;
    std::string asset_class_code;
    std::string arrival_mode;
    double branching_ratio;
    double decay;
    double sensitivity;
    std::string variance_model;
    std::optional<boost::uuids::uuid> variance_calibration_id;

    friend bool operator==(const fx_spot_generation_config_write&,
                           const fx_spot_generation_config_write&) = default;
};

struct fx_spot_generation_config_change {
    fx_spot_generation_config_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const fx_spot_generation_config_change&,
                           const fx_spot_generation_config_change&) = default;
};

struct fx_spot_generation_config_removal {
    fx_spot_generation_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const fx_spot_generation_config_removal&,
                           const fx_spot_generation_config_removal&) = default;
};

struct fx_spot_generation_config_lookup {
    fx_spot_generation_config_key key;
    std::optional<ores::synthetic::domain::fx_spot_generation_config> fx_spot_generation_config;

    friend bool operator==(const fx_spot_generation_config_lookup&,
                           const fx_spot_generation_config_lookup&) = default;
};

struct fx_spot_generation_configs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;

    friend bool operator==(const fx_spot_generation_configs_filter&,
                           const fx_spot_generation_configs_filter&) = default;
};

struct fx_spot_generation_config_event {
    boost::uuids::uuid event_id;
    fx_spot_generation_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fx_spot_generation_config_event&,
                           const fx_spot_generation_config_event&) = default;
};

struct fx_spot_generation_config_version_key {
    fx_spot_generation_config_key fx_spot_generation_config;
    std::uint32_t version;

    friend bool operator==(const fx_spot_generation_config_version_key&,
                           const fx_spot_generation_config_version_key&) = default;
};

struct fx_spot_generation_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fx_spot_generation_config_versions_filter&,
                           const fx_spot_generation_config_versions_filter&) = default;
};

struct list_fx_spot_generation_configs_request {
    using response_type = struct list_fx_spot_generation_configs_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_generation_configs.list";
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
    std::optional<fx_spot_generation_configs_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fx_spot_generation_configs_request&,
                           const list_fx_spot_generation_configs_request&) = default;
};

struct list_fx_spot_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_generation_config> fx_spot_generation_configs;
    std::uint64_t total;

    friend bool operator==(const list_fx_spot_generation_configs_response&,
                           const list_fx_spot_generation_configs_response&) = default;
};

struct get_fx_spot_generation_config_request {
    using response_type = struct get_fx_spot_generation_config_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_generation_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_generation_config_key key;

    friend bool operator==(const get_fx_spot_generation_config_request&,
                           const get_fx_spot_generation_config_request&) = default;
};

struct get_fx_spot_generation_config_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_generation_config> fx_spot_generation_config;

    friend bool operator==(const get_fx_spot_generation_config_response&,
                           const get_fx_spot_generation_config_response&) = default;
};

struct get_many_fx_spot_generation_configs_request {
    using response_type = struct get_many_fx_spot_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_generation_config_key> keys;

    friend bool operator==(const get_many_fx_spot_generation_configs_request&,
                           const get_many_fx_spot_generation_configs_request&) = default;
};

struct get_many_fx_spot_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<fx_spot_generation_config_lookup> entries;

    friend bool operator==(const get_many_fx_spot_generation_configs_response&,
                           const get_many_fx_spot_generation_configs_response&) = default;
};

struct put_fx_spot_generation_config_request {
    using response_type = struct put_fx_spot_generation_config_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.fx_spot_generation_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_generation_config_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_fx_spot_generation_config_request&,
                           const put_fx_spot_generation_config_request&) = default;
};

struct put_fx_spot_generation_config_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_generation_config> fx_spot_generation_config;

    friend bool operator==(const put_fx_spot_generation_config_response&,
                           const put_fx_spot_generation_config_response&) = default;
};

struct put_many_fx_spot_generation_configs_request {
    using response_type = struct put_many_fx_spot_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_generation_config_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_fx_spot_generation_configs_request&,
                           const put_many_fx_spot_generation_configs_request&) = default;
};

struct put_many_fx_spot_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_generation_config> fx_spot_generation_configs;

    friend bool operator==(const put_many_fx_spot_generation_configs_response&,
                           const put_many_fx_spot_generation_configs_response&) = default;
};

struct delete_fx_spot_generation_config_request {
    using response_type = struct delete_fx_spot_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_generation_config_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_fx_spot_generation_config_request&,
                           const delete_fx_spot_generation_config_request&) = default;
};

struct delete_fx_spot_generation_config_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_fx_spot_generation_config_response&,
                           const delete_fx_spot_generation_config_response&) = default;
};

struct delete_many_fx_spot_generation_configs_request {
    using response_type = struct delete_many_fx_spot_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_spot_generation_config_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_fx_spot_generation_configs_request&,
                           const delete_many_fx_spot_generation_configs_request&) = default;
};

struct delete_many_fx_spot_generation_configs_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_fx_spot_generation_configs_response&,
                           const delete_many_fx_spot_generation_configs_response&) = default;
};

struct list_fx_spot_generation_config_versions_request {
    using response_type = struct list_fx_spot_generation_config_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_generation_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_spot_generation_config_versions_filter> filter;

    friend bool operator==(const list_fx_spot_generation_config_versions_request&,
                           const list_fx_spot_generation_config_versions_request&) = default;
};

struct list_fx_spot_generation_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::fx_spot_generation_config> versions;
    std::uint64_t total;

    friend bool operator==(const list_fx_spot_generation_config_versions_response&,
                           const list_fx_spot_generation_config_versions_response&) = default;
};

struct get_fx_spot_generation_config_version_request {
    using response_type = struct get_fx_spot_generation_config_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.fx_spot_generation_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_spot_generation_config_version_key key;

    friend bool operator==(const get_fx_spot_generation_config_version_request&,
                           const get_fx_spot_generation_config_version_request&) = default;
};

struct get_fx_spot_generation_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::fx_spot_generation_config> version;

    friend bool operator==(const get_fx_spot_generation_config_version_response&,
                           const get_fx_spot_generation_config_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_spot_generation_config_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.events.fx_spot_generation_configs.created";
inline constexpr std::string_view updated =
    "synthetic.v1.events.fx_spot_generation_configs.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.events.fx_spot_generation_configs.deleted";
}
/**
 * @brief The members of fx_spot_generation_config_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_key>) {
    return std::tuple{
        utility::codec::field{"id", &fx_spot_generation_config_key::id},
    };
}

/**
 * @brief The members of fx_spot_generation_config_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_write>) {
    return std::tuple{
        utility::codec::field{"id", &fx_spot_generation_config_write::id},
        utility::codec::field{"config_id", &fx_spot_generation_config_write::config_id},
        utility::codec::field{"base_currency_code",
                              &fx_spot_generation_config_write::base_currency_code},
        utility::codec::field{"quote_currency_code",
                              &fx_spot_generation_config_write::quote_currency_code},
        utility::codec::field{"source_name", &fx_spot_generation_config_write::source_name},
        utility::codec::field{"oresmd_uri", &fx_spot_generation_config_write::oresmd_uri},
        utility::codec::field{"price_source", &fx_spot_generation_config_write::price_source},
        utility::codec::field{"calibration_window_days",
                              &fx_spot_generation_config_write::calibration_window_days},
        utility::codec::field{"gmm_initial_price",
                              &fx_spot_generation_config_write::gmm_initial_price},
        utility::codec::field{"ticks_per_hour", &fx_spot_generation_config_write::ticks_per_hour},
        utility::codec::field{"process_type", &fx_spot_generation_config_write::process_type},
        utility::codec::field{"enabled", &fx_spot_generation_config_write::enabled},
        utility::codec::field{"auto_start", &fx_spot_generation_config_write::auto_start},
        utility::codec::field{"folder_id", &fx_spot_generation_config_write::folder_id},
        utility::codec::field{"asset_class_code",
                              &fx_spot_generation_config_write::asset_class_code},
        utility::codec::field{"arrival_mode", &fx_spot_generation_config_write::arrival_mode},
        utility::codec::field{"branching_ratio", &fx_spot_generation_config_write::branching_ratio},
        utility::codec::field{"decay", &fx_spot_generation_config_write::decay},
        utility::codec::field{"sensitivity", &fx_spot_generation_config_write::sensitivity},
        utility::codec::field{"variance_model", &fx_spot_generation_config_write::variance_model},
        utility::codec::field{"variance_calibration_id",
                              &fx_spot_generation_config_write::variance_calibration_id},
    };
}

/**
 * @brief The members of fx_spot_generation_config_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_change>) {
    return std::tuple{
        utility::codec::field{"write", &fx_spot_generation_config_change::write},
        utility::codec::field{"precondition", &fx_spot_generation_config_change::precondition},
    };
}

/**
 * @brief The members of fx_spot_generation_config_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_removal>) {
    return std::tuple{
        utility::codec::field{"key", &fx_spot_generation_config_removal::key},
        utility::codec::field{"precondition", &fx_spot_generation_config_removal::precondition},
    };
}

/**
 * @brief The members of fx_spot_generation_config_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &fx_spot_generation_config_lookup::key},
        utility::codec::field{"fx_spot_generation_config",
                              &fx_spot_generation_config_lookup::fx_spot_generation_config},
    };
}

/**
 * @brief The members of fx_spot_generation_configs_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_configs_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of", &fx_spot_generation_configs_filter::id_one_of},
        utility::codec::field{"search", &fx_spot_generation_configs_filter::search},
    };
}

/**
 * @brief The members of fx_spot_generation_config_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_spot_generation_config_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &fx_spot_generation_config_event::event_id},
        utility::codec::field{"key", &fx_spot_generation_config_event::key},
        utility::codec::field{"action", &fx_spot_generation_config_event::action},
        utility::codec::field{"version", &fx_spot_generation_config_event::version},
        utility::codec::field{"occurred_at", &fx_spot_generation_config_event::occurred_at},
        utility::codec::field{"correlation_id", &fx_spot_generation_config_event::correlation_id},
    };
}

/**
 * @brief The members of fx_spot_generation_config_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<fx_spot_generation_config_version_key>) {
    return std::tuple{
        utility::codec::field{"fx_spot_generation_config",
                              &fx_spot_generation_config_version_key::fx_spot_generation_config},
        utility::codec::field{"version", &fx_spot_generation_config_version_key::version},
    };
}

/**
 * @brief The members of fx_spot_generation_config_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<fx_spot_generation_config_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &fx_spot_generation_config_versions_filter::version},
        utility::codec::field{"from_version",
                              &fx_spot_generation_config_versions_filter::from_version},
        utility::codec::field{"to_version", &fx_spot_generation_config_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fx_spot_generation_configs_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_generation_configs_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_fx_spot_generation_configs_request::offset},
        utility::codec::field{"limit", &list_fx_spot_generation_configs_request::limit},
        utility::codec::field{"order", &list_fx_spot_generation_configs_request::order},
        utility::codec::field{"filter", &list_fx_spot_generation_configs_request::filter},
        utility::codec::field{"as_of", &list_fx_spot_generation_configs_request::as_of},
    };
}

/**
 * @brief The members of list_fx_spot_generation_configs_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_generation_configs_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_fx_spot_generation_configs_response::result},
        utility::codec::field{
            "fx_spot_generation_configs",
            &list_fx_spot_generation_configs_response::fx_spot_generation_configs},
        utility::codec::field{"total", &list_fx_spot_generation_configs_response::total},
    };
}

/**
 * @brief The members of get_fx_spot_generation_config_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_generation_config_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_fx_spot_generation_config_request::key},
    };
}

/**
 * @brief The members of get_fx_spot_generation_config_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_generation_config_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_fx_spot_generation_config_response::result},
        utility::codec::field{"fx_spot_generation_config",
                              &get_fx_spot_generation_config_response::fx_spot_generation_config},
    };
}

/**
 * @brief The members of get_many_fx_spot_generation_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_spot_generation_configs_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_fx_spot_generation_configs_request::keys},
    };
}

/**
 * @brief The members of get_many_fx_spot_generation_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_spot_generation_configs_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_fx_spot_generation_configs_response::result},
        utility::codec::field{"entries", &get_many_fx_spot_generation_configs_response::entries},
    };
}

/**
 * @brief The members of put_fx_spot_generation_config_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_fx_spot_generation_config_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_fx_spot_generation_config_request::change},
        utility::codec::field{"intent", &put_fx_spot_generation_config_request::intent},
    };
}

/**
 * @brief The members of put_fx_spot_generation_config_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_fx_spot_generation_config_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_fx_spot_generation_config_response::result},
        utility::codec::field{"fx_spot_generation_config",
                              &put_fx_spot_generation_config_response::fx_spot_generation_config},
    };
}

/**
 * @brief The members of put_many_fx_spot_generation_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_fx_spot_generation_configs_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_fx_spot_generation_configs_request::changes},
        utility::codec::field{"intent", &put_many_fx_spot_generation_configs_request::intent},
    };
}

/**
 * @brief The members of put_many_fx_spot_generation_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_fx_spot_generation_configs_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_fx_spot_generation_configs_response::result},
        utility::codec::field{
            "fx_spot_generation_configs",
            &put_many_fx_spot_generation_configs_response::fx_spot_generation_configs},
    };
}

/**
 * @brief The members of delete_fx_spot_generation_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_fx_spot_generation_config_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_fx_spot_generation_config_request::removal},
        utility::codec::field{"intent", &delete_fx_spot_generation_config_request::intent},
    };
}

/**
 * @brief The members of delete_fx_spot_generation_config_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_fx_spot_generation_config_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_fx_spot_generation_config_response::result},
    };
}

/**
 * @brief The members of delete_many_fx_spot_generation_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_fx_spot_generation_configs_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_fx_spot_generation_configs_request::removals},
        utility::codec::field{"intent", &delete_many_fx_spot_generation_configs_request::intent},
    };
}

/**
 * @brief The members of delete_many_fx_spot_generation_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_fx_spot_generation_configs_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_fx_spot_generation_configs_response::result},
    };
}

/**
 * @brief The members of list_fx_spot_generation_config_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_generation_config_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_fx_spot_generation_config_versions_request::key},
        utility::codec::field{"offset", &list_fx_spot_generation_config_versions_request::offset},
        utility::codec::field{"limit", &list_fx_spot_generation_config_versions_request::limit},
        utility::codec::field{"order", &list_fx_spot_generation_config_versions_request::order},
        utility::codec::field{"filter", &list_fx_spot_generation_config_versions_request::filter},
    };
}

/**
 * @brief The members of list_fx_spot_generation_config_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_spot_generation_config_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_fx_spot_generation_config_versions_response::result},
        utility::codec::field{"versions",
                              &list_fx_spot_generation_config_versions_response::versions},
        utility::codec::field{"total", &list_fx_spot_generation_config_versions_response::total},
    };
}

/**
 * @brief The members of get_fx_spot_generation_config_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_generation_config_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_fx_spot_generation_config_version_request::key},
    };
}

/**
 * @brief The members of get_fx_spot_generation_config_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_spot_generation_config_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_fx_spot_generation_config_version_response::result},
        utility::codec::field{"version", &get_fx_spot_generation_config_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_key& v);
    static ores::synthetic::messaging::fx_spot_generation_config_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_key& v);
    static ores::synthetic::messaging::fx_spot_generation_config_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_write& v);
    static ores::synthetic::messaging::fx_spot_generation_config_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_write& v);
    static ores::synthetic::messaging::fx_spot_generation_config_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_change& v);
    static ores::synthetic::messaging::fx_spot_generation_config_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_change& v);
    static ores::synthetic::messaging::fx_spot_generation_config_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_removal& v);
    static ores::synthetic::messaging::fx_spot_generation_config_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_removal& v);
    static ores::synthetic::messaging::fx_spot_generation_config_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_lookup& v);
    static ores::synthetic::messaging::fx_spot_generation_config_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_lookup& v);
    static ores::synthetic::messaging::fx_spot_generation_config_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_configs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_configs_filter& v);
    static ores::synthetic::messaging::fx_spot_generation_configs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_configs_filter& v);
    static ores::synthetic::messaging::fx_spot_generation_configs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_event& v);
    static ores::synthetic::messaging::fx_spot_generation_config_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_event& v);
    static ores::synthetic::messaging::fx_spot_generation_config_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_version_key& v);
    static ores::synthetic::messaging::fx_spot_generation_config_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_version_key& v);
    static ores::synthetic::messaging::fx_spot_generation_config_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::fx_spot_generation_config_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::fx_spot_generation_config_versions_filter& v);
    static ores::synthetic::messaging::fx_spot_generation_config_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::fx_spot_generation_config_versions_filter& v);
    static ores::synthetic::messaging::fx_spot_generation_config_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::list_fx_spot_generation_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::list_fx_spot_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::list_fx_spot_generation_configs_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::list_fx_spot_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_spot_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::get_many_fx_spot_generation_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::get_many_fx_spot_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_fx_spot_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::get_many_fx_spot_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::get_many_fx_spot_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_fx_spot_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::put_fx_spot_generation_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::put_fx_spot_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_fx_spot_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::put_fx_spot_generation_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::put_fx_spot_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_spot_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::put_many_fx_spot_generation_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::put_many_fx_spot_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_fx_spot_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::put_many_fx_spot_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::put_many_fx_spot_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_spot_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::delete_fx_spot_generation_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_spot_generation_config_request& v);
    static ores::synthetic::messaging::delete_fx_spot_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_fx_spot_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::delete_fx_spot_generation_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_fx_spot_generation_config_response& v);
    static ores::synthetic::messaging::delete_fx_spot_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_spot_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::delete_many_fx_spot_generation_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_fx_spot_generation_configs_request& v);
    static ores::synthetic::messaging::delete_many_fx_spot_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_fx_spot_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::delete_many_fx_spot_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_fx_spot_generation_configs_response& v);
    static ores::synthetic::messaging::delete_many_fx_spot_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_generation_config_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_fx_spot_generation_config_versions_request& v);
    static ores::synthetic::messaging::list_fx_spot_generation_config_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_fx_spot_generation_config_versions_request& v);
    static ores::synthetic::messaging::list_fx_spot_generation_config_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_fx_spot_generation_config_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_fx_spot_generation_config_versions_response& v);
    static ores::synthetic::messaging::list_fx_spot_generation_config_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_fx_spot_generation_config_versions_response& v);
    static ores::synthetic::messaging::list_fx_spot_generation_config_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_generation_config_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_generation_config_version_request& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_fx_spot_generation_config_version_request& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_fx_spot_generation_config_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_fx_spot_generation_config_version_response& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_fx_spot_generation_config_version_response& v);
    static ores::synthetic::messaging::get_fx_spot_generation_config_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
