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
#ifndef ORES_SYNTHETIC_API_MESSAGING_MARKET_DATA_GENERATION_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_MARKET_DATA_GENERATION_CONFIG_PROTOCOL_HPP

#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
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

struct market_data_generation_config_key {
    boost::uuids::uuid id;
};

struct market_data_generation_config_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    bool enabled;
    std::optional<boost::uuids::uuid> dataset_id;
    std::string provider_code;
};

struct market_data_generation_config_change {
    market_data_generation_config_write write;
    ores::utility::domain::precondition precondition;
};

struct market_data_generation_config_removal {
    market_data_generation_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct market_data_generation_config_lookup {
    market_data_generation_config_key key;
    std::optional<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_config;
};

struct market_data_generation_configs_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct market_data_generation_config_event {
    boost::uuids::uuid event_id;
    market_data_generation_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct market_data_generation_config_version_key {
    market_data_generation_config_key market_data_generation_config;
    std::uint32_t version;
};

struct market_data_generation_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_market_data_generation_configs_request {
    using response_type = struct list_market_data_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.list";
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
    std::optional<market_data_generation_configs_filter> filter;
    std::optional<std::string> as_of;
};

struct list_market_data_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_configs;
    std::uint64_t total;
};

struct get_market_data_generation_config_request {
    using response_type = struct get_market_data_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_generation_config_key key;
};

struct get_market_data_generation_config_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_config;
};

struct get_many_market_data_generation_configs_request {
    using response_type = struct get_many_market_data_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_generation_config_key> keys;
};

struct get_many_market_data_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<market_data_generation_config_lookup> entries;
};

struct put_market_data_generation_config_request {
    using response_type = struct put_market_data_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_generation_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_market_data_generation_config_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_config;
};

struct put_many_market_data_generation_configs_request {
    using response_type = struct put_many_market_data_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_generation_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_market_data_generation_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::market_data_generation_config>
        market_data_generation_configs;
};

struct delete_market_data_generation_config_request {
    using response_type = struct delete_market_data_generation_config_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_generation_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_market_data_generation_config_response {
    ores::utility::domain::result result;
};

struct delete_many_market_data_generation_configs_request {
    using response_type = struct delete_many_market_data_generation_configs_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_generation_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_market_data_generation_configs_response {
    ores::utility::domain::result result;
};

struct list_market_data_generation_config_versions_request {
    using response_type = struct list_market_data_generation_config_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_generation_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_data_generation_config_versions_filter> filter;
};

struct list_market_data_generation_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::market_data_generation_config> versions;
    std::uint64_t total;
};

struct get_market_data_generation_config_version_request {
    using response_type = struct get_market_data_generation_config_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.market_data_generation_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_generation_config_version_key key;
};

struct get_market_data_generation_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::market_data_generation_config> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_data_generation_config_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.market_data_generation_configs_events.created";
inline constexpr std::string_view updated =
    "synthetic.v1.market_data_generation_configs_events.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.market_data_generation_configs_events.deleted";
}
/**
 * @brief The members of market_data_generation_config_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_key&) {
    return std::tuple{
        utility::codec::field{"id", &market_data_generation_config_key::id},
    };
}

/**
 * @brief The members of market_data_generation_config_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_write&) {
    return std::tuple{
        utility::codec::field{"id", &market_data_generation_config_write::id},
        utility::codec::field{"name", &market_data_generation_config_write::name},
        utility::codec::field{"description", &market_data_generation_config_write::description},
        utility::codec::field{"enabled", &market_data_generation_config_write::enabled},
        utility::codec::field{"dataset_id", &market_data_generation_config_write::dataset_id},
        utility::codec::field{"provider_code", &market_data_generation_config_write::provider_code},
    };
}

/**
 * @brief The members of market_data_generation_config_change, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_change&) {
    return std::tuple{
        utility::codec::field{"write", &market_data_generation_config_change::write},
        utility::codec::field{"precondition", &market_data_generation_config_change::precondition},
    };
}

/**
 * @brief The members of market_data_generation_config_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_removal&) {
    return std::tuple{
        utility::codec::field{"key", &market_data_generation_config_removal::key},
        utility::codec::field{"precondition", &market_data_generation_config_removal::precondition},
    };
}

/**
 * @brief The members of market_data_generation_config_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &market_data_generation_config_lookup::key},
        utility::codec::field{"market_data_generation_config",
                              &market_data_generation_config_lookup::market_data_generation_config},
    };
}

/**
 * @brief The members of market_data_generation_configs_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_configs_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &market_data_generation_configs_filter::id_one_of},
    };
}

/**
 * @brief The members of market_data_generation_config_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &market_data_generation_config_event::event_id},
        utility::codec::field{"key", &market_data_generation_config_event::key},
        utility::codec::field{"action", &market_data_generation_config_event::action},
        utility::codec::field{"version", &market_data_generation_config_event::version},
        utility::codec::field{"occurred_at", &market_data_generation_config_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &market_data_generation_config_event::correlation_id},
    };
}

/**
 * @brief The members of market_data_generation_config_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_version_key&) {
    return std::tuple{
        utility::codec::field{
            "market_data_generation_config",
            &market_data_generation_config_version_key::market_data_generation_config},
        utility::codec::field{"version", &market_data_generation_config_version_key::version},
    };
}

/**
 * @brief The members of market_data_generation_config_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const market_data_generation_config_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &market_data_generation_config_versions_filter::version},
        utility::codec::field{"from_version",
                              &market_data_generation_config_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &market_data_generation_config_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_market_data_generation_configs_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_data_generation_configs_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_data_generation_configs_request::offset},
        utility::codec::field{"limit", &list_market_data_generation_configs_request::limit},
        utility::codec::field{"order", &list_market_data_generation_configs_request::order},
        utility::codec::field{"filter", &list_market_data_generation_configs_request::filter},
        utility::codec::field{"as_of", &list_market_data_generation_configs_request::as_of},
    };
}

/**
 * @brief The members of list_market_data_generation_configs_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_market_data_generation_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_market_data_generation_configs_response::result},
        utility::codec::field{
            "market_data_generation_configs",
            &list_market_data_generation_configs_response::market_data_generation_configs},
        utility::codec::field{"total", &list_market_data_generation_configs_response::total},
    };
}

/**
 * @brief The members of get_market_data_generation_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_data_generation_config_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_generation_config_request::key},
    };
}

/**
 * @brief The members of get_market_data_generation_config_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_data_generation_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_market_data_generation_config_response::result},
        utility::codec::field{
            "market_data_generation_config",
            &get_market_data_generation_config_response::market_data_generation_config},
    };
}

/**
 * @brief The members of get_many_market_data_generation_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_data_generation_configs_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_data_generation_configs_request::keys},
    };
}

/**
 * @brief The members of get_many_market_data_generation_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_market_data_generation_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_data_generation_configs_response::result},
        utility::codec::field{"entries",
                              &get_many_market_data_generation_configs_response::entries},
    };
}

/**
 * @brief The members of put_market_data_generation_config_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_data_generation_config_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_market_data_generation_config_request::change},
        utility::codec::field{"intent", &put_market_data_generation_config_request::intent},
    };
}

/**
 * @brief The members of put_market_data_generation_config_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_market_data_generation_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_market_data_generation_config_response::result},
        utility::codec::field{
            "market_data_generation_config",
            &put_market_data_generation_config_response::market_data_generation_config},
    };
}

/**
 * @brief The members of put_many_market_data_generation_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_data_generation_configs_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_data_generation_configs_request::changes},
        utility::codec::field{"intent", &put_many_market_data_generation_configs_request::intent},
    };
}

/**
 * @brief The members of put_many_market_data_generation_configs_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_market_data_generation_configs_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_data_generation_configs_response::result},
        utility::codec::field{
            "market_data_generation_configs",
            &put_many_market_data_generation_configs_response::market_data_generation_configs},
    };
}

/**
 * @brief The members of delete_market_data_generation_config_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_data_generation_config_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_data_generation_config_request::removal},
        utility::codec::field{"intent", &delete_market_data_generation_config_request::intent},
    };
}

/**
 * @brief The members of delete_market_data_generation_config_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_market_data_generation_config_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_data_generation_config_response::result},
    };
}

/**
 * @brief The members of delete_many_market_data_generation_configs_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_market_data_generation_configs_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_market_data_generation_configs_request::removals},
        utility::codec::field{"intent",
                              &delete_many_market_data_generation_configs_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_data_generation_configs_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_market_data_generation_configs_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_market_data_generation_configs_response::result},
    };
}

/**
 * @brief The members of list_market_data_generation_config_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_market_data_generation_config_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_market_data_generation_config_versions_request::key},
        utility::codec::field{"offset",
                              &list_market_data_generation_config_versions_request::offset},
        utility::codec::field{"limit", &list_market_data_generation_config_versions_request::limit},
        utility::codec::field{"order", &list_market_data_generation_config_versions_request::order},
        utility::codec::field{"filter",
                              &list_market_data_generation_config_versions_request::filter},
    };
}

/**
 * @brief The members of list_market_data_generation_config_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_market_data_generation_config_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_market_data_generation_config_versions_response::result},
        utility::codec::field{"versions",
                              &list_market_data_generation_config_versions_response::versions},
        utility::codec::field{"total",
                              &list_market_data_generation_config_versions_response::total},
    };
}

/**
 * @brief The members of get_market_data_generation_config_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_market_data_generation_config_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_generation_config_version_request::key},
    };
}

/**
 * @brief The members of get_market_data_generation_config_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_market_data_generation_config_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_market_data_generation_config_version_response::result},
        utility::codec::field{"version",
                              &get_market_data_generation_config_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_key& v);
    static ores::synthetic::messaging::market_data_generation_config_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_key& v);
    static ores::synthetic::messaging::market_data_generation_config_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_write& v);
    static ores::synthetic::messaging::market_data_generation_config_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_write& v);
    static ores::synthetic::messaging::market_data_generation_config_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_change& v);
    static ores::synthetic::messaging::market_data_generation_config_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_change& v);
    static ores::synthetic::messaging::market_data_generation_config_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_removal& v);
    static ores::synthetic::messaging::market_data_generation_config_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_removal& v);
    static ores::synthetic::messaging::market_data_generation_config_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_lookup& v);
    static ores::synthetic::messaging::market_data_generation_config_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_lookup& v);
    static ores::synthetic::messaging::market_data_generation_config_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_configs_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_configs_filter& v);
    static ores::synthetic::messaging::market_data_generation_configs_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_configs_filter& v);
    static ores::synthetic::messaging::market_data_generation_configs_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_event& v);
    static ores::synthetic::messaging::market_data_generation_config_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_event& v);
    static ores::synthetic::messaging::market_data_generation_config_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_version_key& v);
    static ores::synthetic::messaging::market_data_generation_config_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::market_data_generation_config_version_key& v);
    static ores::synthetic::messaging::market_data_generation_config_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::market_data_generation_config_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::market_data_generation_config_versions_filter& v);
    static ores::synthetic::messaging::market_data_generation_config_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::market_data_generation_config_versions_filter& v);
    static ores::synthetic::messaging::market_data_generation_config_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_market_data_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::list_market_data_generation_configs_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::list_market_data_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_market_data_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::list_market_data_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::list_market_data_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_data_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_market_data_generation_config_request& v);
    static ores::synthetic::messaging::get_market_data_generation_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_data_generation_config_request& v);
    static ores::synthetic::messaging::get_market_data_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_data_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_market_data_generation_config_response& v);
    static ores::synthetic::messaging::get_market_data_generation_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_market_data_generation_config_response& v);
    static ores::synthetic::messaging::get_market_data_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_market_data_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::get_many_market_data_generation_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::get_many_market_data_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_market_data_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::get_many_market_data_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::get_many_market_data_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_market_data_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_market_data_generation_config_request& v);
    static ores::synthetic::messaging::put_market_data_generation_config_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_market_data_generation_config_request& v);
    static ores::synthetic::messaging::put_market_data_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_market_data_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_market_data_generation_config_response& v);
    static ores::synthetic::messaging::put_market_data_generation_config_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_market_data_generation_config_response& v);
    static ores::synthetic::messaging::put_market_data_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_market_data_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::put_many_market_data_generation_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::put_many_market_data_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_market_data_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::put_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::put_many_market_data_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::put_many_market_data_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_market_data_generation_config_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_market_data_generation_config_request& v);
    static ores::synthetic::messaging::delete_market_data_generation_config_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_market_data_generation_config_request& v);
    static ores::synthetic::messaging::delete_market_data_generation_config_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_market_data_generation_config_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_market_data_generation_config_response& v);
    static ores::synthetic::messaging::delete_market_data_generation_config_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_market_data_generation_config_response& v);
    static ores::synthetic::messaging::delete_market_data_generation_config_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_market_data_generation_configs_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::delete_many_market_data_generation_configs_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_market_data_generation_configs_request& v);
    static ores::synthetic::messaging::delete_many_market_data_generation_configs_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_market_data_generation_configs_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::delete_many_market_data_generation_configs_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_market_data_generation_configs_response& v);
    static ores::synthetic::messaging::delete_many_market_data_generation_configs_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_market_data_generation_config_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_market_data_generation_config_versions_request& v);
    static ores::synthetic::messaging::list_market_data_generation_config_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_market_data_generation_config_versions_request& v);
    static ores::synthetic::messaging::list_market_data_generation_config_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_market_data_generation_config_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_market_data_generation_config_versions_response& v);
    static ores::synthetic::messaging::list_market_data_generation_config_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_market_data_generation_config_versions_response& v);
    static ores::synthetic::messaging::list_market_data_generation_config_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_data_generation_config_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_market_data_generation_config_version_request& v);
    static ores::synthetic::messaging::get_market_data_generation_config_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_market_data_generation_config_version_request& v);
    static ores::synthetic::messaging::get_market_data_generation_config_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_market_data_generation_config_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_market_data_generation_config_version_response& v);
    static ores::synthetic::messaging::get_market_data_generation_config_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_market_data_generation_config_version_response& v);
    static ores::synthetic::messaging::get_market_data_generation_config_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
