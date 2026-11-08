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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_MAPPING_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_DATA_MAPPING_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_data_mapping_codec.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct market_data_mapping_key {
    std::string oresmd_uri;

    friend bool operator==(const market_data_mapping_key&,
                           const market_data_mapping_key&) = default;
};

struct market_data_mapping_write {
    boost::uuids::uuid id;
    boost::uuids::uuid vendor_instrument_id;
    std::chrono::system_clock::time_point effective_from;
    std::string oresmd_uri;
    std::optional<std::chrono::system_clock::time_point> effective_to;

    friend bool operator==(const market_data_mapping_write&,
                           const market_data_mapping_write&) = default;
};

struct market_data_mapping_change {
    market_data_mapping_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const market_data_mapping_change&,
                           const market_data_mapping_change&) = default;
};

struct market_data_mapping_removal {
    market_data_mapping_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const market_data_mapping_removal&,
                           const market_data_mapping_removal&) = default;
};

struct market_data_mapping_lookup {
    market_data_mapping_key key;
    std::optional<ores::marketdata::domain::market_data_mapping> market_data_mapping;

    friend bool operator==(const market_data_mapping_lookup&,
                           const market_data_mapping_lookup&) = default;
};

struct market_data_mappings_filter {
    std::optional<boost::uuids::uuid> vendor_instrument_id;
    std::optional<std::string> oresmd_uri;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> vendor_instrument_id_one_of;
    std::optional<std::vector<std::string>> oresmd_uri_one_of;
    std::optional<std::string> search;

    friend bool operator==(const market_data_mappings_filter&,
                           const market_data_mappings_filter&) = default;
};

struct market_data_mapping_event {
    boost::uuids::uuid event_id;
    market_data_mapping_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const market_data_mapping_event&,
                           const market_data_mapping_event&) = default;
};

struct market_data_mapping_version_key {
    market_data_mapping_key market_data_mapping;
    std::uint32_t version{};

    friend bool operator==(const market_data_mapping_version_key&,
                           const market_data_mapping_version_key&) = default;
};

struct market_data_mapping_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const market_data_mapping_versions_filter&,
                           const market_data_mapping_versions_filter&) = default;
};

struct list_market_data_mappings_request {
    using response_type = struct list_market_data_mappings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.list";
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
    std::optional<market_data_mappings_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_market_data_mappings_request&,
                           const list_market_data_mappings_request&) = default;
};

struct list_market_data_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_mapping> mappings;
    std::uint64_t total{};

    friend bool operator==(const list_market_data_mappings_response&,
                           const list_market_data_mappings_response&) = default;
};

struct get_market_data_mapping_request {
    using response_type = struct get_market_data_mapping_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_mapping_key key;

    friend bool operator==(const get_market_data_mapping_request&,
                           const get_market_data_mapping_request&) = default;
};

struct get_market_data_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_mapping> market_data_mapping;

    friend bool operator==(const get_market_data_mapping_response&,
                           const get_market_data_mapping_response&) = default;
};

struct get_many_market_data_mappings_request {
    using response_type = struct get_many_market_data_mappings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_mapping_key> keys;

    friend bool operator==(const get_many_market_data_mappings_request&,
                           const get_many_market_data_mappings_request&) = default;
};

struct get_many_market_data_mappings_response {
    ores::utility::domain::result result;
    std::vector<market_data_mapping_lookup> entries;

    friend bool operator==(const get_many_market_data_mappings_response&,
                           const get_many_market_data_mappings_response&) = default;
};

struct put_market_data_mapping_request {
    using response_type = struct put_market_data_mapping_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_mapping_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_market_data_mapping_request&,
                           const put_market_data_mapping_request&) = default;
};

struct put_market_data_mapping_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_mapping> market_data_mapping;

    friend bool operator==(const put_market_data_mapping_response&,
                           const put_market_data_mapping_response&) = default;
};

struct put_many_market_data_mappings_request {
    using response_type = struct put_many_market_data_mappings_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_mapping_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_market_data_mappings_request&,
                           const put_many_market_data_mappings_request&) = default;
};

struct put_many_market_data_mappings_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_mapping> mappings;

    friend bool operator==(const put_many_market_data_mappings_response&,
                           const put_many_market_data_mappings_response&) = default;
};

struct delete_market_data_mapping_request {
    using response_type = struct delete_market_data_mapping_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_data_mappings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_mapping_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_market_data_mapping_request&,
                           const delete_market_data_mapping_request&) = default;
};

struct delete_market_data_mapping_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_market_data_mapping_response&,
                           const delete_market_data_mapping_response&) = default;
};

struct delete_many_market_data_mappings_request {
    using response_type = struct delete_many_market_data_mappings_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_mappings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_data_mapping_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_market_data_mappings_request&,
                           const delete_many_market_data_mappings_request&) = default;
};

struct delete_many_market_data_mappings_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_market_data_mappings_response&,
                           const delete_many_market_data_mappings_response&) = default;
};

struct list_market_data_mapping_versions_request {
    using response_type = struct list_market_data_mapping_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_mappings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_mapping_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<market_data_mapping_versions_filter> filter;

    friend bool operator==(const list_market_data_mapping_versions_request&,
                           const list_market_data_mapping_versions_request&) = default;
};

struct list_market_data_mapping_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_data_mapping> versions;
    std::uint64_t total{};

    friend bool operator==(const list_market_data_mapping_versions_response&,
                           const list_market_data_mapping_versions_response&) = default;
};

struct get_market_data_mapping_version_request {
    using response_type = struct get_market_data_mapping_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_data_mappings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_data_mapping_version_key key;

    friend bool operator==(const get_market_data_mapping_version_request&,
                           const get_market_data_mapping_version_request&) = default;
};

struct get_market_data_mapping_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_data_mapping> version;

    friend bool operator==(const get_market_data_mapping_version_response&,
                           const get_market_data_mapping_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_data_mapping_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.market_data_mappings.created";
inline constexpr std::string_view updated = "marketdata.v1.events.market_data_mappings.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.market_data_mappings.deleted";
}
/**
 * @brief The members of market_data_mapping_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_key>) {
    return std::tuple{
        utility::codec::field{"oresmd_uri", &market_data_mapping_key::oresmd_uri},
    };
}

/**
 * @brief The members of market_data_mapping_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_write>) {
    return std::tuple{
        utility::codec::field{"id", &market_data_mapping_write::id},
        utility::codec::field{"vendor_instrument_id",
                              &market_data_mapping_write::vendor_instrument_id},
        utility::codec::field{"effective_from", &market_data_mapping_write::effective_from},
        utility::codec::field{"oresmd_uri", &market_data_mapping_write::oresmd_uri},
        utility::codec::field{"effective_to", &market_data_mapping_write::effective_to},
    };
}

/**
 * @brief The members of market_data_mapping_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_change>) {
    return std::tuple{
        utility::codec::field{"write", &market_data_mapping_change::write},
        utility::codec::field{"precondition", &market_data_mapping_change::precondition},
    };
}

/**
 * @brief The members of market_data_mapping_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_removal>) {
    return std::tuple{
        utility::codec::field{"key", &market_data_mapping_removal::key},
        utility::codec::field{"precondition", &market_data_mapping_removal::precondition},
    };
}

/**
 * @brief The members of market_data_mapping_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &market_data_mapping_lookup::key},
        utility::codec::field{"market_data_mapping",
                              &market_data_mapping_lookup::market_data_mapping},
    };
}

/**
 * @brief The members of market_data_mappings_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mappings_filter>) {
    return std::tuple{
        utility::codec::field{"vendor_instrument_id",
                              &market_data_mappings_filter::vendor_instrument_id},
        utility::codec::field{"oresmd_uri", &market_data_mappings_filter::oresmd_uri},
        utility::codec::field{"id_one_of", &market_data_mappings_filter::id_one_of},
        utility::codec::field{"vendor_instrument_id_one_of",
                              &market_data_mappings_filter::vendor_instrument_id_one_of},
        utility::codec::field{"oresmd_uri_one_of", &market_data_mappings_filter::oresmd_uri_one_of},
        utility::codec::field{"search", &market_data_mappings_filter::search},
    };
}

/**
 * @brief The members of market_data_mapping_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &market_data_mapping_event::event_id},
        utility::codec::field{"key", &market_data_mapping_event::key},
        utility::codec::field{"action", &market_data_mapping_event::action},
        utility::codec::field{"version", &market_data_mapping_event::version},
        utility::codec::field{"occurred_at", &market_data_mapping_event::occurred_at},
        utility::codec::field{"correlation_id", &market_data_mapping_event::correlation_id},
    };
}

/**
 * @brief The members of market_data_mapping_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_version_key>) {
    return std::tuple{
        utility::codec::field{"market_data_mapping",
                              &market_data_mapping_version_key::market_data_mapping},
        utility::codec::field{"version", &market_data_mapping_version_key::version},
    };
}

/**
 * @brief The members of market_data_mapping_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_data_mapping_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &market_data_mapping_versions_filter::version},
        utility::codec::field{"from_version", &market_data_mapping_versions_filter::from_version},
        utility::codec::field{"to_version", &market_data_mapping_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_market_data_mappings_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_market_data_mappings_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_market_data_mappings_request::offset},
        utility::codec::field{"limit", &list_market_data_mappings_request::limit},
        utility::codec::field{"order", &list_market_data_mappings_request::order},
        utility::codec::field{"filter", &list_market_data_mappings_request::filter},
        utility::codec::field{"as_of", &list_market_data_mappings_request::as_of},
    };
}

/**
 * @brief The members of list_market_data_mappings_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_market_data_mappings_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_market_data_mappings_response::result},
        utility::codec::field{"mappings", &list_market_data_mappings_response::mappings},
        utility::codec::field{"total", &list_market_data_mappings_response::total},
    };
}

/**
 * @brief The members of get_market_data_mapping_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_data_mapping_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_mapping_request::key},
    };
}

/**
 * @brief The members of get_market_data_mapping_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_data_mapping_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_market_data_mapping_response::result},
        utility::codec::field{"market_data_mapping",
                              &get_market_data_mapping_response::market_data_mapping},
    };
}

/**
 * @brief The members of get_many_market_data_mappings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_data_mappings_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_market_data_mappings_request::keys},
    };
}

/**
 * @brief The members of get_many_market_data_mappings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_data_mappings_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_market_data_mappings_response::result},
        utility::codec::field{"entries", &get_many_market_data_mappings_response::entries},
    };
}

/**
 * @brief The members of put_market_data_mapping_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_market_data_mapping_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_market_data_mapping_request::change},
        utility::codec::field{"intent", &put_market_data_mapping_request::intent},
    };
}

/**
 * @brief The members of put_market_data_mapping_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_market_data_mapping_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_market_data_mapping_response::result},
        utility::codec::field{"market_data_mapping",
                              &put_market_data_mapping_response::market_data_mapping},
    };
}

/**
 * @brief The members of put_many_market_data_mappings_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_data_mappings_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_market_data_mappings_request::changes},
        utility::codec::field{"intent", &put_many_market_data_mappings_request::intent},
    };
}

/**
 * @brief The members of put_many_market_data_mappings_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_data_mappings_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_market_data_mappings_response::result},
        utility::codec::field{"mappings", &put_many_market_data_mappings_response::mappings},
    };
}

/**
 * @brief The members of delete_market_data_mapping_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_market_data_mapping_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_market_data_mapping_request::removal},
        utility::codec::field{"intent", &delete_market_data_mapping_request::intent},
    };
}

/**
 * @brief The members of delete_market_data_mapping_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_market_data_mapping_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_market_data_mapping_response::result},
    };
}

/**
 * @brief The members of delete_many_market_data_mappings_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_data_mappings_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_market_data_mappings_request::removals},
        utility::codec::field{"intent", &delete_many_market_data_mappings_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_data_mappings_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_data_mappings_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_market_data_mappings_response::result},
    };
}

/**
 * @brief The members of list_market_data_mapping_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_mapping_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_market_data_mapping_versions_request::key},
        utility::codec::field{"offset", &list_market_data_mapping_versions_request::offset},
        utility::codec::field{"limit", &list_market_data_mapping_versions_request::limit},
        utility::codec::field{"order", &list_market_data_mapping_versions_request::order},
        utility::codec::field{"filter", &list_market_data_mapping_versions_request::filter},
    };
}

/**
 * @brief The members of list_market_data_mapping_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_market_data_mapping_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_market_data_mapping_versions_response::result},
        utility::codec::field{"versions", &list_market_data_mapping_versions_response::versions},
        utility::codec::field{"total", &list_market_data_mapping_versions_response::total},
    };
}

/**
 * @brief The members of get_market_data_mapping_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_mapping_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_market_data_mapping_version_request::key},
    };
}

/**
 * @brief The members of get_market_data_mapping_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_market_data_mapping_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_market_data_mapping_version_response::result},
        utility::codec::field{"version", &get_market_data_mapping_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_data_mapping_key& v);
    static ores::marketdata::messaging::market_data_mapping_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_data_mapping_key& v);
    static ores::marketdata::messaging::market_data_mapping_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_data_mapping_write& v);
    static ores::marketdata::messaging::market_data_mapping_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_write& v);
    static ores::marketdata::messaging::market_data_mapping_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_data_mapping_change& v);
    static ores::marketdata::messaging::market_data_mapping_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_change& v);
    static ores::marketdata::messaging::market_data_mapping_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_mapping_removal& v);
    static ores::marketdata::messaging::market_data_mapping_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_removal& v);
    static ores::marketdata::messaging::market_data_mapping_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_data_mapping_lookup& v);
    static ores::marketdata::messaging::market_data_mapping_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_lookup& v);
    static ores::marketdata::messaging::market_data_mapping_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mappings_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_mappings_filter& v);
    static ores::marketdata::messaging::market_data_mappings_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mappings_filter& v);
    static ores::marketdata::messaging::market_data_mappings_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_data_mapping_event& v);
    static ores::marketdata::messaging::market_data_mapping_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_event& v);
    static ores::marketdata::messaging::market_data_mapping_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_mapping_version_key& v);
    static ores::marketdata::messaging::market_data_mapping_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_version_key& v);
    static ores::marketdata::messaging::market_data_mapping_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_data_mapping_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::market_data_mapping_versions_filter& v);
    static ores::marketdata::messaging::market_data_mapping_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_data_mapping_versions_filter& v);
    static ores::marketdata::messaging::market_data_mapping_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_mappings_request& v);
    static ores::marketdata::messaging::list_market_data_mappings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_data_mappings_request& v);
    static ores::marketdata::messaging::list_market_data_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_mappings_response& v);
    static ores::marketdata::messaging::list_market_data_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_data_mappings_response& v);
    static ores::marketdata::messaging::list_market_data_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_mapping_request& v);
    static ores::marketdata::messaging::get_market_data_mapping_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_mapping_request& v);
    static ores::marketdata::messaging::get_market_data_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_mapping_response& v);
    static ores::marketdata::messaging::get_market_data_mapping_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_mapping_response& v);
    static ores::marketdata::messaging::get_market_data_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_data_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::get_many_market_data_mappings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::get_many_market_data_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_data_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::get_many_market_data_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::get_many_market_data_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_data_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_data_mapping_request& v);
    static ores::marketdata::messaging::put_market_data_mapping_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_data_mapping_request& v);
    static ores::marketdata::messaging::put_market_data_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_data_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_data_mapping_response& v);
    static ores::marketdata::messaging::put_market_data_mapping_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_data_mapping_response& v);
    static ores::marketdata::messaging::put_market_data_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_data_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::put_many_market_data_mappings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::put_many_market_data_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_data_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::put_many_market_data_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::put_many_market_data_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_data_mapping_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_data_mapping_request& v);
    static ores::marketdata::messaging::delete_market_data_mapping_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_data_mapping_request& v);
    static ores::marketdata::messaging::delete_market_data_mapping_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_data_mapping_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_data_mapping_response& v);
    static ores::marketdata::messaging::delete_market_data_mapping_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_data_mapping_response& v);
    static ores::marketdata::messaging::delete_market_data_mapping_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_data_mappings_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::delete_many_market_data_mappings_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_data_mappings_request& v);
    static ores::marketdata::messaging::delete_many_market_data_mappings_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_data_mappings_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::delete_many_market_data_mappings_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_data_mappings_response& v);
    static ores::marketdata::messaging::delete_many_market_data_mappings_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_mapping_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_mapping_versions_request& v);
    static ores::marketdata::messaging::list_market_data_mapping_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_data_mapping_versions_request& v);
    static ores::marketdata::messaging::list_market_data_mapping_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_data_mapping_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_data_mapping_versions_response& v);
    static ores::marketdata::messaging::list_market_data_mapping_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_data_mapping_versions_response& v);
    static ores::marketdata::messaging::list_market_data_mapping_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_mapping_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_mapping_version_request& v);
    static ores::marketdata::messaging::get_market_data_mapping_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_mapping_version_request& v);
    static ores::marketdata::messaging::get_market_data_mapping_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_data_mapping_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_data_mapping_version_response& v);
    static ores::marketdata::messaging::get_market_data_mapping_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_data_mapping_version_response& v);
    static ores::marketdata::messaging::get_market_data_mapping_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
