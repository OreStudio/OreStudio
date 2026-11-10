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
#ifndef ORES_MARKETDATA_API_MESSAGING_ADAPTER_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_ADAPTER_OPERATION_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One value a provider publishes: an id scheme, an external id and a field.
 *
 * The three parts are the vendor's own notation, so the text is stored exactly
 * as the vendor writes it, with its spaces and punctuation.
 */
struct vendor_id {
    std::string id_scheme;
    std::string external_id;
    std::string field;

    friend bool operator==(const vendor_id&, const vendor_id&) = default;
};

/**
 * @brief An id that an adapter refuses to serve, with the reason.
 *
 * The reason is a short code such as not_entitled or unknown_id.
 */
struct rejected_id {
    vendor_id id;
    std::string reason;

    friend bool operator==(const rejected_id&, const rejected_id&) = default;
};

/**
 * @brief An adapter announces itself, its provider and its universe.
 *
 * The universe is the set of ids the adapter can serve. The service checks it
 * against the catalogue of the provider and never takes it as the catalogue.
 */
struct adapter_announce_request {
    std::string adapter_id;
    std::string adapter_code;
    std::string module_version;
    std::string provider_code;
    std::vector<vendor_id> universe;

    friend bool operator==(const adapter_announce_request&,
                           const adapter_announce_request&) = default;
};

/**
 * @brief The service accepts or refuses an announce.
 *
 * The counts say how the universe compared with the catalogue.
 */
struct adapter_announce_response {
    bool success = false;
    std::string message;
    std::uint32_t served_count = 0;
    std::uint32_t unknown_count = 0;

    friend bool operator==(const adapter_announce_response&,
                           const adapter_announce_response&) = default;
};

/**
 * @brief The service tells an adapter which ids to publish.
 *
 * The set replaces the last one: an adapter opens subscriptions for the ids in it
 * and closes the others.
 */
struct adapter_demand_request {
    std::vector<vendor_id> ids;
    std::vector<vendor_id> image;

    friend bool operator==(const adapter_demand_request&, const adapter_demand_request&) = default;
};

/**
 * @brief The adapter accepts some ids and rejects the others, each with a reason.
 */
struct adapter_demand_response {
    std::vector<vendor_id> accepted;
    std::vector<rejected_id> rejected;

    friend bool operator==(const adapter_demand_response&,
                           const adapter_demand_response&) = default;
};

/**
 * @brief One value of one vendor id at one time.
 *
 * It carries no oresmd URI, no tenant, no party and no source kind. The service
 * maps the id and finds the parties.
 */
struct adapter_tick {
    std::string id_scheme;
    std::string external_id;
    std::string field;
    std::string value;
    std::chrono::system_clock::time_point vendor_time;
    std::uint64_t sequence = 0;

    friend bool operator==(const adapter_tick&, const adapter_tick&) = default;
};

/**
 * @brief A periodic sign of life.
 *
 * A missed heartbeat makes the adapter stale.
 */
struct adapter_heartbeat {
    std::string adapter_id;
    std::uint64_t counter = 0;

    friend bool operator==(const adapter_heartbeat&, const adapter_heartbeat&) = default;
};

/**
 * @brief The state of one adapter and the last demand it answered.
 *
 * The state is announced, ready, serving, stale or rejected. The counts say how the
 * announced universe compared with the catalogue. The accepted and rejected ids are
 * those of the last demand reply.
 */
struct adapter_status {
    std::string adapter_id;
    std::string adapter_code;
    std::string module_version;
    std::string provider_code;
    std::string state;
    std::chrono::system_clock::time_point last_heartbeat_at;
    std::uint64_t heartbeat_counter = 0;
    std::uint32_t served_count = 0;
    std::uint32_t unknown_count = 0;
    std::uint32_t demanded_count = 0;
    std::uint32_t routes_seen = 0;
    std::uint64_t ticks_last_minute = 0;
    std::int64_t last_tick_age_ms = -1;
    std::vector<vendor_id> accepted;
    std::vector<rejected_id> rejected;

    friend bool operator==(const adapter_status&, const adapter_status&) = default;
};

/**
 * @brief Asks for the state of every adapter the service tracks.
 *
 * The request carries nothing.
 */
struct list_adapter_status_request {
    using response_type = struct list_adapter_status_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.list_adapter_status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const list_adapter_status_request&,
                           const list_adapter_status_request&) = default;
};

/**
 * @brief The state of every adapter, in the order of their ids.
 */
struct list_adapter_status_response {
    std::vector<adapter_status> adapters;
    bool success = false;
    std::string message;

    friend bool operator==(const list_adapter_status_response&,
                           const list_adapter_status_response&) = default;
};

/**
 * @brief The members of vendor_id, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<vendor_id>) {
    return std::tuple{
        ::ores::utility::codec::field{"id_scheme", &vendor_id::id_scheme},
        ::ores::utility::codec::field{"external_id", &vendor_id::external_id},
        ::ores::utility::codec::field{"field", &vendor_id::field},
    };
}

/**
 * @brief The members of rejected_id, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<rejected_id>) {
    return std::tuple{
        ::ores::utility::codec::field{"id", &rejected_id::id},
        ::ores::utility::codec::field{"reason", &rejected_id::reason},
    };
}

/**
 * @brief The members of adapter_announce_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_announce_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"adapter_id", &adapter_announce_request::adapter_id},
        ::ores::utility::codec::field{"adapter_code", &adapter_announce_request::adapter_code},
        ::ores::utility::codec::field{"module_version", &adapter_announce_request::module_version},
        ::ores::utility::codec::field{"provider_code", &adapter_announce_request::provider_code},
        ::ores::utility::codec::field{"universe", &adapter_announce_request::universe},
    };
}

/**
 * @brief The members of adapter_announce_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_announce_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &adapter_announce_response::success},
        ::ores::utility::codec::field{"message", &adapter_announce_response::message},
        ::ores::utility::codec::field{"served_count", &adapter_announce_response::served_count},
        ::ores::utility::codec::field{"unknown_count", &adapter_announce_response::unknown_count},
    };
}

/**
 * @brief The members of adapter_demand_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_demand_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"ids", &adapter_demand_request::ids},
        ::ores::utility::codec::field{"image", &adapter_demand_request::image},
    };
}

/**
 * @brief The members of adapter_demand_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_demand_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"accepted", &adapter_demand_response::accepted},
        ::ores::utility::codec::field{"rejected", &adapter_demand_response::rejected},
    };
}

/**
 * @brief The members of adapter_tick, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_tick>) {
    return std::tuple{
        ::ores::utility::codec::field{"id_scheme", &adapter_tick::id_scheme},
        ::ores::utility::codec::field{"external_id", &adapter_tick::external_id},
        ::ores::utility::codec::field{"field", &adapter_tick::field},
        ::ores::utility::codec::field{"value", &adapter_tick::value},
        ::ores::utility::codec::field{"vendor_time", &adapter_tick::vendor_time},
        ::ores::utility::codec::field{"sequence", &adapter_tick::sequence},
    };
}

/**
 * @brief The members of adapter_heartbeat, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_heartbeat>) {
    return std::tuple{
        ::ores::utility::codec::field{"adapter_id", &adapter_heartbeat::adapter_id},
        ::ores::utility::codec::field{"counter", &adapter_heartbeat::counter},
    };
}

/**
 * @brief The members of adapter_status, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<adapter_status>) {
    return std::tuple{
        ::ores::utility::codec::field{"adapter_id", &adapter_status::adapter_id},
        ::ores::utility::codec::field{"adapter_code", &adapter_status::adapter_code},
        ::ores::utility::codec::field{"module_version", &adapter_status::module_version},
        ::ores::utility::codec::field{"provider_code", &adapter_status::provider_code},
        ::ores::utility::codec::field{"state", &adapter_status::state},
        ::ores::utility::codec::field{"last_heartbeat_at", &adapter_status::last_heartbeat_at},
        ::ores::utility::codec::field{"heartbeat_counter", &adapter_status::heartbeat_counter},
        ::ores::utility::codec::field{"served_count", &adapter_status::served_count},
        ::ores::utility::codec::field{"unknown_count", &adapter_status::unknown_count},
        ::ores::utility::codec::field{"demanded_count", &adapter_status::demanded_count},
        ::ores::utility::codec::field{"routes_seen", &adapter_status::routes_seen},
        ::ores::utility::codec::field{"ticks_last_minute", &adapter_status::ticks_last_minute},
        ::ores::utility::codec::field{"last_tick_age_ms", &adapter_status::last_tick_age_ms},
        ::ores::utility::codec::field{"accepted", &adapter_status::accepted},
        ::ores::utility::codec::field{"rejected", &adapter_status::rejected},
    };
}

/**
 * @brief The members of list_adapter_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_adapter_status_request>) {
    return std::tuple{};
}

/**
 * @brief The members of list_adapter_status_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_adapter_status_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"adapters", &list_adapter_status_response::adapters},
        ::ores::utility::codec::field{"success", &list_adapter_status_response::success},
        ::ores::utility::codec::field{"message", &list_adapter_status_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::vendor_id> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::vendor_id& v);
    static ores::marketdata::messaging::vendor_id read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::vendor_id& v);
    static ores::marketdata::messaging::vendor_id read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::rejected_id> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::rejected_id& v);
    static ores::marketdata::messaging::rejected_id read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::rejected_id& v);
    static ores::marketdata::messaging::rejected_id read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::adapter_announce_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_announce_request& v);
    static ores::marketdata::messaging::adapter_announce_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::adapter_announce_request& v);
    static ores::marketdata::messaging::adapter_announce_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::adapter_announce_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_announce_response& v);
    static ores::marketdata::messaging::adapter_announce_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::adapter_announce_response& v);
    static ores::marketdata::messaging::adapter_announce_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::adapter_demand_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_demand_request& v);
    static ores::marketdata::messaging::adapter_demand_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::adapter_demand_request& v);
    static ores::marketdata::messaging::adapter_demand_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::adapter_demand_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_demand_response& v);
    static ores::marketdata::messaging::adapter_demand_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::adapter_demand_response& v);
    static ores::marketdata::messaging::adapter_demand_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::adapter_tick> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_tick& v);
    static ores::marketdata::messaging::adapter_tick read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::adapter_tick& v);
    static ores::marketdata::messaging::adapter_tick read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::adapter_heartbeat> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_heartbeat& v);
    static ores::marketdata::messaging::adapter_heartbeat read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::adapter_heartbeat& v);
    static ores::marketdata::messaging::adapter_heartbeat
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::adapter_status> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::adapter_status& v);
    static ores::marketdata::messaging::adapter_status read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::adapter_status& v);
    static ores::marketdata::messaging::adapter_status
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_adapter_status_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_adapter_status_request& v);
    static ores::marketdata::messaging::list_adapter_status_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_adapter_status_request& v);
    static ores::marketdata::messaging::list_adapter_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_adapter_status_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_adapter_status_response& v);
    static ores::marketdata::messaging::list_adapter_status_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_adapter_status_response& v);
    static ores::marketdata::messaging::list_adapter_status_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
