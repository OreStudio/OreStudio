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
#ifndef ORES_TELEMETRY_CORE_MESSAGING_NATS_SAMPLES_PROTOCOL_HPP
#define ORES_TELEMETRY_CORE_MESSAGING_NATS_SAMPLES_PROTOCOL_HPP

#include "ores.telemetry.core/export.hpp"
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

namespace ores::telemetry::messaging {

/**
 * @brief A single point-in-time sample of NATS server-level metrics.
 */
struct nats_server_sample {
    /**
     * @brief When this sample was taken.
     */
    std::chrono::system_clock::time_point sampled_at;
    /**
     * @brief Total inbound messages since server start.
     */
    std::uint64_t in_msgs = 0;
    /**
     * @brief Total outbound messages since server start.
     */
    std::uint64_t out_msgs = 0;
    /**
     * @brief Total inbound bytes since server start.
     */
    std::uint64_t in_bytes = 0;
    /**
     * @brief Total outbound bytes since server start.
     */
    std::uint64_t out_bytes = 0;
    /**
     * @brief Current number of client connections.
     */
    int connections = 0;
    /**
     * @brief Server process resident memory in bytes.
     */
    std::uint64_t mem_bytes = 0;
    /**
     * @brief Number of slow consumers detected since server start.
     */
    int slow_consumers = 0;
};

/**
 * @brief A single point-in-time sample of a JetStream stream's metrics.
 */
struct nats_stream_sample {
    /**
     * @brief When this sample was taken.
     */
    std::chrono::system_clock::time_point sampled_at;
    /**
     * @brief JetStream stream name, for example @c ORES_TRADES.
     */
    std::string stream_name;
    /**
     * @brief Number of messages currently stored in the stream.
     */
    std::uint64_t messages = 0;
    /**
     * @brief Total bytes currently stored in the stream.
     */
    std::uint64_t bytes = 0;
    /**
     * @brief Number of active consumers on this stream.
     */
    int consumer_count = 0;
};

/**
 * @brief Query parameters for retrieving NATS server samples.
 */
struct nats_server_samples_query {
    /**
     * @brief Start of the time range, inclusive.
     */
    std::chrono::system_clock::time_point start_time;
    /**
     * @brief End of the time range, exclusive.
     */
    std::chrono::system_clock::time_point end_time;
    /**
     * @brief Maximum number of results to return.
     */
    std::uint32_t limit = 1000;
};

/**
 * @brief Query parameters for retrieving NATS stream samples.
 */
struct nats_stream_samples_query {
    /**
     * @brief JetStream stream name to query.
     */
    std::string stream_name;
    /**
     * @brief Start of the time range, inclusive.
     */
    std::chrono::system_clock::time_point start_time;
    /**
     * @brief End of the time range, exclusive.
     */
    std::chrono::system_clock::time_point end_time;
    /**
     * @brief Maximum number of results to return.
     */
    std::uint32_t limit = 1000;
};

/**
 * @brief Asks for the NATS server samples in a time range.
 */
struct get_nats_server_samples_request {
    using response_type = struct get_nats_server_samples_response;
    static constexpr std::string_view nats_subject = "telemetry.v1.nats_server_samples.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The time range and limit the read applies.
     */
    nats_server_samples_query query;
};

/**
 * @brief The NATS server samples the query selected.
 */
struct get_nats_server_samples_response {
    /**
     * @brief Whether the samples were read.
     */
    bool success = false;
    /**
     * @brief Why they were not, when they were not.
     */
    std::string message;
    /**
     * @brief The server samples the read returned.
     */
    std::vector<nats_server_sample> samples;
};

/**
 * @brief Asks for one stream's samples in a time range.
 */
struct get_nats_stream_samples_request {
    using response_type = struct get_nats_stream_samples_response;
    static constexpr std::string_view nats_subject = "telemetry.v1.nats_stream_samples.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The stream, time range and limit the read applies.
     */
    nats_stream_samples_query query;
};

/**
 * @brief The NATS stream samples the query selected.
 */
struct get_nats_stream_samples_response {
    /**
     * @brief Whether the samples were read.
     */
    bool success = false;
    /**
     * @brief Why they were not, when they were not.
     */
    std::string message;
    /**
     * @brief The stream samples the read returned.
     */
    std::vector<nats_stream_sample> samples;
};

/**
 * @brief The members of nats_server_sample, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const nats_server_sample&) {
    return std::tuple{
        ::ores::utility::codec::field{"sampled_at", &nats_server_sample::sampled_at},
        ::ores::utility::codec::field{"in_msgs", &nats_server_sample::in_msgs},
        ::ores::utility::codec::field{"out_msgs", &nats_server_sample::out_msgs},
        ::ores::utility::codec::field{"in_bytes", &nats_server_sample::in_bytes},
        ::ores::utility::codec::field{"out_bytes", &nats_server_sample::out_bytes},
        ::ores::utility::codec::field{"connections", &nats_server_sample::connections},
        ::ores::utility::codec::field{"mem_bytes", &nats_server_sample::mem_bytes},
        ::ores::utility::codec::field{"slow_consumers", &nats_server_sample::slow_consumers},
    };
}

/**
 * @brief The members of nats_stream_sample, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const nats_stream_sample&) {
    return std::tuple{
        ::ores::utility::codec::field{"sampled_at", &nats_stream_sample::sampled_at},
        ::ores::utility::codec::field{"stream_name", &nats_stream_sample::stream_name},
        ::ores::utility::codec::field{"messages", &nats_stream_sample::messages},
        ::ores::utility::codec::field{"bytes", &nats_stream_sample::bytes},
        ::ores::utility::codec::field{"consumer_count", &nats_stream_sample::consumer_count},
    };
}

/**
 * @brief The members of nats_server_samples_query, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const nats_server_samples_query&) {
    return std::tuple{
        ::ores::utility::codec::field{"start_time", &nats_server_samples_query::start_time},
        ::ores::utility::codec::field{"end_time", &nats_server_samples_query::end_time},
        ::ores::utility::codec::field{"limit", &nats_server_samples_query::limit},
    };
}

/**
 * @brief The members of nats_stream_samples_query, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const nats_stream_samples_query&) {
    return std::tuple{
        ::ores::utility::codec::field{"stream_name", &nats_stream_samples_query::stream_name},
        ::ores::utility::codec::field{"start_time", &nats_stream_samples_query::start_time},
        ::ores::utility::codec::field{"end_time", &nats_stream_samples_query::end_time},
        ::ores::utility::codec::field{"limit", &nats_stream_samples_query::limit},
    };
}

/**
 * @brief The members of get_nats_server_samples_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_nats_server_samples_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"query", &get_nats_server_samples_request::query},
    };
}

/**
 * @brief The members of get_nats_server_samples_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_nats_server_samples_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_nats_server_samples_response::success},
        ::ores::utility::codec::field{"message", &get_nats_server_samples_response::message},
        ::ores::utility::codec::field{"samples", &get_nats_server_samples_response::samples},
    };
}

/**
 * @brief The members of get_nats_stream_samples_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_nats_stream_samples_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"query", &get_nats_stream_samples_request::query},
    };
}

/**
 * @brief The members of get_nats_stream_samples_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_nats_stream_samples_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_nats_stream_samples_response::success},
        ::ores::utility::codec::field{"message", &get_nats_stream_samples_response::message},
        ::ores::utility::codec::field{"samples", &get_nats_stream_samples_response::samples},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_TELEMETRY_CORE_EXPORT wire_traits<ores::telemetry::messaging::nats_server_sample> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::telemetry::messaging::nats_server_sample& v);
    static ores::telemetry::messaging::nats_server_sample read_json(std::string_view text);
    static std::string write_msgpack(const ores::telemetry::messaging::nats_server_sample& v);
    static ores::telemetry::messaging::nats_server_sample
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT wire_traits<ores::telemetry::messaging::nats_stream_sample> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::telemetry::messaging::nats_stream_sample& v);
    static ores::telemetry::messaging::nats_stream_sample read_json(std::string_view text);
    static std::string write_msgpack(const ores::telemetry::messaging::nats_stream_sample& v);
    static ores::telemetry::messaging::nats_stream_sample
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::nats_server_samples_query> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::telemetry::messaging::nats_server_samples_query& v);
    static ores::telemetry::messaging::nats_server_samples_query read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::nats_server_samples_query& v);
    static ores::telemetry::messaging::nats_server_samples_query
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::nats_stream_samples_query> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::telemetry::messaging::nats_stream_samples_query& v);
    static ores::telemetry::messaging::nats_stream_samples_query read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::nats_stream_samples_query& v);
    static ores::telemetry::messaging::nats_stream_samples_query
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::get_nats_server_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::telemetry::messaging::get_nats_server_samples_request& v);
    static ores::telemetry::messaging::get_nats_server_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::get_nats_server_samples_request& v);
    static ores::telemetry::messaging::get_nats_server_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::get_nats_server_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::telemetry::messaging::get_nats_server_samples_response& v);
    static ores::telemetry::messaging::get_nats_server_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::get_nats_server_samples_response& v);
    static ores::telemetry::messaging::get_nats_server_samples_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::get_nats_stream_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::telemetry::messaging::get_nats_stream_samples_request& v);
    static ores::telemetry::messaging::get_nats_stream_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::get_nats_stream_samples_request& v);
    static ores::telemetry::messaging::get_nats_stream_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TELEMETRY_CORE_EXPORT
    wire_traits<ores::telemetry::messaging::get_nats_stream_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::telemetry::messaging::get_nats_stream_samples_response& v);
    static ores::telemetry::messaging::get_nats_stream_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::telemetry::messaging::get_nats_stream_samples_response& v);
    static ores::telemetry::messaging::get_nats_stream_samples_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
