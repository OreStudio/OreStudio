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

#include <chrono>
#include <cstdint>
#include <string>
#include <string_view>
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

}

#endif
