/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief A single point-in-time sample of NATS server-level metrics.
 */
export interface NatsServerSample {
    /**
     * @brief When this sample was taken.
     */
    sampled_at: string;
    /**
     * @brief Total inbound messages since server start.
     */
    in_msgs: number;
    /**
     * @brief Total outbound messages since server start.
     */
    out_msgs: number;
    /**
     * @brief Total inbound bytes since server start.
     */
    in_bytes: number;
    /**
     * @brief Total outbound bytes since server start.
     */
    out_bytes: number;
    /**
     * @brief Current number of client connections.
     */
    connections: number;
    /**
     * @brief Server process resident memory in bytes.
     */
    mem_bytes: number;
    /**
     * @brief Number of slow consumers detected since server start.
     */
    slow_consumers: number;
}

/**
 * @brief A single point-in-time sample of a JetStream stream's metrics.
 */
export interface NatsStreamSample {
    /**
     * @brief When this sample was taken.
     */
    sampled_at: string;
    /**
     * @brief JetStream stream name, for example @c ORES_TRADES.
     */
    stream_name: string;
    /**
     * @brief Number of messages currently stored in the stream.
     */
    messages: number;
    /**
     * @brief Total bytes currently stored in the stream.
     */
    bytes: number;
    /**
     * @brief Number of active consumers on this stream.
     */
    consumer_count: number;
}

/**
 * @brief Query parameters for retrieving NATS server samples.
 */
export interface NatsServerSamplesQuery {
    /**
     * @brief Start of the time range, inclusive.
     */
    start_time: string;
    /**
     * @brief End of the time range, exclusive.
     */
    end_time: string;
    /**
     * @brief Maximum number of results to return.
     */
    limit: number;
}

/**
 * @brief Query parameters for retrieving NATS stream samples.
 */
export interface NatsStreamSamplesQuery {
    /**
     * @brief JetStream stream name to query.
     */
    stream_name: string;
    /**
     * @brief Start of the time range, inclusive.
     */
    start_time: string;
    /**
     * @brief End of the time range, exclusive.
     */
    end_time: string;
    /**
     * @brief Maximum number of results to return.
     */
    limit: number;
}

/**
 * @brief Asks for the NATS server samples in a time range.
 */
export interface GetNatsServerSamplesRequest {
    /**
     * @brief The time range and limit the read applies.
     */
    query: NatsServerSamplesQuery;
}

/**
 * @brief The NATS server samples the query selected.
 */
export interface GetNatsServerSamplesResponse {
    /**
     * @brief Whether the samples were read.
     */
    success: boolean;
    /**
     * @brief Why they were not, when they were not.
     */
    message: string;
    /**
     * @brief The server samples the read returned.
     */
    samples: NatsServerSample[];
}

/**
 * @brief Asks for one stream's samples in a time range.
 */
export interface GetNatsStreamSamplesRequest {
    /**
     * @brief The stream, time range and limit the read applies.
     */
    query: NatsStreamSamplesQuery;
}

/**
 * @brief The NATS stream samples the query selected.
 */
export interface GetNatsStreamSamplesResponse {
    /**
     * @brief Whether the samples were read.
     */
    success: boolean;
    /**
     * @brief Why they were not, when they were not.
     */
    message: string;
    /**
     * @brief The stream samples the read returned.
     */
    samples: NatsStreamSample[];
}

export const subjects = {
    get_nats_server_samples_request: 'telemetry.v1.nats_server_samples.list',
    get_nats_stream_samples_request: 'telemetry.v1.nats_stream_samples.list',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_nats_server_samples_request: true,
    get_nats_stream_samples_request: true,
} as const;
