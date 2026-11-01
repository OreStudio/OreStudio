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
#ifndef ORES_TELEMETRY_SERVICE_APP_NATS_MONITORING_REPLIES_HPP
#define ORES_TELEMETRY_SERVICE_APP_NATS_MONITORING_REPLIES_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace ores::telemetry::service::app {

/**
 * @brief Maps the relevant fields of the NATS /varz response.
 *
 * Field names match the NATS monitoring HTTP API.
 * Note: NATS reports memory as "mem" (bytes).
 */
struct varz_response {
    std::uint64_t in_msgs{0};
    std::uint64_t out_msgs{0};
    std::uint64_t in_bytes{0};
    std::uint64_t out_bytes{0};
    int connections{0};
    std::uint64_t mem{0};
    int slow_consumers{0};
};

constexpr auto wire_fields(const varz_response&) {
    return std::tuple{utility::codec::field{"in_msgs", &varz_response::in_msgs},
                      utility::codec::field{"out_msgs", &varz_response::out_msgs},
                      utility::codec::field{"in_bytes", &varz_response::in_bytes},
                      utility::codec::field{"out_bytes", &varz_response::out_bytes},
                      utility::codec::field{"connections", &varz_response::connections},
                      utility::codec::field{"mem", &varz_response::mem},
                      utility::codec::field{"slow_consumers", &varz_response::slow_consumers}};
}
static_assert(utility::codec::names_every_member<varz_response>());

struct jsz_stream_state {
    std::uint64_t messages{0};
    std::uint64_t bytes{0};
    int consumer_count{0};
};

constexpr auto wire_fields(const jsz_stream_state&) {
    return std::tuple{utility::codec::field{"messages", &jsz_stream_state::messages},
                      utility::codec::field{"bytes", &jsz_stream_state::bytes},
                      utility::codec::field{"consumer_count", &jsz_stream_state::consumer_count}};
}
static_assert(utility::codec::names_every_member<jsz_stream_state>());

/**
 * @brief One entry of the per-account "stream_detail" list.
 *
 * In nats-server v2.14 the /jsz "streams" field is the stream *count*
 * (an int), not the stream list — the details live under
 * account_details[].stream_detail[], and each entry is flat (name at
 * top level, not under config).
 */
struct jsz_stream_detail {
    std::string name;
    jsz_stream_state state;
};

constexpr auto wire_fields(const jsz_stream_detail&) {
    return std::tuple{utility::codec::field{"name", &jsz_stream_detail::name},
                      utility::codec::field{"state", &jsz_stream_detail::state}};
}
static_assert(utility::codec::names_every_member<jsz_stream_detail>());

struct jsz_account_detail {
    std::vector<jsz_stream_detail> stream_detail;
};

constexpr auto wire_fields(const jsz_account_detail&) {
    return std::tuple{utility::codec::field{"stream_detail", &jsz_account_detail::stream_detail}};
}
static_assert(utility::codec::names_every_member<jsz_account_detail>());

// nats-server tags account_details `omitempty` in its Go struct, so the
// field is absent from the JSON entirely when there are no JetStream
// accounts/streams to report (e.g. right after NATS starts). It must be
// optional or parsing fails every poll cycle in that state.
struct jsz_response {
    std::optional<std::vector<jsz_account_detail>> account_details;
};

constexpr auto wire_fields(const jsz_response&) {
    return std::tuple{utility::codec::field{"account_details", &jsz_response::account_details}};
}
static_assert(utility::codec::names_every_member<jsz_response>());

}

#endif
