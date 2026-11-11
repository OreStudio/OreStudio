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
#ifndef ORES_HTTP_ROUTES_SYNTHETIC_MARKET_REGIME_ROUTES_HPP
#define ORES_HTTP_ROUTES_SYNTHETIC_MARKET_REGIME_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include "ores.utility/codec/shaped_wire_traits.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <memory>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::http::routes::synthetic {

/**
 * @brief One decision of the scheduler's loop: a tick with the gap its law drew
 * and the wall gap it took, a regime turn, a pin, or a change of the feeds.
 */
struct market_trace_event final {
    std::string kind;
    std::string sourceName;
    std::chrono::system_clock::time_point at;
    double tau = 0.0;
    double operationalGap = 0.0;
    double wallGap = 0.0;
    double compensatedGap = 0.0;
    double heat = 0.0;
    double multiplier = 1.0;
    double pace = 1.0;
    std::string state;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_trace_event>) {
    using ores::utility::codec::field;
    using e = market_trace_event;
    return std::tuple{field{"kind", &e::kind},
                      field{"sourceName", &e::sourceName},
                      field{"at", &e::at},
                      field{"tau", &e::tau},
                      field{"operationalGap", &e::operationalGap},
                      field{"wallGap", &e::wallGap},
                      field{"compensatedGap", &e::compensatedGap},
                      field{"heat", &e::heat},
                      field{"multiplier", &e::multiplier},
                      field{"pace", &e::pace},
                      field{"state", &e::state}};
}

/// One running feed and the state of its arrival law now.
struct market_feed_law final {
    std::string sourceName;
    std::string mode;
    double rate = 0.0;
    double sensitivity = 1.0;
    double effectiveRate = 0.0;
    double phase = 0.0;
    double excess = 0.0;
    double branchingRatio = 0.0;
    double decay = 0.0;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_feed_law>) {
    using ores::utility::codec::field;
    using f = market_feed_law;
    return std::tuple{field{"sourceName", &f::sourceName},
                      field{"mode", &f::mode},
                      field{"rate", &f::rate},
                      field{"sensitivity", &f::sensitivity},
                      field{"effectiveRate", &f::effectiveRate},
                      field{"phase", &f::phase},
                      field{"excess", &f::excess},
                      field{"branchingRatio", &f::branchingRatio},
                      field{"decay", &f::decay}};
}

/**
 * @brief The market trace: the scheduler's recent decisions and its own
 * numbers, with every running feed's law now.
 */
struct market_trace final {
    std::uint64_t runSeed = 0;
    double ticksPerSecond = 0.0;
    std::uint64_t heapSize = 0;
    double latenessSeconds = 0.0;
    double maxLatenessSeconds = 0.0;
    std::uint64_t series = 0;
    std::vector<market_trace_event> events;
    std::vector<market_feed_law> feeds;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_trace>) {
    using ores::utility::codec::field;
    using t = market_trace;
    return std::tuple{field{"runSeed", &t::runSeed},
                      field{"ticksPerSecond", &t::ticksPerSecond},
                      field{"heapSize", &t::heapSize},
                      field{"latenessSeconds", &t::latenessSeconds},
                      field{"maxLatenessSeconds", &t::maxLatenessSeconds},
                      field{"series", &t::series},
                      field{"events", &t::events},
                      field{"feeds", &t::feeds}};
}

/**
 * @brief The market-wide arrival factor: the regime in force, the switch that
 * forces it, the settings that move it, the tick counts of the last stretch of
 * the run, and the market trace view.
 *
 * The factor belongs to the run rather than to a feed. A read the service does
 * not answer is the service failing, so it answers 502; settings the service
 * refuses are a bad request, so they answer 400 with its words.
 */
class market_regime_routes {
public:
    /// The market trace view from the service's trace.
    static market_trace
    trace_of(const ores::synthetic::messaging::get_market_trace_response& reply);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::market_trace_event>
    : shaped_wire_traits<ores::http::routes::synthetic::market_trace_event> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::market_feed_law>
    : shaped_wire_traits<ores::http::routes::synthetic::market_feed_law> {};

template <>
struct ores::utility::codec::wire_traits<ores::http::routes::synthetic::market_trace>
    : shaped_wire_traits<ores::http::routes::synthetic::market_trace> {};

#endif
