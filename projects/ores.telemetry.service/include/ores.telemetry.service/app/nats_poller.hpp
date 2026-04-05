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
#ifndef ORES_TELEMETRY_SERVICE_APP_NATS_POLLER_HPP
#define ORES_TELEMETRY_SERVICE_APP_NATS_POLLER_HPP

#include <string>
#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.telemetry.database/repository/telemetry_repository.hpp"

namespace ores::telemetry::service::app {

/**
 * @brief Background coroutine that polls the NATS monitoring HTTP API
 *        and persists server and stream metrics to TimescaleDB.
 *
 * Polls /varz (server-level stats) and /jsz?streams=true (per-stream
 * JetStream stats) at a configurable interval.  Each poll inserts one
 * row per call into ores_nats_server_samples_tbl and one row per stream
 * into ores_nats_stream_samples_tbl.
 */
class nats_poller {
private:
    inline static std::string_view logger_name =
        "ores.telemetry.service.app.nats_poller";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    nats_poller(const std::string& monitor_url,
                std::uint32_t interval_seconds,
                ores::database::context ctx);

    /**
     * @brief Runs the polling loop as an awaitable coroutine.
     *
     * Sleeps for @c interval_seconds between each poll.  The coroutine
     * exits cleanly when its steady_timer is cancelled (e.g. when the
     * io_context shuts down on SIGINT/SIGTERM).
     */
    boost::asio::awaitable<void> run();

private:
    /**
     * @brief Performs one synchronous HTTP GET and returns the body.
     */
    std::string http_get(const std::string& target) const;

    /**
     * @brief Fetches /varz, parses it, and inserts a server sample.
     */
    void poll_server();

    /**
     * @brief Fetches /jsz?streams=true, parses it, and inserts stream samples.
     */
    void poll_streams();

    /**
     * @brief Runs one full poll cycle (server + streams).
     */
    void poll_once();

    std::string monitor_host_;
    unsigned short monitor_port_{8222};
    std::uint32_t interval_seconds_;
    ores::database::context ctx_;
    database::repository::telemetry_repository repo_;
    unsigned int consecutive_failures_{0};
};

}

#endif
