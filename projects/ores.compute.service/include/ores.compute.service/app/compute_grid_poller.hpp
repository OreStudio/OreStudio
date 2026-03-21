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
#ifndef ORES_COMPUTE_SERVICE_APP_COMPUTE_GRID_POLLER_HPP
#define ORES_COMPUTE_SERVICE_APP_COMPUTE_GRID_POLLER_HPP

#include <cstdint>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::compute::service::app {

/**
 * @brief Samples server-side compute grid metrics on a fixed interval.
 *
 * Reads host, result, workunit, and batch counts from the database, computes
 * aggregate statistics, and writes one row to ores_compute_grid_samples_tbl
 * every @p interval_seconds seconds.
 *
 * Analogous to nats_poller in ores.telemetry.service.
 */
class compute_grid_poller {
private:
    inline static std::string_view logger_name =
        "ores.compute.service.app.compute_grid_poller";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    compute_grid_poller(std::uint32_t interval_seconds,
                        ores::database::context ctx);

    /**
     * @brief Async coroutine entry point.
     *
     * Runs forever, sampling once per interval, until the io_context is
     * stopped (e.g. on SIGTERM).
     */
    boost::asio::awaitable<void> run();

private:
    void poll_once();

    std::uint32_t interval_seconds_;
    ores::database::context ctx_;
};

}

#endif
