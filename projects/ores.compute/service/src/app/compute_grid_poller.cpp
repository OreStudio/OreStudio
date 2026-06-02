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
#include "ores.compute.service/app/compute_grid_poller.hpp"

#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"

namespace ores::compute::service::app {

using namespace ores::logging;

compute_grid_poller::compute_grid_poller(std::uint32_t interval_seconds,
                                         ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , ctx_(std::move(ctx)) {}

void compute_grid_poller::poll_once() {
    repository::compute_telemetry_repository telemetry_repo;
    const auto sample = telemetry_repo.compute_grid_stats(ctx_);
    telemetry_repo.insert_grid_sample(ctx_, sample);

    BOOST_LOG_SEV(lg(), debug)
        << "Grid sample: hosts=" << sample.total_hosts
        << " online=" << sample.online_hosts
        << " idle=" << sample.idle_hosts
        << " in_progress=" << sample.results_in_progress
        << " workunits=" << sample.total_workunits;
}

boost::asio::awaitable<void> compute_grid_poller::run() {
    BOOST_LOG_SEV(lg(), info)
        << "Compute grid poller started. Sampling every "
        << interval_seconds_ << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            try {
                poll_once();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Grid poll failed: " << e.what();
            }

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn)
                << "Grid poller timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Compute grid poller stopped.";
}

}
