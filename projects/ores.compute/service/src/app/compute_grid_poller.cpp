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
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"
#include "ores.compute.core/repository/grid_sample_repository.hpp"
#include "ores.compute.core/repository/work_tenant_repository.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::compute::service::app {

using namespace ores::logging;

compute_grid_poller::compute_grid_poller(std::uint32_t interval_seconds,
                                         ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , ctx_(std::move(ctx)) {}

void compute_grid_poller::poll_once() {
    repository::compute_telemetry_repository telemetry_repo;
    repository::grid_sample_repository sample_repo;
    // One instant for every row of the poll, so a session that reads the rows of a tenant
    // sums the rows of its parties as one sample.
    const auto sampled_at = std::chrono::system_clock::now();

    // The system tenant is counted first and carries the fleet. The other tenants are the
    // ones that have dispatched work, and each is entered to count its own work.
    const auto system = ores::utility::uuid::tenant_id::system().to_string();
    std::vector<std::string> tenants{system};
    for (const auto& tenant : repository::work_tenant_repository().list(ctx_)) {
        const auto id = boost::uuids::to_string(tenant);
        if (id != system)
            tenants.push_back(id);
    }

    std::size_t rows = 0;
    for (const auto& tenant : tenants) {
        try {
            const bool is_system = tenant == system;
            const auto tenant_ctx =
                is_system ? ctx_ : ores::database::service::tenant_context::with_tenant(ctx_, tenant);
            for (const auto& party : telemetry_repo.parties_with_work(tenant_ctx)) {
                // The row of the system tenant's work with no party also counts the hosts,
                // which belong to the installation.
                auto sample =
                    telemetry_repo.compute_grid_stats(tenant_ctx, party, is_system && !party);
                // The row is a fact about an instant rather than a running record, so the
                // writer names it and the generated repository writes it.
                sample.sampled_at = sampled_at;
                sample.id = boost::uuids::random_generator()();
                sample_repo.write(tenant_ctx, sample);
                ++rows;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Could not sample the grid for tenant " << tenant << ": " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Grid sample: " << rows << " row(s) for " << tenants.size()
                               << " tenant(s)";
}

boost::asio::awaitable<void> compute_grid_poller::run() {
    BOOST_LOG_SEV(lg(), info) << "Compute grid poller started. Sampling every " << interval_seconds_
                              << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            try {
                poll_once();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Grid poll failed: " << e.what();
            }

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn) << "Grid poller timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Compute grid poller stopped.";
}

}
