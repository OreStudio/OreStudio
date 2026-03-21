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

#include <chrono>
#include <set>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.compute/domain/grid_sample.hpp"
#include "ores.compute/repository/host_repository.hpp"
#include "ores.compute/repository/result_repository.hpp"
#include "ores.compute/repository/workunit_repository.hpp"
#include "ores.compute/repository/batch_repository.hpp"
#include "ores.compute/repository/compute_telemetry_repository.hpp"

namespace ores::compute::service::app {

using namespace ores::logging;

compute_grid_poller::compute_grid_poller(std::uint32_t interval_seconds,
                                         ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , ctx_(std::move(ctx)) {}

void compute_grid_poller::poll_once() {
    using namespace std::chrono;
    constexpr auto online_threshold = minutes(5);
    constexpr auto outcome_window   = hours(24);
    const auto now = system_clock::now();

    domain::grid_sample sample;
    sample.sampled_at = now;

    // -------------------------------------------------------------------------
    // Hosts
    // -------------------------------------------------------------------------
    repository::host_repository host_repo;
    const auto hosts = host_repo.read_latest(ctx_);
    sample.total_hosts = static_cast<int>(hosts.size());

    std::set<std::string> online_host_ids;
    for (const auto& h : hosts) {
        if (h.last_rpc_time == system_clock::time_point{})
            continue;
        if (now - h.last_rpc_time <= online_threshold)
            online_host_ids.insert(boost::uuids::to_string(h.id));
    }
    sample.online_hosts = static_cast<int>(online_host_ids.size());

    // -------------------------------------------------------------------------
    // Results — state breakdown + idle host count + 24h outcomes
    // -------------------------------------------------------------------------
    repository::result_repository result_repo;
    const auto results = result_repo.read_latest(ctx_);

    std::set<std::string> busy_host_ids;

    for (const auto& r : results) {
        switch (r.server_state) {
        case 1: ++sample.results_inactive;    break;
        case 2: ++sample.results_unsent;      break;
        case 4:
            ++sample.results_in_progress;
            busy_host_ids.insert(boost::uuids::to_string(r.host_id));
            break;
        case 5:
            ++sample.results_done;
            if (r.received_at != system_clock::time_point{} &&
                    now - r.received_at <= outcome_window) {
                switch (r.outcome) {
                case 1: ++sample.outcomes_success;      break;
                case 3: ++sample.outcomes_client_error; break;
                case 4: ++sample.outcomes_no_reply;     break;
                default: break;
                }
            }
            break;
        default: break;
        }
    }

    for (const auto& id : online_host_ids) {
        if (!busy_host_ids.count(id))
            ++sample.idle_hosts;
    }

    // -------------------------------------------------------------------------
    // Workunits
    // -------------------------------------------------------------------------
    repository::workunit_repository wu_repo;
    const auto workunits = wu_repo.read_latest(ctx_);
    sample.total_workunits = static_cast<int>(workunits.size());

    // -------------------------------------------------------------------------
    // Batches — total + active (has at least one InProgress result)
    // -------------------------------------------------------------------------
    repository::batch_repository batch_repo;
    const auto batches = batch_repo.read_latest(ctx_);
    sample.total_batches = static_cast<int>(batches.size());

    // Collect batch IDs that have at least one InProgress result.
    std::set<std::string> active_batch_ids;
    for (const auto& wu : workunits) {
        // A workunit belongs to a batch; if any of its results is InProgress
        // its batch is "active". We approximate: if a result is InProgress
        // and its workunit is in a batch, mark that batch active.
        // For now, count batches that own an InProgress workunit transitively.
        // (This avoids a separate join — good enough for telemetry.)
        (void)wu;  // populated via workunits vector; loop handled below
    }
    // Simpler approach: mark a batch active if any result for its workunits
    // is InProgress. Build workunit→batch map from workunits, then walk results.
    std::unordered_map<std::string, std::string> wu_to_batch;
    for (const auto& wu : workunits) {
        wu_to_batch[boost::uuids::to_string(wu.id)] =
            boost::uuids::to_string(wu.batch_id);
    }
    for (const auto& r : results) {
        if (r.server_state == 4) {
            const auto wuid = boost::uuids::to_string(r.workunit_id);
            const auto it = wu_to_batch.find(wuid);
            if (it != wu_to_batch.end())
                active_batch_ids.insert(it->second);
        }
    }
    sample.active_batches = static_cast<int>(active_batch_ids.size());

    // -------------------------------------------------------------------------
    // Persist
    // -------------------------------------------------------------------------
    repository::compute_telemetry_repository telemetry_repo;
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
