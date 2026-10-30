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
#include "ores.marketdata.service/app/usage_recorder.hpp"
#include "ores.marketdata.api/domain/delivery_count.hpp"
#include "ores.marketdata.core/repository/delivery_count_repository.hpp"
#include "ores.marketdata.core/repository/demand_interval_repository.hpp"
#include "ores.marketdata.core/repository/live_tenant_repository.hpp"
#include "ores.marketdata.core/repository/subscription_interval_repository.hpp"
#include "ores.marketdata.service/app/live_tenants.hpp"
#include "ores.marketdata.service/app/usage_planner.hpp"
#include <boost/uuid/random_generator.hpp>
#include <map>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

constexpr auto usage_recorder_service_name = "ores.marketdata.service";

/// The number of waiting counts beyond which a store that is down is reported.
constexpr std::size_t pending_warning_limit = 100000;
}

usage_recorder::usage_recorder(ores::database::context ctx, std::shared_ptr<delivery_meter> meter)
    : ctx_(std::move(ctx))
    , meter_(std::move(meter)) {}

usage_recorder::~usage_recorder() {
    stop_.store(true, std::memory_order_relaxed);
    wake_.notify_all();
    if (flusher_.joinable())
        flusher_.join();
    flush(std::chrono::system_clock::now());
    if (meter_ && meter_->pending() > 0)
        BOOST_LOG_SEV(lg(), warn) << meter_->pending()
                                  << " delivery count(s) were not written at stop and are lost";
}

void usage_recorder::start() {
    flusher_ = std::thread(&usage_recorder::flush_loop, this);
}

void usage_recorder::sync(const demand_catalogue& catalogue,
                          std::chrono::system_clock::time_point now) {
    std::lock_guard lock(sync_mu_);
    try {
        const auto live = repository::live_tenant_repository{}.read(ctx_);
        const auto open_subscriptions = [&] {
            auto rows =
                repository::subscription_interval_repository{}.read_latest_all_tenants(ctx_);
            std::erase_if(rows, [](const auto& r) { return r.interval_to.has_value(); });
            keep_live_tenants(rows, live);
            return rows;
        }();
        const auto subscription_plan = plan_subscription_intervals(
            subscription_states(catalogue), open_subscriptions, now, ctx_.service_account());
        const auto refused = [](const auto& i, const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Failed to record a usage interval of tenant "
                                       << i.tenant_id << ": " << e.what();
        };
        const auto subscriptions = write_plan(
            subscription_plan,
            [this](const domain::subscription_interval& i) {
                repository::subscription_interval_repository{}.write(
                    ctx_.with_tenant(i.tenant_id, usage_recorder_service_name), i);
            },
            refused);

        const auto open_demand = [&] {
            auto rows = repository::demand_interval_repository{}.read_latest_all_tenants(ctx_);
            std::erase_if(rows, [](const auto& r) { return r.interval_to.has_value(); });
            keep_live_tenants(rows, live);
            return rows;
        }();
        const auto demand_plan =
            plan_demand_intervals(demand_states(compute_party_demand(catalogue, now)),
                                  open_demand,
                                  now,
                                  ctx_.service_account());
        const auto demand = write_plan(
            demand_plan,
            [this](const domain::demand_interval& i) {
                repository::demand_interval_repository{}.write(
                    ctx_.with_tenant(i.tenant_id, usage_recorder_service_name), i);
            },
            refused);

        BOOST_LOG_SEV(lg(), info) << "Usage intervals: " << subscriptions.opened
                                  << " subscription opened, " << subscriptions.closed << " closed; "
                                  << demand.opened << " demand opened, " << demand.closed
                                  << " closed; " << subscriptions.failed + demand.failed
                                  << " refused, " << subscriptions.held + demand.held
                                  << " held for the next sync";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to record the usage intervals: " << e.what();
    }
}

void usage_recorder::flush(std::chrono::system_clock::time_point now) {
    if (!meter_)
        return;
    auto counts = meter_->drain();
    if (counts.empty())
        return;

    std::map<std::string, std::vector<domain::delivery_count>> by_tenant;
    std::map<std::string, std::vector<delivery_entry>> entries_by_tenant;
    boost::uuids::random_generator gen;
    for (const auto& c : counts) {
        domain::delivery_count row;
        row.id = gen();
        row.tenant_id = c.key.tenant_id;
        row.party_id = c.key.party_id;
        row.provider_code = c.key.provider_code;
        row.feed_id = c.key.feed_id;
        row.oresmd_uri = c.key.oresmd_uri;
        row.period_start = c.key.period_start;
        row.flushed_at = now;
        row.deliveries = c.deliveries;
        row.modified_by = ctx_.service_account();
        row.performed_by = ctx_.service_account();
        row.change_reason_code = "system.new_record";
        row.change_commentary = "Deliveries counted by the fan-out";
        const auto tenant = c.key.tenant_id.to_string();
        by_tenant[tenant].push_back(std::move(row));
        entries_by_tenant[tenant].push_back(c);
    }

    for (auto& [tenant, rows] : by_tenant) {
        try {
            const auto tenant_ctx =
                ctx_.with_tenant(rows.front().tenant_id, usage_recorder_service_name);
            repository::delivery_count_repository{}.write(tenant_ctx, rows);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to write " << rows.size() << " delivery count(s): " << e.what();
            meter_->add_many(entries_by_tenant[tenant]);
            if (meter_->pending() > pending_warning_limit)
                BOOST_LOG_SEV(lg(), warn) << meter_->pending()
                                          << " delivery count(s) wait for a store that does not "
                                             "answer: memory grows until it does";
        }
    }
}

void usage_recorder::flush_loop() {
    std::unique_lock lock(wait_mu_);
    while (!stop_.load(std::memory_order_relaxed)) {
        wake_.wait_for(
            lock, flush_interval, [this] { return stop_.load(std::memory_order_relaxed); });
        if (stop_.load(std::memory_order_relaxed))
            break;
        lock.unlock();
        flush(std::chrono::system_clock::now());
        lock.lock();
    }
}

}
