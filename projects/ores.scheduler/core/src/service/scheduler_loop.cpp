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
#include "ores.scheduler.core/service/scheduler_loop.hpp"

#include <span>
#include <stdexcept>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.eventing/domain/entity_change_event.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.scheduler.api/domain/job_instance.hpp"
#include "ores.scheduler.api/domain/job_status.hpp"
#include "ores.scheduler.core/repository/job_definition_repository.hpp"
#include "ores.scheduler.core/repository/job_instance_repository.hpp"

namespace ores::scheduler::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.scheduler.service.scheduler_loop");
    return instance;
}

} // anonymous namespace

scheduler_loop::scheduler_loop(ores::nats::service::client& nats,
    database::context system_ctx,
    std::vector<std::unique_ptr<action_handler>> handlers)
    : nats_(nats)
    , system_ctx_(std::move(system_ctx))
    , handlers_(std::move(handlers)) {}

void scheduler_loop::reload() {
    needs_reload_.store(true, std::memory_order_relaxed);
}

void scheduler_loop::publish_instance_event(std::string_view change_type,
    const domain::job_definition& job, std::int64_t inst_id) {
    try {
        eventing::domain::entity_change_event ev;
        ev.entity = "ores.scheduler.job_instance";
        ev.timestamp = std::chrono::system_clock::now();
        ev.entity_ids = {boost::uuids::to_string(job.id),
                         std::to_string(inst_id)};
        if (job.tenant_id)
            ev.tenant_id = boost::uuids::to_string(*job.tenant_id);

        const auto json = rfl::json::write(ev);
        const auto* p = reinterpret_cast<const std::byte*>(json.data());
        nats_.publish(job_instance_events_subject,
            std::span<const std::byte>(p, json.size()));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to publish instance event (" << change_type
            << ") for job " << job.job_name << ": " << e.what();
    }
}

void scheduler_loop::load_jobs() {
    BOOST_LOG_SEV(lg(), info) << "Loading all active job definitions.";

    repository::job_definition_repository repo;
    jobs_ = repo.read_all_active(system_ctx_);

    BOOST_LOG_SEV(lg(), info) << "Loaded " << jobs_.size() << " active jobs.";

    // Seed last_run_ from the job_instance table so we don't re-fire jobs
    // that already ran since the last minute boundary.
    repository::job_instance_repository inst_repo;
    for (const auto& job : jobs_) {
        if (last_run_.count(job.id) == 0) {
            auto last = inst_repo.last_run_at(system_ctx_, job.id);
            if (last) {
                last_run_[job.id] = *last;
                BOOST_LOG_SEV(lg(), debug) << "Seeded last_run for job "
                    << job.job_name << ": " << last_run_[job.id].time_since_epoch().count();
            }
        }
    }
}

boost::asio::awaitable<void>
scheduler_loop::fire_job(const domain::job_definition& job) {
    BOOST_LOG_SEV(lg(), info) << "Firing job: " << job.job_name;

    const auto now = std::chrono::system_clock::now();
    domain::job_instance inst;
    inst.tenant_id = job.tenant_id;
    inst.party_id = job.party_id;
    inst.job_definition_id = job.id;
    inst.action_type = job.action_type;
    inst.status = domain::job_status::starting;
    inst.triggered_at = now;
    inst.started_at = now;

    repository::job_instance_repository inst_repo;
    std::int64_t inst_id = 0;
    try {
        inst_id = inst_repo.write_started(system_ctx_, inst);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to write job instance for "
            << job.job_name << ": " << e.what();
        co_return;
    }
    publish_instance_event("created", job, inst_id);

    // Find the handler matching the job's action_type.
    action_handler* chosen = nullptr;
    for (const auto& h : handlers_) {
        if (h->action_type() == job.action_type) {
            chosen = h.get();
            break;
        }
    }

    if (!chosen) {
        const auto msg = "No handler found for action_type: " + job.action_type;
        BOOST_LOG_SEV(lg(), error) << msg << " (job: " << job.job_name << ")";
        try {
            inst_repo.write_completed(system_ctx_, inst_id, now,
                domain::job_status::failed, msg);
        } catch (...) {}
        publish_instance_event("updated", job, inst_id);
        last_run_[job.id] = now;
        co_return;
    }

    action_context actx{.job = job, .db_ctx = system_ctx_, .inst_id = inst_id};
    auto result = co_await chosen->execute(actx);
    if (result) {
        // Handler executed successfully.
        try {
            inst_repo.write_completed(system_ctx_, inst_id, now,
                domain::job_status::succeeded);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to write completion for "
                << job.job_name << ": " << e.what();
        }
        publish_instance_event("updated", job, inst_id);
        last_run_[job.id] = now;
        BOOST_LOG_SEV(lg(), info) << "Job succeeded: " << job.job_name;
    } else {
        const auto& err = result.error();
        try {
            inst_repo.write_completed(system_ctx_, inst_id, now,
                domain::job_status::failed, err);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to write failure for "
                << job.job_name << ": " << e.what();
        }
        publish_instance_event("updated", job, inst_id);
        last_run_[job.id] = now;
        BOOST_LOG_SEV(lg(), error) << "Job failed: " << job.job_name
                                   << " error: " << err;
    }
}

boost::asio::awaitable<void>
scheduler_loop::tick(boost::asio::io_context& ioc) {
    if (needs_reload_.exchange(false, std::memory_order_relaxed)) {
        BOOST_LOG_SEV(lg(), info) << "Reloading job definitions.";
        load_jobs();
    }

    const auto now = std::chrono::system_clock::now();

    for (const auto& job : jobs_) {
        if (!job.is_active) continue;

        // Determine the time point to compute next_occurrence from.
        auto it = last_run_.find(job.id);
        const auto after = (it != last_run_.end()) ? it->second
            : (now - std::chrono::minutes(1));

        const auto next = job.schedule_expression.next_occurrence(after);
        if (next <= now) {
            // Spawn the job in a detached coroutine so it doesn't block the tick.
            boost::asio::co_spawn(ioc,
                [this, job]() -> boost::asio::awaitable<void> {
                    co_await fire_job(job);
                },
                boost::asio::detached);
        }
    }

    co_return;
}

boost::asio::awaitable<void>
scheduler_loop::run(boost::asio::io_context& ioc) {
    BOOST_LOG_SEV(lg(), info) << "Scheduler loop starting.";

    load_jobs();

    while (true) {
        // Compute the duration until the next minute boundary.
        const auto now = std::chrono::system_clock::now();
        const auto now_t = std::chrono::system_clock::to_time_t(now);
        const auto next_min_t = ((now_t / 60) + 1) * 60;
        const auto next_min = std::chrono::system_clock::from_time_t(next_min_t);
        const auto wait_duration = next_min - now;

        boost::asio::steady_timer timer(ioc);
        timer.expires_after(wait_duration);

        boost::system::error_code ec;
        co_await timer.async_wait(
            boost::asio::redirect_error(boost::asio::use_awaitable, ec));

        if (ec == boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), info) << "Scheduler loop timer cancelled, stopping.";
            co_return;
        }

        co_await tick(ioc);
    }
}

} // namespace ores::scheduler::service
