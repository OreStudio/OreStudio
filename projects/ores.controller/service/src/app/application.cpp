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
#include "ores.controller.service/app/application.hpp"
#include "ores.controller.core/messaging/registrar.hpp"
#include "ores.controller.core/service/process_supervisor.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <filesystem>
#include <memory>

namespace ores::controller::service::app {

using namespace ores::logging;

namespace {

constexpr std::string_view service_name = "ores.controller.service";
constexpr std::string_view service_version = ORES_VERSION;

} // namespace

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = 4,
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.controller.service", 0, 1);

    const auto db_ctx = make_context(cfg.database);
    const auto bin_dir = std::filesystem::canonical("/proc/self/exe").parent_path();
    const auto log_level = cfg.logging ? cfg.logging->severity : std::string("info");

    service::process_supervisor supervisor(
        io_ctx, bin_dir, cfg.nats, log_level, db_ctx, cfg.http_port, cfg.wt_port);

    // Start all services in the background (dependency-ordered: IAM first,
    // then dependents). Runs concurrently with our own NATS connect and JWKS
    // fetch so the controller does not block on IAM startup.
    //
    // Spawned detached, NOT with use_awaitable held onto for a later
    // co_await: confirmed by direct experiment (and reproducible over
    // hundreds of retries / hours of runtime) that a co_spawn(...,
    // use_awaitable) whose returned awaitable isn't awaited immediately
    // never gets its first resume scheduled in some environments, even
    // though the same io_context is demonstrably running other coroutines
    // throughout. detached's first resume runs immediately and reliably in
    // every environment tested. Completion is instead signalled through
    // start_all_done -- a steady_timer set to time_point::max() and moved
    // into the past when start_all() finishes -- the standard Asio idiom
    // for a one-shot async event, so we can still await "has it finished"
    // without holding the coroutine's own completion token. Moving the
    // expiry into the past (rather than cancel()) matters: cancel() only
    // affects an *already pending* wait, so if start_all() finishes before
    // we reach the join point below (the common, fast case) a plain
    // cancel() would be a no-op and the later async_wait() would block
    // forever against an expiry of time_point::max(). An expiry in the
    // past makes any wait issued afterward complete immediately too.
    auto start_all_done =
        std::make_shared<boost::asio::steady_timer>(io_ctx, boost::asio::steady_timer::time_point::max());
    auto start_all_failure = std::make_shared<std::exception_ptr>();
    boost::asio::co_spawn(
        io_ctx,
        [&supervisor, start_all_done, start_all_failure]() -> boost::asio::awaitable<void> {
            try {
                co_await supervisor.start_all();
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "start_all() failed: " << e.what();
                *start_all_failure = std::current_exception();
            }
            start_all_done->expires_at(boost::asio::steady_timer::clock_type::time_point::min());
            co_return;
        },
        boost::asio::detached);

    std::exception_ptr failure;
    try {
        ores::nats::service::client nats(cfg.nats);
        nats.connect();
        BOOST_LOG_SEV(lg(), info) << "Connected to NATS: " << cfg.nats.url << " (namespace: '"
                                  << (cfg.nats.subject_prefix.empty() ? "(none)" :
                                                                        cfg.nats.subject_prefix)
                                  << "')";

        co_await ores::service::service::run(
            io_ctx,
            nats,
            db_ctx,
            "ores.controller.service",
            [&nats, &supervisor](auto& n, auto c, auto v) {
                return ores::controller::messaging::registrar::register_handlers(
                    n, std::move(c), std::move(v), &supervisor);
            },
            [&nats](boost::asio::io_context& ioc) {
                auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                    std::string(service_name), std::string(service_version), nats);
                boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
            });
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "ores.controller.service failed to start: " << e.what();
        failure = std::current_exception();
    }

    // Whether we got here via normal shutdown or the catch above, start_all()
    // may still be in flight — always wait for it to finish before touching
    // `supervisor` again (stop_all(), then its destructor) to avoid the
    // use-after-free race described above: destroying process_supervisor
    // while start_all() is still running races its internal thread pool
    // teardown against the still-executing coroutine. async_wait() on an
    // already-cancelled timer completes immediately with operation_aborted,
    // which is exactly our "start_all() already finished" signal, not an
    // error to propagate.
    try {
        co_await start_all_done->async_wait(boost::asio::use_awaitable);
    } catch (const boost::system::system_error&) {
        // Expected: cancel() completes the wait with operation_aborted.
    }
    if (*start_all_failure && !failure)
        failure = *start_all_failure;

    // Gracefully stop all child processes before we exit. Without this,
    // children never receive SIGTERM and keep running as orphans.
    co_await supervisor.stop_all();

    // Rethrow after teardown so main()'s existing exception handling still
    // reports the failure and exits non-zero — we only needed to defer this
    // past the graceful-shutdown steps above, not swallow it.
    if (failure)
        std::rethrow_exception(failure);

    co_return;
}

}
