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
#ifndef ORES_SERVICE_SERVICE_SERVICE_LIFECYCLE_HPP
#define ORES_SERVICE_SERVICE_SERVICE_LIFECYCLE_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/jwks.hpp"
#include "ores.platform/process/signals.hpp"
#include "ores.service/service/systemd_notify.hpp"
#include <boost/asio/bind_cancellation_slot.hpp>
#include <boost/asio/cancellation_signal.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/error.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/system/error_code.hpp>
#include <functional>
#include <optional>
#include <string>
#include <string_view>

namespace ores::service::service {

namespace detail {

inline auto& runner_logger() {
    static auto instance = ores::logging::make_logger("ores.service.service.runner");
    return instance;
}

inline auto& signing_runner_logger() {
    static auto instance = ores::logging::make_logger("ores.service.service.signing_runner");
    return instance;
}

/**
 * @brief Adds every signal that means "shut down" to @p signals.
 *
 * The set is constructed by the caller because boost::asio::signal_set is not
 * movable, so it cannot be returned from a factory.
 */
inline void add_shutdown_signals(boost::asio::signal_set& signals) {
    for (int sig : ores::platform::process::shutdown_signals)
        signals.add(sig);
}

/**
 * @brief Turns a shutdown signal into a cancellation on @p startup_cancel.
 *
 * The handler emits on the cancellation signal rather than calling
 * io_ctx.stop(), so the startup coroutine can unwind cleanly (log, co_return)
 * before the io_context exits. io_ctx.stop() would let io_ctx.run() in main()
 * return before the startup handler's catch block executes.
 *
 * The caller cancels the handler with signals.cancel() before re-arming the
 * same signal set for the operational phase. Only a service with an abortable
 * startup phase needs this; a service that reaches its signal wait directly
 * waits on the set without it.
 */
inline void arm_startup_cancellation(boost::asio::signal_set& signals,
                                     boost::asio::cancellation_signal& startup_cancel) {
    signals.async_wait([&startup_cancel](const boost::system::error_code& ec, int) {
        if (!ec)
            startup_cancel.emit(boost::asio::cancellation_type::all);
    });
}

/**
 * @brief Fetches the JWKS public key from IAM, abortable by a shutdown signal.
 *
 * @return The key, or std::nullopt when a shutdown signal arrived first. The
 * caller treats the nullopt as a clean early exit: no subscription is
 * registered yet, so there is nothing to drain.
 */
inline boost::asio::awaitable<std::optional<std::string>>
fetch_public_key_or_shutdown(boost::asio::io_context& io_ctx,
                             ores::nats::service::client& nats,
                             boost::asio::cancellation_signal& startup_cancel,
                             std::string_view name,
                             ores::logging::logger_t& lg) {
    using namespace ores::logging;

    try {
        // bind_cancellation_slot propagates the slot to the spawned coroutine:
        // when startup_cancel.emit() fires, the coroutine is cancelled at its
        // next async point (the backoff timer) with operation_aborted.
        co_return co_await boost::asio::co_spawn(
            io_ctx,
            ores::nats::service::fetch_jwks_public_key(nats),
            boost::asio::bind_cancellation_slot(startup_cancel.slot(), boost::asio::use_awaitable));
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg, error) << "Fatal error during startup: " << e.what();
            throw;
        }
        BOOST_LOG_SEV(lg, info) << "Shutdown signal received during startup.";
        BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
        co_return std::nullopt;
    }
}

/**
 * @brief Registers the handlers, announces readiness, and drains on shutdown.
 *
 * The tail every service lifecycle shares: register, log the subscription
 * count, run the optional start hook, tell systemd, wait for a shutdown signal,
 * run the optional stop hook, and drain NATS.
 *
 * @param register_fn Called once with no arguments; the caller binds whatever
 * the service needs (database context, verifier or signer) into it. Taken by
 * value so the state it owns outlives the caller's frame.
 */
template <typename RegisterFn>
boost::asio::awaitable<void>
announce_ready_and_drain(boost::asio::io_context& io_ctx,
                         ores::nats::service::client& nats,
                         std::string_view name,
                         boost::asio::signal_set& signals,
                         RegisterFn register_fn,
                         std::function<void(boost::asio::io_context&)> on_started,
                         std::function<void()> on_shutdown,
                         ores::logging::logger_t& lg) {
    using namespace ores::logging;

    auto subs = register_fn();
    BOOST_LOG_SEV(lg, info) << "Registered " << subs.size() << " subscription(s).";

    if (on_started)
        on_started(io_ctx);

    BOOST_LOG_SEV(lg, info) << "Service ready.";
    notify_systemd_ready();
    BOOST_LOG_SEV(lg, info) << "Waiting for requests...";
    co_await signals.async_wait(boost::asio::use_awaitable);

    BOOST_LOG_SEV(lg, info) << "Shutdown signal received. Draining...";
    if (on_shutdown)
        on_shutdown();
    nats.drain();
    BOOST_LOG_SEV(lg, info) << "Shutdown complete: " << name;
    co_return;
}

}

}

#endif
