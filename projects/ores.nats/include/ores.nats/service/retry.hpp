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
#ifndef ORES_NATS_SERVICE_RETRY_HPP
#define ORES_NATS_SERVICE_RETRY_HPP

#include <chrono>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/system/system_error.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::nats::service {

/**
 * @brief Retries an async operation with exponential backoff.
 *
 * @param fn Callable returning boost::asio::awaitable<void>. Must throw
 *           std::exception on failure.
 * @param operation_name Human-readable name logged on each retry.
 * @param initial_delay Delay before the first retry.
 * @param max_delay Upper bound on the per-retry delay.
 * @param max_retries Maximum number of retries (-1 = retry indefinitely).
 *
 * Logs each failure at WARN level and waits before the next attempt.
 * Throws the last exception if max_retries is exhausted.
 */
template<typename Fn>
boost::asio::awaitable<void>
retry_with_backoff(Fn&& fn,
    std::string_view operation_name,
    std::chrono::milliseconds initial_delay = std::chrono::seconds(1),
    std::chrono::milliseconds max_delay     = std::chrono::seconds(30),
    int max_retries = -1) {

    using namespace ores::logging;
    static const std::string_view logger_name = "ores.nats.service.retry";
    static auto& lg = []() -> auto& {
        static auto instance = make_logger(logger_name);
        return instance;
    }();

    auto delay = initial_delay;
    for (int attempt = 0; ; ++attempt) {
        bool failed = false;
        try {
            co_await fn();
            co_return;
        } catch (const boost::system::system_error& e) {
            // Propagate cancellation (SIGTERM/SIGQUIT) immediately.
            // Treat all other system errors (e.g. "No responders available")
            // as transient and subject to the normal retry/backoff logic.
            if (e.code() == boost::asio::error::operation_aborted)
                throw;
            if (max_retries >= 0 && attempt >= max_retries)
                throw;
            BOOST_LOG_SEV(lg, warn)
                << operation_name << " failed (attempt " << (attempt + 1)
                << "): " << e.what()
                << ". Retrying in " << delay.count() << " ms.";
            failed = true;
        } catch (const std::exception& e) {
            if (max_retries >= 0 && attempt >= max_retries)
                throw;

            BOOST_LOG_SEV(lg, warn)
                << operation_name << " failed (attempt " << (attempt + 1)
                << "): " << e.what()
                << ". Retrying in " << delay.count() << " ms.";
            failed = true;
        }

        // co_await is not allowed inside a catch handler — do the sleep here.
        if (failed) {
            const auto ex = co_await boost::asio::this_coro::executor;
            boost::asio::steady_timer timer(ex, delay);
            co_await timer.async_wait(boost::asio::use_awaitable);
            delay = std::min(delay * 2, max_delay);
        }
    }
}

} // namespace ores::nats::service

#endif
