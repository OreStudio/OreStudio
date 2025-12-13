/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.database/service/health_monitor.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/bind_cancellation_slot.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.database/domain/database_options.hpp"

namespace ores::database {

namespace {
    using severity_level = boost::log::trivial::severity_level;
    constexpr auto info = severity_level::info;
    constexpr auto debug = severity_level::debug;
    constexpr auto warn = severity_level::warning;
    constexpr auto error = severity_level::error;
}

health_monitor::health_monitor(database_options options,
    std::chrono::seconds poll_interval)
    : options_(std::move(options)),
      poll_interval_(poll_interval) {
}

void health_monitor::set_status_change_callback(status_change_callback callback) {
    std::lock_guard lock(mutex_);
    status_callback_ = std::move(callback);
}

bool health_monitor::is_available() const {
    return available_.load();
}

std::string health_monitor::last_error() const {
    std::lock_guard lock(mutex_);
    return last_error_;
}

void health_monitor::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping database health monitor";
    running_ = false;
    stop_signal_.emit(boost::asio::cancellation_type::all);
}

bool health_monitor::check_health() {
    const bool was_available = available_.load();

    try {
        BOOST_LOG_SEV(lg(), debug) << "Checking database connectivity...";

        auto credentials = to_credentials(options_);
        auto conn_result = sqlgen::postgres::connect(credentials);

        if (conn_result) {
            // Connection successful
            available_ = true;
            {
                std::lock_guard lock(mutex_);
                last_error_.clear();
            }

            if (!was_available) {
                BOOST_LOG_SEV(lg(), info)
                    << "Database connection restored - database is now available";

                status_change_callback callback;
                {
                    std::lock_guard lock(mutex_);
                    callback = status_callback_;
                }
                if (callback) {
                    callback(true, "");
                }
            } else {
                BOOST_LOG_SEV(lg(), debug) << "Database connectivity check passed";
            }
            return true;
        } else {
            // Connection failed
            std::string error_msg = "Failed to connect to database";
            available_ = false;
            {
                std::lock_guard lock(mutex_);
                last_error_ = error_msg;
            }

            if (was_available) {
                BOOST_LOG_SEV(lg(), error)
                    << "Database connection lost: " << error_msg;

                status_change_callback callback;
                {
                    std::lock_guard lock(mutex_);
                    callback = status_callback_;
                }
                if (callback) {
                    callback(false, error_msg);
                }
            } else {
                BOOST_LOG_SEV(lg(), warn)
                    << "Database still unavailable: " << error_msg;
            }
            return false;
        }
    } catch (const std::exception& e) {
        std::string error_msg = e.what();
        available_ = false;
        {
            std::lock_guard lock(mutex_);
            last_error_ = error_msg;
        }

        if (was_available) {
            BOOST_LOG_SEV(lg(), error)
                << "Database connection lost: " << error_msg;

            status_change_callback callback;
            {
                std::lock_guard lock(mutex_);
                callback = status_callback_;
            }
            if (callback) {
                callback(false, error_msg);
            }
        } else {
            BOOST_LOG_SEV(lg(), warn)
                << "Database still unavailable: " << error_msg;
        }
        return false;
    }
}

boost::asio::awaitable<void>
health_monitor::run(boost::asio::io_context& io_context) {
    BOOST_LOG_SEV(lg(), info)
        << "Starting database health monitor with poll interval of "
        << poll_interval_.count() << " seconds";

    running_ = true;

    // Perform initial health check
    BOOST_LOG_SEV(lg(), info) << "Performing initial database connectivity check...";
    const bool initial_available = check_health();

    if (initial_available) {
        BOOST_LOG_SEV(lg(), info) << "Initial database connectivity check PASSED";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Initial database connectivity check FAILED";
        BOOST_LOG_SEV(lg(), warn) << "Database is currently unavailable - clients will be notified";
    }

    // Continue polling
    boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);

    while (running_) {
        try {
            timer.expires_after(poll_interval_);
            co_await timer.async_wait(
                boost::asio::bind_cancellation_slot(
                    stop_signal_.slot(),
                    boost::asio::use_awaitable));

            if (!running_) {
                break;
            }

            check_health();

        } catch (const boost::system::system_error& e) {
            if (e.code() == boost::asio::error::operation_aborted) {
                BOOST_LOG_SEV(lg(), debug) << "Health monitor timer cancelled";
                break;
            }
            BOOST_LOG_SEV(lg(), error)
                << "Health monitor error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Database health monitor stopped";
}

}
