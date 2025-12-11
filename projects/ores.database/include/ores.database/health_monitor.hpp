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
#ifndef ORES_DATABASE_HEALTH_MONITOR_HPP
#define ORES_DATABASE_HEALTH_MONITOR_HPP

#include <mutex>
#include <atomic>
#include <chrono>
#include <string>
#include <functional>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/cancellation_signal.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/sources/severity_channel_logger.hpp>
#include "ores.database/database_options.hpp"

namespace ores::database {

/**
 * @brief Monitors database connectivity and provides health status.
 *
 * This class periodically checks database connectivity and maintains the
 * current health status. It can notify listeners when the status changes,
 * allowing the server to inform clients of database availability issues.
 *
 * Usage:
 * @code
 *     health_monitor monitor(db_options, std::chrono::seconds(5));
 *
 *     // Set callback for status changes
 *     monitor.set_status_change_callback([](bool available) {
 *         if (!available) {
 *             notify_clients_database_unavailable();
 *         }
 *     });
 *
 *     // Start monitoring
 *     co_await monitor.run(io_context);
 * @endcode
 */
class health_monitor final {
private:
    using severity_level = boost::log::trivial::severity_level;
    using logger_type = boost::log::sources::severity_channel_logger_mt<
        severity_level, std::string>;

    [[nodiscard]] static logger_type& lg() {
        static logger_type instance(
            boost::log::keywords::channel = "ores.database.health_monitor");
        return instance;
    }

public:
    /**
     * @brief Callback type for status change notifications.
     *
     * The callback receives the new availability status (true = available,
     * false = unavailable) and an optional error message when unavailable.
     */
    using status_change_callback =
        std::function<void(bool available, const std::string& error_message)>;

    /**
     * @brief Construct a health monitor with database options.
     *
     * @param options Database connection options.
     * @param poll_interval Interval between health checks.
     */
    explicit health_monitor(database_options options,
        std::chrono::seconds poll_interval = std::chrono::seconds(5));

    ~health_monitor() = default;

    health_monitor(const health_monitor&) = delete;
    health_monitor& operator=(const health_monitor&) = delete;
    health_monitor(health_monitor&&) = delete;
    health_monitor& operator=(health_monitor&&) = delete;

    /**
     * @brief Set the callback to be invoked when database status changes.
     *
     * @param callback The callback function.
     */
    void set_status_change_callback(status_change_callback callback);

    /**
     * @brief Check if the database is currently available.
     *
     * @return true if the database is available, false otherwise.
     */
    [[nodiscard]] bool is_available() const;

    /**
     * @brief Get the last error message if database is unavailable.
     *
     * @return The error message, or empty string if database is available.
     */
    [[nodiscard]] std::string last_error() const;

    /**
     * @brief Run the health monitor polling loop.
     *
     * This performs an initial check and then polls at the configured
     * interval until stopped.
     *
     * @param io_context The io_context to run on.
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& io_context);

    /**
     * @brief Stop the health monitor.
     *
     * Cancels the polling loop.
     */
    void stop();

    /**
     * @brief Perform a single health check.
     *
     * Can be called manually to check database connectivity.
     *
     * @return true if database is available, false otherwise.
     */
    bool check_health();

private:
    database_options options_;
    std::chrono::seconds poll_interval_;
    std::atomic<bool> available_{false};
    std::atomic<bool> running_{false};
    mutable std::mutex mutex_;
    std::string last_error_;
    status_change_callback status_callback_;
    boost::asio::cancellation_signal stop_signal_;
};

}

#endif
