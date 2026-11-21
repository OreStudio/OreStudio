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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_PQ_POOL_HPP
#define ORES_DATABASE_PQ_POOL_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/connection_fwd.hpp"
#include "ores.database/pq/options.hpp"
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

namespace ores::database::pq {

/**
 * @brief A service's few long-lived connections, elastic between a floor and a
 * ceiling.
 *
 * The floor opens when the pool is made and lives as long as the pool, so no
 * query pays for a lookup, a login and a teardown. A caller that finds nothing
 * idle opens one more when the ceiling allows it, and a connection above the
 * floor closes once it has been idle for the stated period, so an idle service
 * holds only its floor. A lease takes one connection and gives it back on every
 * path. A connection that broke, or that comes back still inside a transaction
 * because a rollback or a copy failed, is reset when it is next leased, so no
 * caller inherits another's transaction or settings.
 *
 * A lease must not outlive its pool.
 */
class ORES_DATABASE_EXPORT pool final {
public:
    /**
     * @brief Gives its connection back when it goes out of scope.
     */
    class ORES_DATABASE_EXPORT lease final {
    public:
        lease(pool& owner, std::unique_ptr<connection> c) noexcept;
        lease(lease&& other) noexcept;
        lease& operator=(lease&&) = delete;
        lease(const lease&) = delete;
        lease& operator=(const lease&) = delete;
        ~lease();

        [[nodiscard]] connection& get() const noexcept {
            return *connection_;
        }

    private:
        pool* owner_;
        std::unique_ptr<connection> connection_;
    };

    /**
     * @brief Opens the floor connections, retrying a failure to connect under
     * @p policy.
     */
    pool(connection_options options,
         pool_settings settings,
         open_policy policy = {},
         std::chrono::milliseconds acquire_timeout = std::chrono::seconds(5));

    /**
     * @brief A pool that keeps @p size connections and opens no more.
     */
    pool(connection_options options,
         std::size_t size,
         open_policy policy = {},
         std::chrono::milliseconds acquire_timeout = std::chrono::seconds(5));

    pool(const pool&) = delete;
    pool& operator=(const pool&) = delete;

    /**
     * @brief Closes the connections the pool still holds.
     *
     * Declared here and defined in the .cpp, where a connection is complete:
     * the header names the type without defining it.
     */
    ~pool();

    /**
     * @brief Leases a connection, opening one if nothing is idle and the ceiling
     * allows it, and waiting at most the acquire timeout. A pool that stays busy
     * for that long is a retryable failure.
     */
    [[nodiscard]] lease acquire();

    /**
     * @brief The connections the pool holds open now, idle and leased.
     */
    [[nodiscard]] std::size_t size() const;

    [[nodiscard]] std::size_t idle() const;

    [[nodiscard]] const pool_settings& settings() const noexcept {
        return settings_;
    }

private:
    struct idle_connection final {
        std::unique_ptr<connection> connection;
        std::chrono::steady_clock::time_point since;
    };

    void open_floor();
    void give_back(std::unique_ptr<connection> c) noexcept;
    void release_slot() noexcept;
    void reap();
    void start_reaper();
    void stop_reaper() noexcept;

    connection_options options_;
    pool_settings settings_;
    std::chrono::milliseconds acquire_timeout_;
    mutable std::mutex mutex_;
    std::condition_variable available_;
    std::condition_variable ticker_;
    std::vector<idle_connection> idle_;
    std::size_t open_ = 0;
    bool stopping_ = false;
    std::thread reaper_;
};

}

#endif
