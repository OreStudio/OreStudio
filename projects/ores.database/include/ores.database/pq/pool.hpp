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
#include "ores.database/pq/connection.hpp"
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <memory>
#include <mutex>
#include <vector>

namespace ores::database::pq {

/**
 * @brief The shape of the waits between attempts to open a pool.
 */
enum class backoff { linear, exponential };

/**
 * @brief How hard a pool tries to open its connections before it gives up.
 *
 * A database that is restarting refuses connections for a while; the attempts
 * absorb that instead of failing the first request after it. Only a failure
 * to connect is retried.
 */
struct open_policy final {
    /// Maximum number of attempts.
    std::size_t attempts = 10;
    /// Base wait between attempts.
    std::chrono::seconds wait{1};
    /// Wait shape between attempts.
    backoff shape = backoff::exponential;
};

/**
 * @brief A service's few long-lived connections.
 *
 * Every connection opens when the pool is made and lives as long as the pool,
 * so no query pays for a lookup, a login and a teardown. A lease takes one
 * connection and gives it back on every path. A connection that broke, or
 * that comes back still inside a transaction because a rollback or a copy
 * failed, is reset when it is next leased, so no caller inherits another's
 * transaction or settings.
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
     * @brief Opens @p size connections; a failure to open any is retryable.
     */
    /**
     * @brief Opens @p size connections, retrying a failure to connect under
     * @p policy.
     */
    pool(connection_options options,
         std::size_t size,
         open_policy policy = {},
         std::chrono::milliseconds acquire_timeout = std::chrono::seconds(5));

    pool(const pool&) = delete;
    pool& operator=(const pool&) = delete;

    /**
     * @brief Leases a connection, waiting at most the acquire timeout. A pool
     * that stays busy for that long is a retryable failure.
     */
    [[nodiscard]] lease acquire();

    [[nodiscard]] std::size_t size() const noexcept {
        return size_;
    }

    [[nodiscard]] std::size_t idle() const;

private:
    void open_all();
    void give_back(std::unique_ptr<connection> c) noexcept;

    connection_options options_;
    std::size_t size_;
    std::chrono::milliseconds acquire_timeout_;
    mutable std::mutex mutex_;
    std::condition_variable available_;
    std::vector<std::unique_ptr<connection>> idle_;
};

}

#endif
