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
 * @brief A service's few long-lived connections.
 *
 * Every connection opens when the pool is made and lives as long as the pool,
 * so no query pays for a lookup, a login and a teardown. A lease takes one
 * connection and gives it back on every path. A connection that broke is
 * reconnected when it is next leased.
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
    pool(connection_options options,
         std::size_t size,
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
