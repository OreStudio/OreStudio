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
#ifndef ORES_DATABASE_CONNECTION_HPP
#define ORES_DATABASE_CONNECTION_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/options.hpp"
#include <cstddef>
#include <cstdint>
#include <memory>
#include <mutex>
#include <string>

namespace ores::database {

namespace pq {
class pool;
}

/**
 * @brief The libpq pool of one database: where it is, how to log in, and how
 * hard to try.
 *
 * This is one half of what a context carries. It changes when the deployment
 * changes, which is rarely, and every context of a process shares one. The
 * other half is who a request acts for, which changes per request and lives in
 * the context itself.
 *
 * The pool opens on first use, under the open policy, so a process that never
 * reaches its database never opens a connection.
 */
class ORES_DATABASE_EXPORT connection final {
public:
    explicit connection(pq::connection_options options, pq::open_policy policy = {});

    ~connection();

    connection(const connection&) = delete;
    connection& operator=(const connection&) = delete;

    /**
     * @brief The options a connection of this pool opens with.
     */
    [[nodiscard]] const pq::connection_options& options() const noexcept {
        return options_;
    }

    /**
     * @brief The pool, opened on first use under the open policy.
     */
    pq::pool& pool() const;

    /**
     * @brief The connections the pool keeps and the ones a burst may reach,
     * when it opens.
     */
    void set_pool_settings(pq::pool_settings settings);

    /**
     * @brief The endpoint the notification listener connects to, when it is not
     * the one this service uses.
     *
     * A transaction pooler hands a LISTEN to somebody else at the next commit,
     * so an environment that pools its traffic names PostgreSQL's port here and
     * leaves this service's port at the pooler's. An empty host and a zero port
     * mean the listener uses the service's own endpoint.
     */
    void set_listener_endpoint(std::string host, std::uint16_t port);

    /**
     * @brief The options a listener's connection opens with.
     */
    [[nodiscard]] pq::connection_options listener_options() const;

private:
    pq::connection_options options_;
    pq::open_policy policy_;
    mutable std::mutex mutex_;
    mutable std::unique_ptr<pq::pool> pool_;
    pq::pool_settings settings_;
    std::string listener_host_;
    std::uint16_t listener_port_ = 0;
};

}

#endif
