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
#ifndef ORES_UTILITY_REPOSITORY_CONTEXT_HPP
#define ORES_UTILITY_REPOSITORY_CONTEXT_HPP

#include <sqlgen/postgres.hpp>

namespace ores::utility::repository {

/**
 * @brief Context for the operations on a repository.
 */
class context {
public:
    using connection_type = sqlgen::postgres::Connection;
    using connection_pool_type = sqlgen::Result<
        sqlgen::ConnectionPool<connection_type>>;
    using single_connection_type = sqlgen::Result<
        rfl::Ref<connection_type>>;

    explicit context(connection_pool_type connection_pool,
                     single_connection_type single_connection)
        : connection_pool_(std::move(connection_pool)),
          single_connection_(std::move(single_connection)) {}

    connection_pool_type connection_pool() { return connection_pool_; }
    single_connection_type single_connection() { return single_connection_; }

    /**
     * @brief Set the bootstrap mode flag.
     *
     * Should be called during application initialization after checking bootstrap state.
     */
    void set_bootstrap_mode(bool mode) { bootstrap_mode_ = mode; }

    /**
     * @brief Check if the system is in bootstrap mode.
     *
     * @return true if system is in bootstrap mode (no admin accounts exist), false otherwise
     */
    bool is_in_bootstrap_mode() const { return bootstrap_mode_; }

private:
    connection_pool_type connection_pool_;
    single_connection_type single_connection_;
    bool bootstrap_mode_ = false;
};

}

#endif
