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

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

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

    explicit context(connection_pool_type connection_pool)
        : connection_pool_(std::move(connection_pool)) {}

    connection_pool_type connection_pool() { return connection_pool_; }

private:
    connection_pool_type connection_pool_;
};

}

#endif
