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
#include "ores.database/domain/connection.hpp"
#include "ores.database/pq/pool.hpp"

namespace ores::database {

connection::connection(pq::connection_options options, pq::open_policy policy)
    : options_(std::move(options))
    , policy_(std::move(policy)) {}

connection::~connection() = default;

pq::pool& connection::pool() const {
    std::scoped_lock lock(mutex_);
    if (!pool_)
        pool_ = std::make_unique<pq::pool>(options_, size_, policy_);
    return *pool_;
}

void connection::set_pool_size(std::size_t size) {
    std::scoped_lock lock(mutex_);
    size_ = size;
}

}
