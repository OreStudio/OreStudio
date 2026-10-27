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
#include "ores.database/pq/pool.hpp"
#include "ores.database/pq/store_error.hpp"

namespace ores::database::pq {

pool::lease::lease(pool& owner, std::unique_ptr<connection> c) noexcept
    : owner_(&owner)
    , connection_(std::move(c)) {}

pool::lease::lease(lease&& other) noexcept
    : owner_(other.owner_)
    , connection_(std::move(other.connection_)) {}

pool::lease::~lease() {
    if (connection_)
        owner_->give_back(std::move(connection_));
}

pool::pool(connection_options options, std::size_t size, std::chrono::milliseconds acquire_timeout)
    : options_(std::move(options))
    , size_(size)
    , acquire_timeout_(acquire_timeout) {
    idle_.reserve(size_);
    for (std::size_t i = 0; i < size_; ++i)
        idle_.push_back(std::make_unique<connection>(options_));
}

pool::lease pool::acquire() {
    std::unique_ptr<connection> c;
    {
        std::unique_lock lock(mutex_);
        if (!available_.wait_for(lock, acquire_timeout_, [this] { return !idle_.empty(); }))
            throw store_error("No connection came free within the acquire timeout",
                              "53300",
                              error_kind::retryable);
        c = std::move(idle_.back());
        idle_.pop_back();
    }
    lease l(*this, std::move(c));
    if (!l.get().healthy())
        l.get().reset();
    return l;
}

std::size_t pool::idle() const {
    std::scoped_lock lock(mutex_);
    return idle_.size();
}

void pool::give_back(std::unique_ptr<connection> c) noexcept {
    {
        std::scoped_lock lock(mutex_);
        idle_.push_back(std::move(c));
    }
    available_.notify_one();
}

}
