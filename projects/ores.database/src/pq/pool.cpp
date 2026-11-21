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
#include "ores.database/pq/connection.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.logging/logger.hpp"
#include <algorithm>
#include <system_error>
#include <thread>

namespace ores::database::pq {

namespace {

auto& pool_lg() {
    static auto instance = logging::make_logger("ores.database.pq.pool");
    return instance;
}

pool_settings normalised(const pool_settings& settings) {
    pool_settings r = settings;
    r.floor = std::max<std::size_t>(r.floor, 1);
    r.ceiling = std::max(r.ceiling, r.floor);
    // A period of nothing would make every idle connection due on every pass,
    // which is a reaper that spins rather than one that waits.
    r.idle_after = std::max(r.idle_after, std::chrono::milliseconds(1));
    return r;
}

}

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

pool::pool(connection_options options,
           pool_settings settings,
           open_policy policy,
           std::chrono::milliseconds acquire_timeout)
    : options_(std::move(options))
    , settings_(normalised(settings))
    , acquire_timeout_(acquire_timeout) {
    using namespace ores::logging;
    const auto attempts = std::max<std::size_t>(policy.attempts, 1);
    for (std::size_t attempt = 1;; ++attempt) {
        try {
            open_floor();
            start_reaper();
            return;
        } catch (const store_error& e) {
            idle_.clear();
            open_ = 0;
            if (!e.retryable() || attempt >= attempts)
                throw;
            const auto wait = policy.shape == backoff::linear ?
                                  policy.wait * attempt :
                                  policy.wait * (std::size_t{1} << (attempt - 1));
            ORES_LOG_SEV(pool_lg(), warn)
                << "Opening the database pool failed (attempt " << attempt << " of " << attempts
                << "), retrying in " << wait.count() << "s: " << e.what();
            std::this_thread::sleep_for(wait);
        }
    }
}

pool::pool(connection_options options,
           std::size_t size,
           open_policy policy,
           std::chrono::milliseconds acquire_timeout)
    : pool(std::move(options),
           pool_settings{.floor = size, .ceiling = size},
           policy,
           acquire_timeout) {}

pool::~pool() {
    stop_reaper();
}

void pool::open_floor() {
    idle_.reserve(settings_.floor);
    for (std::size_t i = 0; i < settings_.floor; ++i) {
        try {
            idle_.push_back(
                {std::make_unique<connection>(options_), std::chrono::steady_clock::now()});
            ++open_;
        } catch (const store_error& e) {
            throw store_error("Connection " + std::to_string(i + 1) + " of " +
                                  std::to_string(settings_.floor) + ": " + e.what(),
                              e.sqlstate(),
                              e.kind());
        }
    }
}

pool::lease pool::acquire() {
    std::unique_ptr<connection> c;
    bool opened_here = false;
    {
        std::unique_lock lock(mutex_);
        const auto deadline = std::chrono::steady_clock::now() + acquire_timeout_;
        if (!available_.wait_until(
                lock, deadline, [this] { return !idle_.empty() || open_ < settings_.ceiling; }))
            throw store_error("No connection came free within the acquire timeout",
                              "53300",
                              error_kind::retryable);
        if (!idle_.empty()) {
            c = std::move(idle_.back().connection);
            idle_.pop_back();
        } else {
            ++open_;
            opened_here = true;
        }
    }

    if (opened_here) {
        // The wait above ends before a login begins, so one slow connection does
        // not hold the pool for every other caller.
        try {
            c = std::make_unique<connection>(options_);
        } catch (...) {
            release_slot();
            throw;
        }
    }

    lease l(*this, std::move(c));
    if (!l.get().healthy() || !l.get().idle())
        l.get().reset();
    return l;
}

std::size_t pool::size() const {
    std::scoped_lock lock(mutex_);
    return open_;
}

std::size_t pool::idle() const {
    std::scoped_lock lock(mutex_);
    return idle_.size();
}

void pool::give_back(std::unique_ptr<connection> c) noexcept {
    bool above_floor = false;
    {
        std::scoped_lock lock(mutex_);
        // The back is the hottest and the front the coldest, which is what lets a
        // lease take the back and the reaper close the front.
        idle_.push_back({std::move(c), std::chrono::steady_clock::now()});
        above_floor = idle_.size() > settings_.floor;
    }
    available_.notify_one();
    if (above_floor)
        reaper_wakeup_.notify_one();
}

void pool::release_slot() noexcept {
    {
        std::scoped_lock lock(mutex_);
        --open_;
    }
    available_.notify_one();
}

void pool::start_reaper() {
    // A pool that cannot grow has nothing to close.
    if (settings_.ceiling <= settings_.floor)
        return;
    try {
        reaper_ = std::thread([this] { reap(); });
    } catch (const std::system_error& e) {
        // A pool that cannot start its reaper keeps its floor and its ceiling
        // and never shrinks. That is worse than the alternative, not fatal, so
        // it is logged rather than thrown past the caller's open policy.
        ORES_LOG_SEV(pool_lg(), logging::warn)
            << "The pool could not start its reaper, so connections above the floor "
               "will not be closed: "
            << e.what();
    }
}

void pool::stop_reaper() noexcept {
    {
        std::scoped_lock lock(mutex_);
        stopping_ = true;
    }
    reaper_wakeup_.notify_all();
    if (reaper_.joinable())
        reaper_.join();
}

void pool::reap() {
    std::unique_lock lock(mutex_);
    while (!stopping_) {
        if (idle_.size() <= settings_.floor) {
            reaper_wakeup_.wait_for(lock, settings_.idle_after);
            continue;
        }

        const auto due = idle_.front().since + settings_.idle_after;
        const auto now = std::chrono::steady_clock::now();
        if (due > now) {
            reaper_wakeup_.wait_until(lock, due);
            continue;
        }

        // The front is the coldest, so it is the one that has been idle longest.
        // Closed outside the lock: a teardown that blocks must not block a lease.
        std::unique_ptr<connection> dying = std::move(idle_.front().connection);
        idle_.erase(idle_.begin());
        lock.unlock();
        dying.reset();
        lock.lock();
        // The slot is given up only once the connection is closed, so a caller
        // cannot open one more while the old socket is still open.
        --open_;
        available_.notify_one();
    }
}

}
