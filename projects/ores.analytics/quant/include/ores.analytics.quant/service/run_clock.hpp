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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RUN_CLOCK_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RUN_CLOCK_HPP

#include <chrono>
#include <condition_variable>
#include <mutex>

namespace ores::analytics::quant::service {

/**
 * @brief The one source of time for a run: what time it is, and a wait for one.
 *
 * Everything in the arrival path reads and waits on time through this, so a test
 * can swap in a manual clock and assert exact event times without sleeping.
 */
class i_run_clock {
public:
    using time_point = std::chrono::steady_clock::time_point;

    /// Why a wait returned.
    enum class wake { deadline, interrupted };

    virtual ~i_run_clock() = default;

    [[nodiscard]] virtual time_point now() const = 0;

    /// The wall time of a point on this clock, for stamping what happened then.
    [[nodiscard]] virtual std::chrono::system_clock::time_point wall(time_point t) const = 0;

    /**
     * @brief Block until now() reaches the deadline, or until interrupt().
     *
     * An interrupt sent while no one waits is kept, and the next wait returns at
     * once, so a wake sent just before a wait starts is not lost.
     */
    virtual wake wait_until(time_point deadline) = 0;

    /// Wake the waiter now, or the next one if none waits.
    virtual void interrupt() = 0;
};

/**
 * @brief The production clock: the steady clock, mapped to wall time once.
 *
 * Wall time is taken at construction and carried forward by the steady clock,
 * so a stamp never jumps when the system clock is adjusted.
 */
class steady_run_clock final : public i_run_clock {
public:
    steady_run_clock()
        : origin_(std::chrono::steady_clock::now())
        , wall_origin_(std::chrono::system_clock::now()) {}

    [[nodiscard]] time_point now() const override {
        return std::chrono::steady_clock::now();
    }

    [[nodiscard]] std::chrono::system_clock::time_point wall(time_point t) const override {
        return wall_origin_ +
               std::chrono::duration_cast<std::chrono::system_clock::duration>(t - origin_);
    }

    wake wait_until(time_point deadline) override {
        std::unique_lock lock(mutex_);
        if (!woken_.wait_until(lock, deadline, [this] { return interrupted_; }))
            return wake::deadline;
        interrupted_ = false;
        return wake::interrupted;
    }

    void interrupt() override {
        {
            const auto guard = std::lock_guard(mutex_);
            interrupted_ = true;
        }
        woken_.notify_all();
    }

private:
    time_point origin_;
    std::chrono::system_clock::time_point wall_origin_;
    std::mutex mutex_;
    std::condition_variable woken_;
    bool interrupted_ = false;
};

/**
 * @brief A clock that moves only when a test moves it.
 *
 * Starts at the steady clock's epoch, which maps to the wall time it is given.
 * A waiter wakes when advance() takes now() to or past its deadline.
 */
class manual_run_clock final : public i_run_clock {
public:
    explicit manual_run_clock(std::chrono::system_clock::time_point wall_origin)
        : wall_origin_(wall_origin) {}

    [[nodiscard]] time_point now() const override {
        const auto guard = std::lock_guard(mutex_);
        return now_;
    }

    [[nodiscard]] std::chrono::system_clock::time_point wall(time_point t) const override {
        return wall_origin_ + std::chrono::duration_cast<std::chrono::system_clock::duration>(
                                  t.time_since_epoch());
    }

    wake wait_until(time_point deadline) override {
        std::unique_lock lock(mutex_);
        ++waiters_;
        waiter_arrived_.notify_all();
        woken_.wait(lock, [&] { return interrupted_ || now_ >= deadline; });
        --waiters_;
        if (interrupted_) {
            interrupted_ = false;
            return wake::interrupted;
        }
        return wake::deadline;
    }

    void interrupt() override {
        {
            const auto guard = std::lock_guard(mutex_);
            interrupted_ = true;
        }
        woken_.notify_all();
    }

    /// Move time forward and wake every waiter whose deadline it reaches.
    void advance(std::chrono::steady_clock::duration by) {
        {
            const auto guard = std::lock_guard(mutex_);
            now_ += by;
        }
        woken_.notify_all();
    }

    /// Block until a thread is waiting on this clock, so a test can advance past it.
    void await_waiter() {
        std::unique_lock lock(mutex_);
        waiter_arrived_.wait(lock, [this] { return waiters_ > 0; });
    }

private:
    std::chrono::system_clock::time_point wall_origin_;
    mutable std::mutex mutex_;
    std::condition_variable woken_;
    std::condition_variable waiter_arrived_;
    time_point now_{};
    int waiters_ = 0;
    bool interrupted_ = false;
};

}

#endif
