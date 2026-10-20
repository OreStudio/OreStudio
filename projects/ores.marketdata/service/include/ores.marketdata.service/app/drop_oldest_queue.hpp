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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_SERVICE_APP_DROP_OLDEST_QUEUE_HPP
#define ORES_MARKETDATA_SERVICE_APP_DROP_OLDEST_QUEUE_HPP

#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <mutex>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief A bounded queue between a delivery thread and one worker. A full
 * queue drops its oldest entry.
 *
 * A tick is perishable: an old tick that waits while a newer one arrives is
 * worse than a tick that is lost. So a push never waits, and when the queue is
 * full it makes room by dropping the entry that has waited longest. The worker
 * takes everything that has arrived in one call, so a batch grows as the worker
 * falls behind and the cost of a write is shared by more ticks.
 *
 * @see The tick ingest pipeline (doc/knowledge/architecture/tick_ingest_pipeline.org)
 */
template <typename T>
class drop_oldest_queue final {
public:
    explicit drop_oldest_queue(std::size_t capacity)
        : capacity_(capacity) {}

    drop_oldest_queue(const drop_oldest_queue&) = delete;
    drop_oldest_queue& operator=(const drop_oldest_queue&) = delete;

    /// Adds @p value and wakes the worker. Drops the oldest entry when full.
    void push(T value) {
        {
            std::lock_guard lock(mu_);
            if (items_.size() >= capacity_) {
                items_.pop_front();
                ++dropped_;
            }
            items_.push_back(std::move(value));
        }
        ready_.notify_one();
    }

    /**
     * @brief Moves up to @p max entries, oldest first, into @p out.
     *
     * Waits up to @p wait for the first entry. Returns how many entries it
     * took, which is zero when the wait ended with nothing to take or the queue
     * was closed.
     */
    std::size_t take_all(std::vector<T>& out, std::size_t max, std::chrono::milliseconds wait) {
        std::unique_lock lock(mu_);
        ready_.wait_for(lock, wait, [this] { return closed_ || !items_.empty(); });
        std::size_t taken = 0;
        while (taken < max && !items_.empty()) {
            out.push_back(std::move(items_.front()));
            items_.pop_front();
            ++taken;
        }
        return taken;
    }

    /// Wakes a waiting worker, and makes every later wait return at once.
    void close() {
        {
            std::lock_guard lock(mu_);
            closed_ = true;
        }
        ready_.notify_all();
    }

    [[nodiscard]] std::size_t size() const {
        std::lock_guard lock(mu_);
        return items_.size();
    }

    /// The entries dropped because the queue was full, since construction.
    [[nodiscard]] std::uint64_t dropped() const {
        std::lock_guard lock(mu_);
        return dropped_;
    }

private:
    const std::size_t capacity_;
    mutable std::mutex mu_;
    std::condition_variable ready_;
    std::deque<T> items_;
    std::uint64_t dropped_ = 0;
    bool closed_ = false;
};

}

#endif
