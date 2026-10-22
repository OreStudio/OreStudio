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
#ifndef ORES_MARKETDATA_SERVICE_APP_BATCH_HANDOFF_HPP
#define ORES_MARKETDATA_SERVICE_APP_BATCH_HANDOFF_HPP

#include <condition_variable>
#include <cstddef>
#include <deque>
#include <mutex>
#include <optional>
#include <utility>

namespace ores::marketdata::service::app {

/**
 * @brief A bounded queue that hands the batches of one stage to the next.
 *
 * The ingest prepares a batch in one thread and copies it in another, so the copy of one batch
 * runs while the next is prepared. A push waits while the queue is full, so a slow copy stops the
 * preparation and the batches do not pile up in memory. This is the opposite of the queue of
 * ticks, which never makes the delivery thread wait: a tick is perishable, and a batch that is
 * prepared is not.
 *
 * A close ends the wait of both sides. The batches that are queued at that time stay, so the
 * copy stage finishes them before it stops.
 *
 * @see The tick ingest pipeline (doc/knowledge/architecture/tick_ingest_pipeline.org)
 */
template <typename T>
class batch_handoff final {
public:
    explicit batch_handoff(std::size_t capacity)
        : capacity_(capacity) {}

    batch_handoff(const batch_handoff&) = delete;
    batch_handoff& operator=(const batch_handoff&) = delete;

    /// Adds @p value, waiting while the queue is full. Returns false, and drops @p value, when
    /// the queue is closed.
    bool push(T value) {
        std::unique_lock lock(mu_);
        not_full_.wait(lock, [this] { return closed_ || items_.size() < capacity_; });
        if (closed_)
            return false;
        items_.push_back(std::move(value));
        lock.unlock();
        not_empty_.notify_one();
        return true;
    }

    /// Takes the oldest batch, waiting for one. Returns nothing when the queue is closed and
    /// empty.
    std::optional<T> pop() {
        std::unique_lock lock(mu_);
        not_empty_.wait(lock, [this] { return closed_ || !items_.empty(); });
        if (items_.empty())
            return std::nullopt;
        T value = std::move(items_.front());
        items_.pop_front();
        lock.unlock();
        not_full_.notify_one();
        return value;
    }

    /// Ends every wait. Later pushes fail, and later pops return what is queued and then nothing.
    void close() {
        {
            std::lock_guard lock(mu_);
            closed_ = true;
        }
        not_full_.notify_all();
        not_empty_.notify_all();
    }

private:
    const std::size_t capacity_;
    mutable std::mutex mu_;
    std::condition_variable not_full_;
    std::condition_variable not_empty_;
    std::deque<T> items_;
    bool closed_ = false;
};

}

#endif
