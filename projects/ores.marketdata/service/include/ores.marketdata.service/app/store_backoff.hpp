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
#ifndef ORES_MARKETDATA_SERVICE_APP_STORE_BACKOFF_HPP
#define ORES_MARKETDATA_SERVICE_APP_STORE_BACKOFF_HPP

#include <algorithm>
#include <chrono>

namespace ores::marketdata::service::app {

/**
 * @brief How long the ingest waits before it tries a store that did not answer.
 *
 * A store that is down answers every write with an error. Trying every batch would
 * make the worker wait for each failure, and the ticks behind it would age. After a
 * failure the loop leaves the store alone for a wait, and it doubles the wait at each
 * further failure up to a longest wait. One success ends the outage and resets the wait.
 *
 * Not thread safe: the caller holds its own lock.
 *
 * @see The tick ingest pipeline (doc/knowledge/architecture/tick_ingest_pipeline.org)
 */
class store_backoff final {
public:
    using clock = std::chrono::steady_clock;

    explicit store_backoff(std::chrono::milliseconds first_wait = std::chrono::seconds(1),
                           std::chrono::milliseconds longest_wait = std::chrono::seconds(30))
        : first_wait_(first_wait)
        , longest_wait_(longest_wait)
        , wait_(first_wait) {}

    /// Whether the loop may try the store at @p now: it is up, or its wait has ended.
    [[nodiscard]] bool may_try(clock::time_point now) const {
        return !down_ || now >= retry_at_;
    }

    /// Records a failure at @p now. Returns true when it starts an outage.
    bool note_failure(clock::time_point now) {
        const bool starts = !down_;
        down_ = true;
        retry_at_ = now + wait_;
        wait_ = std::min(wait_ * 2, longest_wait_);
        return starts;
    }

    /// Records a success. Returns true when it ends an outage.
    bool note_success() {
        const bool ends = down_;
        down_ = false;
        wait_ = first_wait_;
        return ends;
    }

    [[nodiscard]] bool down() const {
        return down_;
    }

private:
    std::chrono::milliseconds first_wait_;
    std::chrono::milliseconds longest_wait_;
    std::chrono::milliseconds wait_;
    clock::time_point retry_at_{};
    bool down_ = false;
};

}

#endif
