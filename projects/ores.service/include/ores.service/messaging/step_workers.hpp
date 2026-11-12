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
#ifndef ORES_SERVICE_MESSAGING_STEP_WORKERS_HPP
#define ORES_SERVICE_MESSAGING_STEP_WORKERS_HPP

#include <boost/asio/post.hpp>
#include <boost/asio/thread_pool.hpp>
#include <condition_variable>
#include <cstddef>
#include <functional>
#include <mutex>
#include <utility>

namespace ores::service::messaging {

/**
 * @brief A fixed number of threads on which a service runs its workflow steps.
 *
 * A step command arrives on the delivery thread of its subscription, and a step
 * run on that thread holds back every command behind it: a step that orchestrates
 * a long piece of work, such as generating a tenant's trades, keeps the steps
 * dispatched beside it waiting until the workflow gives them up as unclaimed.
 *
 * run() hands a step to a free worker, and when every worker is busy it waits for
 * one rather than queueing it. The waiting step's caller holds its delivery and
 * keeps it alive, and the commands behind it stay with the server. With one worker
 * a step runs on the calling thread.
 *
 * The destructor waits for every running step, so no step outlives the
 * subscription that took it, nor the handlers it calls.
 */
class step_workers final {
public:
    explicit step_workers(std::size_t workers)
        : workers_(workers == 0 ? 1 : workers)
        , free_(workers_)
        , pool_(workers_ > 1 ? workers_ : 0) {}

    ~step_workers() {
        pool_.join();
    }

    step_workers(const step_workers&) = delete;
    step_workers& operator=(const step_workers&) = delete;

    /// The number of steps that can run at once.
    [[nodiscard]] std::size_t size() const noexcept {
        return workers_;
    }

    /**
     * @brief Runs @p step on a free worker, waiting for one when all are busy.
     *
     * The step must not throw: it runs on a worker, where nothing could answer
     * the exception.
     */
    void run(std::move_only_function<void()> step) {
        if (workers_ == 1) {
            step();
            return;
        }
        {
            std::unique_lock lock(mutex_);
            freed_.wait(lock, [this] { return free_ > 0; });
            --free_;
        }
        try {
            boost::asio::post(pool_, [this, step = std::move(step)]() mutable noexcept {
                step();
                step = nullptr;
                release();
            });
        } catch (...) {
            release();
            throw;
        }
    }

private:
    void release() noexcept {
        std::scoped_lock lock(mutex_);
        ++free_;
        freed_.notify_one();
    }

    std::size_t workers_;
    std::mutex mutex_;
    std::condition_variable freed_;
    std::size_t free_;
    boost::asio::thread_pool pool_;
};

}

#endif
