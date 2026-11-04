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
#ifndef ORES_HTTP_API_NET_WORKER_POOL_HPP
#define ORES_HTTP_API_NET_WORKER_POOL_HPP

#include "ores.http.api/export.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/thread_pool.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <atomic>
#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

namespace ores::http::net {

/**
 * @brief A bounded pool of threads on which a handler may block.
 *
 * A route handler forwards to a service over NATS and the request call is
 * synchronous, so running it on the connection's own thread would stall every
 * other connection for the length of one downstream answer. The pool moves the
 * blocking part onto a worker and hands the result back to the connection's
 * executor, so the I/O thread only ever runs completions.
 *
 * The pool is bounded twice over. The thread count is fixed at construction, and
 * the number of requests the pool will accept at once is capped, so a slow
 * service costs latency and a refusal rather than unbounded memory and threads.
 * When the cap is reached @c saturated() reports true and the caller refuses the
 * request with 503 instead of queueing it.
 *
 * Handlers run on pool threads concurrently, so a handler must not touch the
 * connection: it receives the request by const reference and returns a whole
 * response, which is exactly the route contract.
 *
 * @see doc/plans/2026-10-28-single-http-server-design.org, increment A1.
 */
class ORES_HTTP_API_EXPORT worker_pool final {
public:
    /**
     * @param threads      Worker threads. Zero selects a default of eight.
     * @param max_inflight Requests the pool accepts at once. Zero means no cap,
     *                     which is only appropriate for a test.
     */
    explicit worker_pool(std::size_t threads, std::size_t max_inflight = 1024)
        : pool_(threads == 0 ? 8 : threads)
        , max_inflight_(max_inflight) {}

    ~worker_pool() {
        pool_.join();
    }

    worker_pool(const worker_pool&) = delete;
    worker_pool& operator=(const worker_pool&) = delete;

    /// Whether the pool is at its in-flight cap.
    [[nodiscard]] bool saturated() const noexcept {
        return max_inflight_ != 0 && inflight_.load(std::memory_order_relaxed) >= max_inflight_;
    }

    /// The requests in flight or queued on the pool.
    [[nodiscard]] std::size_t inflight() const noexcept {
        return inflight_.load(std::memory_order_relaxed);
    }

    /**
     * @brief Runs an awaitable on the pool and returns its result.
     *
     * The caller stays suspended on its own executor, so the connection is not
     * pinned to a worker while the handler blocks. The slot is released when the
     * awaitable finishes, cancellation included.
     *
     * @p invoke must be copyable, because it is held as a std::function.
     */
    template <typename Invoke>
    boost::asio::awaitable<typename std::invoke_result_t<Invoke>::value_type> run(Invoke invoke) {
        using result_type = typename std::invoke_result_t<Invoke>::value_type;
        return run_erased<result_type>(
            std::function<boost::asio::awaitable<result_type>()>(std::move(invoke)));
    }

private:
    /*
     * A member type and not a local of run(), because run() is a coroutine and
     * its frame holds the guard: a local type would give the frame a subobject
     * with no linkage, which gcc refuses under -Wsubobject-linkage.
     */
    struct inflight_guard final {
        std::atomic<std::size_t>& counter;
        ~inflight_guard() {
            counter.fetch_sub(1, std::memory_order_relaxed);
        }
    };

    /*
     * The coroutine behind run(). Its frame holds the callable for as long as the
     * awaitable runs, because a lambda coroutine reads its captures through the
     * closure. The callable is held as a std::function and not as the caller's
     * type: a caller's lambda has no linkage, and a frame holding one is what gcc
     * refuses under -Wsubobject-linkage.
     */
    template <typename Result>
    boost::asio::awaitable<Result> run_erased(std::function<boost::asio::awaitable<Result>()> invoke) {
        inflight_.fetch_add(1, std::memory_order_relaxed);
        inflight_guard guard{inflight_};

        if constexpr (std::is_void_v<Result>) {
            co_await boost::asio::co_spawn(
                pool_.get_executor(), invoke(), boost::asio::use_awaitable);
        } else {
            auto value = co_await boost::asio::co_spawn(
                pool_.get_executor(), invoke(), boost::asio::use_awaitable);
            co_return value;
        }
    }

    boost::asio::thread_pool pool_;
    std::size_t max_inflight_;
    std::atomic<std::size_t> inflight_{0};
};

}

#endif
