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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_CONCURRENCY_STOPPABLE_THREAD_HPP
#define ORES_PLATFORM_CONCURRENCY_STOPPABLE_THREAD_HPP

#include <stop_token>
#include <thread>
#include <utility>

namespace ores::platform::concurrency {

/**
 * @brief A thread that carries a stop token and joins when it is destroyed.
 *
 * The part of @c std::jthread Apple's libc++ does not implement. It has
 * @c std::stop_token and @c std::stop_source, so the only thing missing is the
 * thread that owns them and joins in its destructor:
 *
 *     error: no type named 'jthread' in namespace 'std'; did you mean 'thread'?
 *
 * The gap has stopped the macOS build twice. A comms thread was reverted to
 * @c std::thread for it on 2026-03-12, and the workflow engine's deadline
 * watch reintroduced @c std::jthread on 2026-09-30 and stopped both macOS
 * legs. It lives here so the next thread that wants a stop token reaches for
 * the portable type rather than for the standard one, and so the portability
 * decision is made once instead of in every caller.
 *
 * The class is deliberately not exported: every member is defined here, and an
 * export attribute on a header-only type makes each consumer import symbols no
 * translation unit of this library defines.
 */
class stoppable_thread final {
public:
    stoppable_thread() = default;

    /**
     * @brief Starts a thread that runs @p callable with this thread's token.
     *
     * The callable is stored by value, as @c std::thread stores it, so a
     * temporary is safe to pass.
     */
    template <typename Callable>
    explicit stoppable_thread(Callable&& callable) {
        // The token is taken before the thread starts, and the callable is
        // stored by value, as std::thread would store it.
        auto token = source_.get_token();
        thread_ = std::thread(
            [token, callable = std::forward<Callable>(callable)]() mutable { callable(token); });
    }

    stoppable_thread(const stoppable_thread&) = delete;
    stoppable_thread& operator=(const stoppable_thread&) = delete;

    stoppable_thread(stoppable_thread&& other) noexcept
        : source_(std::move(other.source_))
        , thread_(std::move(other.thread_)) {}

    stoppable_thread& operator=(stoppable_thread&& other) noexcept {
        if (this != &other) {
            request_stop();
            join();
            source_ = std::move(other.source_);
            thread_ = std::move(other.thread_);
        }
        return *this;
    }

    ~stoppable_thread() {
        request_stop();
        join();
    }

    /// Asks the thread to stop; a thread that never looks at its token still
    /// joins, because the destructor waits for it either way.
    void request_stop() {
        source_.request_stop();
    }

    [[nodiscard]] bool stop_requested() const {
        return source_.stop_requested();
    }

    [[nodiscard]] std::stop_token get_stop_token() const {
        return source_.get_token();
    }

    [[nodiscard]] bool joinable() const {
        return thread_.joinable();
    }

    void join() {
        if (thread_.joinable())
            thread_.join();
    }

private:
    std::stop_source source_;
    std::thread thread_;
};

}

#endif
