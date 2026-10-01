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

#include <atomic>
#include <memory>
#include <thread>
#include <utility>

namespace ores::platform::concurrency {

/**
 * @brief The stop state a @c stoppable_thread shares with its callable.
 *
 * A faithful subset of @c std::stop_source: it answers @c request_stop() and
 * @c stop_requested(), and copies share the one flag. It exists because
 * Apple's libc++ has neither @c std::stop_source nor @c std::stop_token, which
 * the macOS build reports as:
 *
 *     error: no type named 'stop_token' in namespace 'std'
 *     error: no type named 'stop_source' in namespace 'std'
 */
class stop_source final {
public:
    stop_source()
        : stopped_(std::make_shared<std::atomic<bool>>(false)) {}

    // A moved-from source holds no state, and the thread it belonged to has
    // already been handed to whoever took the move. It stays inert rather than
    // dereferencing a null pointer, because a moved-from object is still
    // destroyed: the workflow engine assigns a stoppable_thread temporary and
    // the temporary's destructor asks it to stop.
    void request_stop() const {
        if (stopped_)
            stopped_->store(true, std::memory_order_relaxed);
    }

    [[nodiscard]] bool stop_requested() const {
        return stopped_ && stopped_->load(std::memory_order_relaxed);
    }

private:
    std::shared_ptr<std::atomic<bool>> stopped_;
};

/**
 * @brief A thread that carries a stop state and joins when it is destroyed.
 *
 * The portable counterpart of @c std::jthread. Apple's libc++ implements none
 * of the C++20 stop machinery -- no @c jthread, no @c stop_token and no
 * @c stop_source -- and that has stopped the macOS build twice: a comms thread
 * was reverted to @c std::thread for it on 2026-03-12, and the workflow
 * engine's deadline watch hit it on 2026-09-30. It lives here, beside the
 * concurrency header's other stand-ins for a standard facility a supported
 * library does not implement, so the next author reaches for the portable type
 * instead of the standard one.
 *
 * It is deliberately not exported: every member is defined here, and an export
 * attribute on a header-only type makes each consumer import symbols no
 * translation unit of this library defines.
 */
class stoppable_thread final {
public:
    stoppable_thread() = default;

    /**
     * @brief Starts a thread that runs @p callable with the stop state.
     *
     * The callable is stored by value, as @c std::thread stores it, so a
     * temporary is safe to pass. It takes the @c stop_source by value and asks
     * it whether a stop was requested.
     */
    template <typename Callable>
    explicit stoppable_thread(Callable&& callable) {
        auto source = source_;
        thread_ = std::thread(
            [source, callable = std::forward<Callable>(callable)]() mutable { callable(source); });
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

    /// Asks the thread to stop. A thread that never looks at the flag still
    /// joins, because the destructor waits for it either way.
    void request_stop() {
        source_.request_stop();
    }

    [[nodiscard]] bool stop_requested() const {
        return source_.stop_requested();
    }

    [[nodiscard]] bool joinable() const {
        return thread_.joinable();
    }

    void join() {
        if (thread_.joinable())
            thread_.join();
    }

private:
    stop_source source_;
    std::thread thread_;
};

}

#endif
