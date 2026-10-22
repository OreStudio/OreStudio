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

#include <chrono>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <thread>
#include <utility>

namespace ores::platform::concurrency {

/**
 * @brief The stop state a @c stoppable_thread shares with its callable.
 *
 * A subset of @c std::stop_source that adds the one thing a waiting thread
 * needs, an interruptible wait. It answers @c request_stop(),
 * @c stop_requested() and @c wait_for(), and copies share the one state. It
 * exists because Apple's libc++ has neither @c std::stop_source nor
 * @c std::stop_token, which the macOS build reports as:
 *
 *     error: no type named 'stop_token' in namespace 'std'
 *     error: no type named 'stop_source' in namespace 'std'
 *
 * Every platform uses this type, including the ones whose library has the
 * standard one. A second implementation selected by a feature macro would be
 * code that only one platform runs, and this one gives the standard's behaviour
 * on all of them: a stop wakes a wait at once.
 */
class stop_source final {
public:
    stop_source()
        : state_(std::make_shared<state>()) {}

    // A moved-from source holds no state, and the thread it belonged to has
    // already been handed to whoever took the move. It stays inert rather than
    // dereferencing a null pointer, because a moved-from object is still
    // destroyed: the workflow engine assigns a stoppable_thread temporary and
    // the temporary's destructor asks it to stop.
    void request_stop() const {
        if (!state_)
            return;
        {
            const std::lock_guard lock(state_->mutex);
            state_->stopped = true;
        }
        state_->woken.notify_all();
    }

    [[nodiscard]] bool stop_requested() const {
        if (!state_)
            return true;
        const std::lock_guard lock(state_->mutex);
        return state_->stopped;
    }

    /**
     * @brief Waits for @p span, or until a stop is requested.
     *
     * Returns at once when a stop was requested before the call. The answer is
     * whether a stop was requested, so a loop that waits between passes reads
     * it as its exit test. A moved-from source answers true without waiting.
     */
    bool wait_for(std::chrono::milliseconds span) const {
        if (!state_)
            return true;
        std::unique_lock lock(state_->mutex);
        return state_->woken.wait_for(lock, span, [this] { return state_->stopped; });
    }

private:
    struct state {
        std::mutex mutex;
        std::condition_variable woken;
        bool stopped = false;
    };

    std::shared_ptr<state> state_;
};

/**
 * @brief A thread that carries a stop state and joins when it is destroyed.
 *
 * The portable counterpart of @c std::jthread. Apple's libc++ implements none
 * of the C++20 stop machinery -- no @c jthread, no @c stop_token and no
 * @c stop_source -- and that has stopped the macOS build three times: a comms
 * thread was reverted to @c std::thread for it on 2026-03-12, the workflow
 * engine's deadline watch hit it on 2026-09-30, and the feed restore retry hit
 * it on 2026-10-19. It lives here, beside the concurrency header's other
 * stand-ins for a standard facility a supported library does not implement, so
 * the next author reaches for the portable type instead of the standard one.
 * check_portable_stop_types.py fails on the standard types outside this
 * library.
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

    /// Asks the thread to stop and wakes a wait. A thread that never looks at
    /// the stop state still joins, because the destructor waits for it either
    /// way.
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
