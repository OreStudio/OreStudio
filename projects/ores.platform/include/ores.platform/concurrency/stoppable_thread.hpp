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
#include <type_traits>
#include <utility>
#include <version>

// Defining ORES_PLATFORM_PORTABLE_STOP selects the portable types on a library that has the
// standard ones, so a build can run the code that Apple's libc++ runs.
#if defined(__cpp_lib_jthread) && !defined(ORES_PLATFORM_PORTABLE_STOP)
#    define ORES_PLATFORM_STANDARD_STOP
#    include <stop_token>
#endif

/**
 * @file
 * @brief @c jthread, @c stop_token and @c stop_source on every platform.
 *
 * Where the standard library has the C++20 stop machinery, the names in
 * @c ores::platform::concurrency are the standard types. Apple's libc++ has
 * none of it, and the macOS build reports:
 *
 *     error: no type named 'jthread' in namespace 'std'
 *     error: no type named 'stop_token' in namespace 'std'
 *
 * That broke the build three times: a comms thread on 2026-03-12, the workflow
 * engine's deadline watch on 2026-09-30 and the feed restore retry on
 * 2026-10-19. There the names are the types of the @c portable namespace.
 * They are a subset of the standard ones with the same spelling, so a caller
 * is written once against the standard interface.
 *
 * The portable types are compiled on every platform, though only a library
 * without the standard ones selects them, so the unit tests run them on Linux.
 *
 * This header is meant to be deleted. When Apple's libc++ has the standard
 * types, replace @c ores::platform::concurrency:: with @c std:: at each use,
 * keep @c wait_for as a function of the caller's own, and remove this file.
 * check_portable_stop_types.py fails on the standard types outside this
 * library until then.
 */
namespace ores::platform::concurrency::portable {

/// The state a stop source shares with its tokens and with a waiting thread.
struct stop_state {
    std::mutex mutex;
    std::condition_variable woken;
    bool stopped = false;
};

/// A view of a stop source. A default constructed token has no state and is
/// never stopped, as the standard one.
class stop_token final {
public:
    stop_token() = default;

    [[nodiscard]] bool stop_requested() const {
        if (!state_)
            return false;
        const std::lock_guard lock(state_->mutex);
        return state_->stopped;
    }

    [[nodiscard]] bool stop_possible() const {
        return state_ != nullptr;
    }

private:
    friend class stop_source;
    friend bool wait_for(const stop_token&, std::chrono::milliseconds);

    explicit stop_token(std::shared_ptr<stop_state> state)
        : state_(std::move(state)) {}

    std::shared_ptr<stop_state> state_;
};

/// The owner of a stop state. Copies share the one state. A moved-from source
/// has no state, and its calls do nothing, as the standard one.
class stop_source final {
public:
    stop_source()
        : state_(std::make_shared<stop_state>()) {}

    [[nodiscard]] stop_token get_token() const {
        return stop_token(state_);
    }

    /// Returns whether this call made the request, and wakes every wait.
    bool request_stop() const {
        if (!state_)
            return false;
        {
            const std::lock_guard lock(state_->mutex);
            if (state_->stopped)
                return false;
            state_->stopped = true;
        }
        state_->woken.notify_all();
        return true;
    }

    [[nodiscard]] bool stop_requested() const {
        return get_token().stop_requested();
    }

private:
    std::shared_ptr<stop_state> state_;
};

/// Waits for @p span, or until a stop is requested. The answer is whether a
/// stop was requested, so a loop that waits between passes reads it as its
/// exit test. A token with no state waits the whole span.
inline bool wait_for(const stop_token& stop, std::chrono::milliseconds span) {
    if (!stop.state_) {
        std::this_thread::sleep_for(span);
        return false;
    }
    std::unique_lock lock(stop.state_->mutex);
    return stop.state_->woken.wait_for(lock, span, [&] { return stop.state_->stopped; });
}

/// A thread that carries a stop state and joins when it is destroyed. The
/// callable takes the stop token when it can, as it does for the standard one.
class jthread final {
public:
    jthread() = default;

    template <typename Callable>
    explicit jthread(Callable&& callable) {
        auto token = source_.get_token();
        thread_ = std::thread([token, callable = std::forward<Callable>(callable)]() mutable {
            if constexpr (std::is_invocable_v<std::decay_t<Callable>&, stop_token>)
                callable(token);
            else
                callable();
        });
    }

    jthread(const jthread&) = delete;
    jthread& operator=(const jthread&) = delete;

    jthread(jthread&& other) noexcept
        : source_(std::move(other.source_))
        , thread_(std::move(other.thread_)) {}

    jthread& operator=(jthread&& other) noexcept {
        if (this != &other) {
            request_stop();
            join();
            source_ = std::move(other.source_);
            thread_ = std::move(other.thread_);
        }
        return *this;
    }

    // A moved-from thread holds no state, so the temporary that the workflow
    // engine assigns from stays inert when it is destroyed.
    ~jthread() {
        request_stop();
        join();
    }

    bool request_stop() {
        return source_.request_stop();
    }

    [[nodiscard]] stop_token get_stop_token() const {
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
    stop_source source_;
    std::thread thread_;
};

}

namespace ores::platform::concurrency {

#if defined(ORES_PLATFORM_STANDARD_STOP)

using jthread = std::jthread;
using stop_token = std::stop_token;
using stop_source = std::stop_source;

/// Waits for @p span, or until a stop is requested, and answers whether one
/// was. The standard wait on a condition variable does the work.
inline bool wait_for(const stop_token& stop, std::chrono::milliseconds span) {
    std::mutex mutex;
    std::condition_variable_any woken;
    std::unique_lock lock(mutex);
    return woken.wait_for(lock, stop, span, [&] { return stop.stop_requested(); });
}

#else

using jthread = portable::jthread;
using stop_token = portable::stop_token;
using stop_source = portable::stop_source;
using portable::wait_for;

#endif

}

#endif
