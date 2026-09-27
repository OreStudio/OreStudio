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
#ifndef ORES_PLATFORM_CONCURRENCY_ATOMIC_SHARED_PTR_HPP
#define ORES_PLATFORM_CONCURRENCY_ATOMIC_SHARED_PTR_HPP

#include <memory>
#include <mutex>
#include <utility>

namespace ores::platform::concurrency {

/**
 * @brief A shared_ptr one thread replaces while other threads read it.
 *
 * The portable counterpart of @c std::atomic<std::shared_ptr<T>>: it publishes
 * an immutable snapshot without a reader ever seeing a half-written object, and
 * it leaves the snapshot it handed out alive after a later store.
 *
 * The mutex is deliberate, not a placeholder for an atomic. libc++ has no
 * @c std::atomic<std::shared_ptr<T>> specialisation, so on macOS the name
 * resolves to the generic @c std::atomic<T> primary template, which
 * static_asserts that T is trivially copyable and refuses to compile. The one
 * code path here therefore compiles and is tested on every platform rather than
 * two paths of which one is only ever built on macOS. The cost is a lock and an
 * unlock per read of an uncontended mutex, which the callers that publish
 * settings reloads do not notice.
 *
 * @tparam T Pointee type. Declare it const to make the snapshot immutable
 * through the pointer, which is the shape every caller here wants.
 */
template <class T>
class atomic_shared_ptr {
public:
    atomic_shared_ptr() = default;

    explicit atomic_shared_ptr(std::shared_ptr<T> value)
        : value_(std::move(value)) {}

    atomic_shared_ptr(const atomic_shared_ptr&) = delete;
    atomic_shared_ptr& operator=(const atomic_shared_ptr&) = delete;
    atomic_shared_ptr(atomic_shared_ptr&&) = delete;
    atomic_shared_ptr& operator=(atomic_shared_ptr&&) = delete;

    /**
     * @brief The current snapshot, which stays alive until the caller drops it
     * even if another thread stores a replacement first.
     */
    [[nodiscard]] std::shared_ptr<T> load() const {
        std::lock_guard lock(mutex_);
        return value_;
    }

    /**
     * @brief Publish @p value as the current snapshot.
     */
    void store(std::shared_ptr<T> value) {
        std::lock_guard lock(mutex_);
        value_ = std::move(value);
    }

private:
    mutable std::mutex mutex_;
    std::shared_ptr<T> value_;
};

}

#endif
