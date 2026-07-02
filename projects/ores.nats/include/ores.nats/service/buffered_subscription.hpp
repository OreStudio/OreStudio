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
#ifndef ORES_NATS_SERVICE_BUFFERED_SUBSCRIPTION_HPP
#define ORES_NATS_SERVICE_BUFFERED_SUBSCRIPTION_HPP

#include "ores.nats/domain/message.hpp"
#include "ores.nats/export.hpp"
#include <cstddef>
#include <memory>
#include <vector>

namespace ores::nats::service {

/**
 * @brief A NATS subscription that retains the last @p capacity messages.
 *
 * Wraps a @c subscription and a fixed-size circular message buffer. Each
 * arriving message is appended to the buffer before the caller's handler
 * is invoked. The handler is optional — callers that only need history can
 * omit it and poll via @c snapshot().
 *
 * Thread safety:
 * - The NATS dispatch thread writes to the buffer.
 * - Any thread may call @c snapshot() or @c size() concurrently.
 * - @c snapshot() returns a full copy, so no external locking is needed.
 *
 * Move-only. Must not outlive the @c client that created it.
 *
 * @code
 * auto sub = nats.subscribe_buffered("synthetic.v1.tick.eur.usd", 1000);
 * // ... later, on the UI thread:
 * auto recent = sub.snapshot(); // vector<message>, oldest first
 * @endcode
 */
class [[nodiscard]] ORES_NATS_EXPORT buffered_subscription {
public:
    ~buffered_subscription();

    buffered_subscription(buffered_subscription&&) noexcept;
    buffered_subscription& operator=(buffered_subscription&&) noexcept;

    buffered_subscription(const buffered_subscription&) = delete;
    buffered_subscription& operator=(const buffered_subscription&) = delete;

    /**
     * @brief Thread-safe snapshot of all buffered messages, oldest first.
     *
     * Returns a copy — safe to read without holding any lock.
     */
    [[nodiscard]] std::vector<message> snapshot() const;

    /**
     * @brief Number of messages currently in the buffer.
     */
    [[nodiscard]] std::size_t size() const;

public:
    // Internal: constructed only by client::subscribe_buffered.
    struct impl;
    explicit buffered_subscription(std::unique_ptr<impl> i);

private:
    std::unique_ptr<impl> impl_;
};

}

#endif
