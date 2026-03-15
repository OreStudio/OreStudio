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
#ifndef ORES_NATS_SERVICE_SUBSCRIPTION_HPP
#define ORES_NATS_SERVICE_SUBSCRIPTION_HPP

#include <memory>

namespace ores::nats::service {

/**
 * @brief RAII handle for a NATS subscription.
 *
 * Automatically unsubscribes and frees cnats resources when destroyed.
 * Move-only — subscriptions cannot be shared or copied.
 *
 * A subscription must not outlive the @c client that created it.
 *
 * @code
 * auto sub = nats.queue_subscribe("ores.iam.v1.>", "ores.iam.service", handler);
 * // ... sub unsubscribes automatically at end of scope
 * @endcode
 */
class [[nodiscard]] subscription {
public:
    ~subscription();

    subscription(subscription&&) noexcept;
    subscription& operator=(subscription&&) noexcept;

    subscription(const subscription&) = delete;
    subscription& operator=(const subscription&) = delete;

    /**
     * @brief Drain the subscription.
     *
     * Stops new message delivery and waits for all in-flight messages
     * to be processed before unsubscribing.
     */
    void drain();

public:
    // Internal: constructed only by client
    struct impl;
    explicit subscription(std::unique_ptr<impl> i);

private:
    std::unique_ptr<impl> impl_;
};

}

#endif
