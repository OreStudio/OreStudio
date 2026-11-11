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
#ifndef ORES_NATS_DOMAIN_DELIVERY_HPP
#define ORES_NATS_DOMAIN_DELIVERY_HPP

#include "ores.nats/domain/message.hpp"
#include "ores.nats/export.hpp"
#include <chrono>
#include <functional>
#include <memory>

namespace ores::nats {

/**
 * @brief Sends a durable delivery's answer to the server.
 *
 * The transport implements it over the cnats message, which the settler owns
 * and frees when it is destroyed. A test implements it to observe the answer.
 * Only a @c delivery calls it, at most once for an answer, and never at the
 * same time as @c in_progress.
 */
class delivery_settler {
public:
    virtual ~delivery_settler() = default;

    /// The work is committed; the message is delivered no more.
    virtual void ack() noexcept = 0;
    /// The work is not done; deliver the message again now.
    virtual void nak() noexcept = 0;
    /// The work is not done; deliver the message again after @p delay.
    virtual void nak_after(std::chrono::milliseconds delay) noexcept = 0;
    /// The message can never be handled; stop delivering it.
    virtual void term() noexcept = 0;
    /// The work is still running; wait a full ack wait again before redelivering.
    virtual void in_progress() noexcept = 0;
};

/**
 * @brief One durable delivery, owned by one holder until it is settled.
 *
 * A delivery is a transaction on the message. Its holder settles it once:
 * @c commit when the work is done, @c rollback or @c rollback_after when it is
 * not, @c reject when it never can be. A delivery dropped without a settlement
 * rolls back, so a throw, an early return or a lost task leads to a
 * redelivery, never to an acknowledgement of work that was not done.
 *
 * The type enforces the ownership. It cannot be copied, so two holders cannot
 * exist; a handler gives it to another thread by moving it. A settlement
 * consumes it, so a second settlement is a use after move. While it lives, the
 * cnats message it answers lives, and the subscription that made it is not
 * destroyed.
 *
 * @code
 * void provision_step(ores::nats::delivery delivery) {
 *     const auto& msg = delivery.msg();
 *     auto req = decode<provision_step_request>(msg);
 *     if (!req)
 *         return std::move(delivery).reject();
 *     do_the_work(*req);
 *     std::move(delivery).commit();
 * }
 * @endcode
 */
class ORES_NATS_EXPORT delivery final {
public:
    delivery(message msg, std::unique_ptr<delivery_settler> settler);
    ~delivery();

    delivery(delivery&& other) noexcept;
    delivery& operator=(delivery&& other) noexcept;
    delivery(const delivery&) = delete;
    delivery& operator=(const delivery&) = delete;

    /// The message delivered. It stays readable after a settlement, and is
    /// empty once the delivery has been moved from.
    [[nodiscard]] const message& msg() const noexcept {
        return msg_;
    }

    /// The work is committed; the server delivers the message no more.
    void commit() && noexcept;

    /// The work is not done; the server delivers the message again now.
    void rollback() && noexcept;

    /**
     * @brief The work is not done; the server delivers the message again
     *        after @p delay.
     *
     * Suits a delivery that another holder is already working: an immediate
     * redelivery lands on the same live work, loses the same race, and spends
     * the delivery budget in a loop.
     */
    void rollback_after(std::chrono::milliseconds delay) && noexcept;

    /// The message can never be handled; the server stops delivering it.
    void reject() && noexcept;

    /**
     * @brief Tells the server, every @p interval, that the work still runs.
     *
     * The ticks run on a thread the delivery owns. Each tick reports progress
     * to the server and then calls @p on_tick; a tick that returns false ends
     * the reports, which lets the server redeliver once its ack wait passes.
     * The thread ends before the delivery is settled or dropped, so no tick
     * touches a delivery that is gone. Calling it again replaces the reports.
     */
    void keep_alive(std::chrono::milliseconds interval, std::function<bool()> on_tick);

private:
    void settle(void (delivery_settler::*answer)() noexcept) noexcept;
    void stop_keep_alive() noexcept;

    struct keeper;
    message msg_;
    std::unique_ptr<delivery_settler> settler_;
    std::unique_ptr<keeper> keeper_;
};

/**
 * @brief Callback type for a durable subscription: it takes the delivery.
 */
using delivery_handler = std::function<void(delivery)>;

}

#endif
