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
#include "ores.nats/domain/delivery.hpp"
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include <utility>

namespace ores::nats {

struct delivery::keeper {
    ores::platform::concurrency::jthread thread;
};

template <typename Answer>
void delivery::settle(Answer answer) noexcept {
    stop_keep_alive();
    if (auto settler = std::move(settler_))
        answer(*settler);
}

delivery::delivery(message msg, std::unique_ptr<delivery_settler> settler)
    : msg_(std::move(msg))
    , settler_(std::move(settler)) {}

delivery::~delivery() {
    settle([](delivery_settler& s) { s.nak(); });
}

delivery::delivery(delivery&& other) noexcept
    : msg_(std::move(other.msg_))
    , settler_(std::move(other.settler_))
    , keeper_(std::move(other.keeper_)) {}

delivery& delivery::operator=(delivery&& other) noexcept {
    if (this != &other) {
        settle([](delivery_settler& s) { s.nak(); });
        msg_ = std::move(other.msg_);
        settler_ = std::move(other.settler_);
        keeper_ = std::move(other.keeper_);
    }
    return *this;
}

void delivery::commit() && noexcept {
    settle([](delivery_settler& s) { s.ack(); });
}

void delivery::rollback() && noexcept {
    settle([](delivery_settler& s) { s.nak(); });
}

void delivery::rollback_after(std::chrono::milliseconds delay) && noexcept {
    settle([delay](delivery_settler& s) { s.nak_after(delay); });
}

void delivery::reject() && noexcept {
    settle([](delivery_settler& s) { s.term(); });
}

void delivery::keep_alive(std::chrono::milliseconds interval, std::function<bool()> on_tick) {
    stop_keep_alive();
    if (!settler_)
        return;
    // The settler lives on the heap, so a move of the delivery does not move
    // it, and the thread ends before the settler is answered or freed.
    keeper_ = std::make_unique<keeper>();
    keeper_->thread = ores::platform::concurrency::jthread(
        [settler = settler_.get(), interval, on_tick = std::move(on_tick)](
            ores::platform::concurrency::stop_token stop) {
            while (!ores::platform::concurrency::wait_for(stop, interval)) {
                settler->in_progress();
                if (on_tick && !on_tick())
                    return;
            }
        });
}

void delivery::stop_keep_alive() noexcept {
    keeper_.reset();
}

}
