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
#include "ores.http.core/stream/change_ticker.hpp"
#include <boost/asio/post.hpp>
#include <boost/asio/redirect_error.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <utility>

namespace ores::http::stream {

namespace {

/**
 * @brief How long the loop sleeps while no change is waiting.
 *
 * It bounds nothing but the sleep: a change expires the timer as it arms the window,
 * so this is only how long an idle server is allowed to stay asleep.
 */
constexpr auto idle_wait = std::chrono::hours(1);

}

change_ticker::change_ticker(change_coalescer& coalescer,
                             change_hub& hub,
                             std::chrono::milliseconds window)
    : pump_(coalescer, hub)
    , coalescer_(coalescer)
    , window_(window) {
    coalescer_.set_wake([this] { arm(); });
}

change_ticker::~change_ticker() {
    coalescer_.set_wake(nullptr);
}

boost::asio::awaitable<void> change_ticker::run(boost::asio::any_io_executor executor) {
    const auto open = std::make_shared<window>();
    open->timer = std::make_shared<boost::asio::steady_timer>(executor);
    {
        const std::lock_guard lock(mu_);
        open_ = open;
        executor_ = std::make_shared<boost::asio::any_io_executor>(executor);
    }

    while (!open->done) {
        /*
         * A change recorded before the ticker ran found no one to arm its window, so
         * the first pass checks whether anything is already waiting.
         */
        if (!open->armed && coalescer_.pending() > 0)
            open->armed = true;

        const auto closing = open->armed;
        open->timer->expires_after(closing ? window_ : idle_wait);

        boost::system::error_code ec;
        co_await open->timer->async_wait(
            boost::asio::redirect_error(boost::asio::use_awaitable, ec));
        if (open->done)
            break;

        // An arm cut the sleep short: the next pass waits the window out.
        if (!closing)
            continue;

        open->armed = false;
        pump_.pump();
    }
}

void change_ticker::arm() {
    post([](const std::shared_ptr<window>& open) {
        if (open->done || open->armed)
            return;
        open->armed = true;
        open->timer->expires_at(std::chrono::steady_clock::now());
    });
}

void change_ticker::stop() {
    stopped_ = true;
    post([](const std::shared_ptr<window>& open) {
        open->done = true;
        open->timer->expires_at(std::chrono::steady_clock::now());
    });
}

void change_ticker::post(std::function<void(const std::shared_ptr<window>&)> change) {
    std::shared_ptr<boost::asio::any_io_executor> executor;
    std::shared_ptr<window> open;
    {
        const std::lock_guard lock(mu_);
        executor = executor_;
        open = open_;
    }
    if (!executor || !open)
        return;
    boost::asio::post(*executor, [open, change = std::move(change)] { change(open); });
}

}
