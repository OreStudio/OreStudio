/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.comms/net/response_channel.hpp"

#include <chrono>
#include <boost/asio/error.hpp>
#include <boost/asio/redirect_error.hpp>

namespace ores::comms::net {

response_channel::response_channel(boost::asio::any_io_executor executor)
    : signal_(std::move(executor)), ready_(false) {
    // Set timer to "never expire" initially - we'll cancel it to signal
    signal_.expires_at(std::chrono::steady_clock::time_point::max());
}

void response_channel::set_value(messaging::frame response) {
    {
        std::lock_guard guard{mutex_};
        if (ready_) {
            return; // Already set, ignore duplicate
        }
        response_ = std::move(response);
        ready_ = true;
    }
    // Cancel the timer to wake up any waiting get()
    signal_.cancel();
}

void response_channel::set_error(messaging::error_code ec) {
    {
        std::lock_guard guard{mutex_};
        if (ready_) {
            return; // Already set, ignore duplicate
        }
        error_ = ec;
        ready_ = true;
    }
    // Cancel the timer to wake up any waiting get()
    signal_.cancel();
}

boost::asio::awaitable<std::expected<messaging::frame, messaging::error_code>>
response_channel::get() {
    // Check if already ready (fast path)
    {
        std::lock_guard guard{mutex_};
        if (ready_) {
            if (error_) {
                co_return std::unexpected(*error_);
            }
            co_return std::move(*response_);
        }
    }

    // Wait for signal (timer cancel)
    boost::system::error_code ec;
    co_await signal_.async_wait(
        boost::asio::redirect_error(boost::asio::use_awaitable, ec));

    // Timer was cancelled (normal case) or expired (shouldn't happen)
    // Either way, check the result
    {
        std::lock_guard guard{mutex_};
        if (error_) {
            co_return std::unexpected(*error_);
        }
        if (response_) {
            co_return std::move(*response_);
        }
        // This shouldn't happen - signal without value
        co_return std::unexpected(messaging::error_code::network_error);
    }
}

bool response_channel::is_ready() const {
    std::lock_guard guard{mutex_};
    return ready_;
}

}
