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
#ifndef ORES_COMMS_NET_RESPONSE_CHANNEL_HPP
#define ORES_COMMS_NET_RESPONSE_CHANNEL_HPP

#include <mutex>
#include <optional>
#include <expected>
#include <boost/asio/any_io_executor.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::net {

/**
 * @brief Single-value async channel for delivering a response frame.
 *
 * Used by the message loop to deliver responses to waiting coroutines.
 * Each channel can hold exactly one response (or error). The consumer
 * calls get() which blocks until set_value() or set_error() is called
 * by the producer (message loop).
 *
 * Uses a steady_timer as the signaling primitive - a common Boost.Asio
 * pattern for implementing async channels without std::experimental features.
 */
class response_channel final {
public:
    explicit response_channel(boost::asio::any_io_executor executor);

    // Non-copyable, non-movable (timer has executor affinity)
    response_channel(const response_channel&) = delete;
    response_channel& operator=(const response_channel&) = delete;
    response_channel(response_channel&&) = delete;
    response_channel& operator=(response_channel&&) = delete;

    /**
     * @brief Set the response value (producer side).
     *
     * Called by the message loop when a response arrives.
     * Wakes up the waiting get() coroutine.
     *
     * @param response The response frame to deliver
     */
    void set_value(messaging::frame response);

    /**
     * @brief Set an error (producer side).
     *
     * Called by the message loop on timeout or disconnect.
     * Wakes up the waiting get() coroutine with an error.
     *
     * @param ec The error code to deliver
     */
    void set_error(messaging::error_code ec);

    /**
     * @brief Wait for and retrieve the response (consumer side).
     *
     * Blocks until set_value() or set_error() is called.
     *
     * @return Expected containing the response frame, or error_code
     */
    boost::asio::awaitable<std::expected<messaging::frame, messaging::error_code>> get();

    /**
     * @brief Check if the channel has already received a value or error.
     */
    bool is_ready() const;

private:
    boost::asio::steady_timer signal_;
    mutable std::mutex mutex_;
    std::optional<messaging::frame> response_;
    std::optional<messaging::error_code> error_;
    bool ready_;
};

}

#endif
