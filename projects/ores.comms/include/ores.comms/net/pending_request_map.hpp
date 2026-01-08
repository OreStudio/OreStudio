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
#ifndef ORES_COMMS_NET_PENDING_REQUEST_MAP_HPP
#define ORES_COMMS_NET_PENDING_REQUEST_MAP_HPP

#include <mutex>
#include <memory>
#include <cstdint>
#include <unordered_map>
#include <boost/asio/any_io_executor.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/net/response_channel.hpp"

namespace ores::comms::net {

/**
 * @brief Thread-safe map for tracking pending requests by correlation ID.
 *
 * Used by the client to match incoming responses to their originating
 * requests. When a request is sent, a response_channel is registered
 * with its correlation ID. When a response arrives, the message loop
 * looks up the channel by correlation ID and delivers the response.
 */
class pending_request_map final {
private:
  inline static std::string_view logger_name =
      "ores.comms.net.pending_request_map";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit pending_request_map(boost::asio::any_io_executor executor);

    /**
     * @brief Register a new pending request.
     *
     * Creates a response_channel for the given correlation ID.
     * The caller should co_await the returned channel's get() method.
     *
     * @param correlation_id The correlation ID for this request
     * @return Shared pointer to the response channel
     */
    std::shared_ptr<response_channel> register_request(std::uint32_t correlation_id);

    /**
     * @brief Complete a pending request with a response.
     *
     * Called by the message loop when a response frame arrives.
     * Delivers the response to the waiting coroutine.
     *
     * @param correlation_id The correlation ID from the response
     * @param response The response frame
     * @return true if a pending request was found and completed
     */
    bool complete(std::uint32_t correlation_id, messaging::frame response);

    /**
     * @brief Fail a specific pending request with an error.
     *
     * Called on timeout for a specific request.
     *
     * @param correlation_id The correlation ID to fail
     * @param ec The error code
     * @return true if a pending request was found and failed
     */
    bool fail(std::uint32_t correlation_id, ores::utility::serialization::error_code ec);

    /**
     * @brief Fail all pending requests with an error.
     *
     * Called when connection is lost - fails all waiting requests.
     *
     * @param ec The error code to deliver to all pending requests
     */
    void fail_all(ores::utility::serialization::error_code ec);

    /**
     * @brief Remove a completed/cancelled request from the map.
     *
     * Should be called after get() returns to clean up.
     *
     * @param correlation_id The correlation ID to remove
     */
    void remove(std::uint32_t correlation_id);

    /**
     * @brief Get the number of pending requests.
     */
    std::size_t size() const;

    /**
     * @brief Check if there are any pending requests.
     */
    bool empty() const;

private:
    boost::asio::any_io_executor executor_;
    mutable std::mutex mutex_;
    std::unordered_map<std::uint32_t, std::shared_ptr<response_channel>> pending_;
};

}

#endif
