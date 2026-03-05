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
#ifndef ORES_MQ_BROKER_ROUTING_TABLE_HPP
#define ORES_MQ_BROKER_ROUTING_TABLE_HPP

#include <mutex>
#include <vector>
#include <cstdint>
#include <optional>
#include <nng/nng.h>
#include "ores.logging/make_logger.hpp"

namespace ores::mq::broker {

/**
 * @brief Thread-safe map from message-type ranges to NNG pipe IDs.
 *
 * Services register ranges on startup; the broker looks up the destination
 * pipe for each incoming message type. When a pipe closes the corresponding
 * routes are removed automatically.
 */
class routing_table final {
public:
    routing_table() = default;

    /**
     * @brief Register a message-type range for a pipe.
     *
     * @param min Inclusive lower bound of the handled message-type range
     * @param max Inclusive upper bound of the handled message-type range
     * @param pipe_id NNG pipe identifier for the registered service
     */
    void register_route(std::uint16_t min, std::uint16_t max, nng_pipe pipe_id);

    /**
     * @brief Remove all routes associated with a pipe (called on disconnect).
     *
     * @param pipe_id The pipe whose routes should be removed
     */
    void deregister_pipe(nng_pipe pipe_id);

    /**
     * @brief Find the pipe that handles a given message type.
     *
     * @param msg_type The 16-bit message type value
     * @return Pipe ID if a route exists, nullopt otherwise
     */
    [[nodiscard]] std::optional<nng_pipe> find_pipe(std::uint16_t msg_type) const;

private:
    struct route_entry {
        std::uint16_t min;
        std::uint16_t max;
        nng_pipe pipe_id;
    };

    inline static std::string_view logger_name = "ores.mq.broker.routing_table";
    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    mutable std::mutex mutex_;
    std::vector<route_entry> routes_;
};

}

#endif
