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
#ifndef ORES_MQ_BROKER_SERVICE_REGISTRY_HPP
#define ORES_MQ_BROKER_SERVICE_REGISTRY_HPP

#include <mutex>
#include <string>
#include <vector>
#include <chrono>
#include <cstdint>
#include <utility>
#include <nng/nng.h>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::mq::broker {

/**
 * @brief Tracks registered services and their NNG pipe identifiers.
 */
class service_registry final {
public:
    struct service_entry {
        boost::uuids::uuid assigned_id;
        std::string service_name;
        std::vector<std::pair<std::uint16_t, std::uint16_t>> handled_ranges;
        nng_pipe pipe_id;
        std::chrono::system_clock::time_point connected_at;
    };

    service_registry() = default;

    /**
     * @brief Register a new service and return its assigned UUID.
     */
    boost::uuids::uuid register_service(
        const std::string& service_name,
        const std::vector<std::pair<std::uint16_t, std::uint16_t>>& handled_ranges,
        nng_pipe pipe_id);

    /**
     * @brief Remove all entries for a given pipe (called on disconnect).
     */
    void deregister_pipe(nng_pipe pipe_id);

    /**
     * @brief Get a snapshot of all registered services.
     */
    [[nodiscard]] std::vector<service_entry> all_services() const;

private:
    inline static std::string_view logger_name = "ores.mq.broker.service_registry";
    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    mutable std::mutex mutex_;
    std::vector<service_entry> services_;
};

}

#endif
