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
#ifndef ORES_COMPUTE_WRAPPER_CONFIG_OPTIONS_HPP
#define ORES_COMPUTE_WRAPPER_CONFIG_OPTIONS_HPP

#include <cstdint>
#include <iosfwd>
#include <optional>
#include <string>
#include "ores.logging/logging_options.hpp"
#include "ores.nats/config/nats_options.hpp"

namespace ores::compute::wrapper::config {

/**
 * @brief All of the configuration options required by the wrapper.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::logging::logging_options> logging;

    /**
     * @brief Configuration related to NATS transport.
     */
    ores::nats::config::nats_options nats;

    /**
     * @brief UUID of this node's host record in the compute service.
     */
    std::string host_id;

    /**
     * @brief Tenant identifier; determines which JetStream work queue to subscribe to.
     */
    std::string tenant_id;

    /**
     * @brief Directory used for package cache and per-job scratch space.
     */
    std::string work_dir;

    /**
     * @brief Interval in seconds between heartbeat messages while a job is running.
     */
    std::uint32_t heartbeat_interval_seconds{30};

    /**
     * @brief Base URL of the HTTP server for file transfers (e.g. http://localhost:8080).
     *
     * The wrapper appends compute API paths to this URL when downloading inputs
     * and uploading outputs. If empty, URI fields from work assignments are used as-is.
     */
    std::string http_base_url;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
