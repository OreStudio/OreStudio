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
#ifndef ORES_TELEMETRY_DOMAIN_TELEMETRY_BATCH_HPP
#define ORES_TELEMETRY_DOMAIN_TELEMETRY_BATCH_HPP

#include <vector>
#include <string>
#include "ores.telemetry/domain/telemetry_source.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"

namespace ores::telemetry::domain {

/**
 * @brief A batch of telemetry log entries from a single source.
 *
 * Batching improves network efficiency and database insert performance.
 * Clients should batch logs and submit them periodically (e.g., every 5
 * seconds or when a batch size threshold is reached).
 */
struct telemetry_batch final {
    /**
     * @brief Source type (client or server).
     */
    telemetry_source source = telemetry_source::client;

    /**
     * @brief Name of the source application.
     *
     * This is set once per batch rather than per-entry for efficiency.
     * Examples: "ores.qt", "ores.comms.shell", "ores.comms.service"
     */
    std::string source_name;

    /**
     * @brief The log entries in this batch.
     */
    std::vector<telemetry_log_entry> entries;

    /**
     * @brief Returns the number of entries in the batch.
     */
    [[nodiscard]] std::size_t size() const noexcept {
        return entries.size();
    }

    /**
     * @brief Returns true if the batch is empty.
     */
    [[nodiscard]] bool empty() const noexcept {
        return entries.empty();
    }

    /**
     * @brief Clears all entries from the batch.
     */
    void clear() noexcept {
        entries.clear();
    }
};

}

#endif
