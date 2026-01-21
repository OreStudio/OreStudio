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
#ifndef ORES_TELEMETRY_REPOSITORY_TELEMETRY_MAPPER_HPP
#define ORES_TELEMETRY_REPOSITORY_TELEMETRY_MAPPER_HPP

#include "ores.telemetry/domain/telemetry_log_entry.hpp"
#include "ores.telemetry/domain/telemetry_stats.hpp"
#include "ores.telemetry/repository/telemetry_entity.hpp"

namespace ores::telemetry::repository {

/**
 * @brief Maps between domain and entity types for telemetry.
 */
class telemetry_mapper {
public:
    /**
     * @brief Converts a domain log entry to a database entity.
     */
    static telemetry_entity to_entity(const domain::telemetry_log_entry& entry);

    /**
     * @brief Converts a database entity to a domain log entry.
     */
    static domain::telemetry_log_entry to_domain(const telemetry_entity& entity);

    /**
     * @brief Converts an hourly stats entity to a domain stats object.
     */
    static domain::telemetry_stats to_domain(
        const telemetry_stats_hourly_entity& entity);

    /**
     * @brief Converts a daily stats entity to a domain stats object.
     */
    static domain::telemetry_stats to_domain(
        const telemetry_stats_daily_entity& entity);
};

}

#endif
