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
#ifndef ORES_VARIABILITY_SERVICE_SYSTEM_FLAGS_SEEDER_HPP
#define ORES_VARIABILITY_SERVICE_SYSTEM_FLAGS_SEEDER_HPP

#include <string_view>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/service/feature_flags_service.hpp"

namespace ores::variability::service {

/**
 * @brief Seeds system flags in the database at startup.
 *
 * The system_flags_seeder ensures that all well-known system flags exist in the
 * database with their default values. This should be called early during
 * service startup, before any component queries system flags.
 *
 * The seeder only creates flags that don't already exist - it does not
 * overwrite existing flag values, preserving any user modifications.
 *
 * This is idempotent - running it multiple times will not create duplicate
 * entries.
 */
class system_flags_seeder {
private:
    inline static std::string_view logger_name =
        "ores.variability.service.system_flags_seeder";

    [[nodiscard]] static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a system_flags_seeder.
     *
     * @param ctx The database context to be used by the underlying repository.
     */
    explicit system_flags_seeder(database::context ctx);

    /**
     * @brief Seeds all system flags into the database.
     *
     * Iterates through the compile-time manifest of system flags and creates
     * any flags that don't already exist in the database. Existing flags are
     * left unchanged.
     *
     * @return The number of flags that were created (0 if all already existed).
     */
    std::size_t seed();

private:
    feature_flags_service feature_flags_service_;
};

}

#endif
