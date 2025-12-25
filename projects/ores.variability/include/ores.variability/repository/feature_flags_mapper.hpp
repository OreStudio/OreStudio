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
#ifndef ORES_VARIABILITY_REPOSITORY_FEATURE_FLAGS_MAPPER_HPP
#define ORES_VARIABILITY_REPOSITORY_FEATURE_FLAGS_MAPPER_HPP

#include <vector>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"
#include "ores.variability/repository/feature_flags_entity.hpp"

namespace ores::variability::repository {

/**
 * @brief Maps feature flags domain model entities to data storage layer and vice-versa.
 */
class feature_flags_mapper {
private:
    inline static std::string_view logger_name =
        "ores.variability.repository.feature_flags_mapper";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::feature_flags map(const feature_flags_entity& v);
    static feature_flags_entity map(const domain::feature_flags& v);

    static std::vector<domain::feature_flags>
    map(const std::vector<feature_flags_entity>& v);
    static std::vector<feature_flags_entity>
    map(const std::vector<domain::feature_flags>& v);
};

}

#endif
