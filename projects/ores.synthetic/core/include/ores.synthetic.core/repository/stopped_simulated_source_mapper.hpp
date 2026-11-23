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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_STOPPED_SIMULATED_SOURCE_MAPPER_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_STOPPED_SIMULATED_SOURCE_MAPPER_HPP

#include "ores.logging/logger.hpp"
#include "ores.synthetic.api/domain/stopped_simulated_source.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/stopped_simulated_source_entity.hpp"

namespace ores::synthetic::repository {

/**
 * @brief Maps stopped_simulated_source domain entities to data storage layer and vice-versa.
 */
class ORES_SYNTHETIC_CORE_EXPORT stopped_simulated_source_mapper {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.repository.stopped_simulated_source_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::stopped_simulated_source map(const stopped_simulated_source_entity& v);
    static stopped_simulated_source_entity map(const domain::stopped_simulated_source& v);

    static std::vector<domain::stopped_simulated_source>
    map(const std::vector<stopped_simulated_source_entity>& v);
    static std::vector<stopped_simulated_source_entity>
    map(const std::vector<domain::stopped_simulated_source>& v);
};

}

#endif
