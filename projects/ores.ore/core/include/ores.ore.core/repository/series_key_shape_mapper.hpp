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
#ifndef ORES_ORE_CORE_REPOSITORY_SERIES_KEY_SHAPE_MAPPER_HPP
#define ORES_ORE_CORE_REPOSITORY_SERIES_KEY_SHAPE_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.api/domain/series_key_shape.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/repository/series_key_shape_entity.hpp"

namespace ores::ore::repository {

/**
 * @brief Maps series_key_shape domain entities to data storage layer and vice-versa.
 */
class ORES_ORE_CORE_EXPORT series_key_shape_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.repository.series_key_shape_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::series_key_shape map(const series_key_shape_entity& v);
    static series_key_shape_entity map(const domain::series_key_shape& v);

    static std::vector<domain::series_key_shape> map(const std::vector<series_key_shape_entity>& v);
    static std::vector<series_key_shape_entity> map(const std::vector<domain::series_key_shape>& v);
};

}

#endif
