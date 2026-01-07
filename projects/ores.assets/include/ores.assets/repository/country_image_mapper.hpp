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
#ifndef ORES_ASSETS_REPOSITORY_COUNTRY_IMAGE_MAPPER_HPP
#define ORES_ASSETS_REPOSITORY_COUNTRY_IMAGE_MAPPER_HPP

#include <vector>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.assets/domain/country_image.hpp"
#include "ores.assets/repository/country_image_entity.hpp"

namespace ores::assets::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class country_image_mapper {
private:
    inline static std::string_view logger_name =
        "ores.assets.repository.country_image_mapper";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::country_image map(const country_image_entity& v);
    static country_image_entity map(const domain::country_image& v);

    static std::vector<domain::country_image>
    map(const std::vector<country_image_entity>& v);
    static std::vector<country_image_entity>
    map(const std::vector<domain::country_image>& v);
};

}

#endif
