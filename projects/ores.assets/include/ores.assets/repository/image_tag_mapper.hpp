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
#ifndef ORES_ASSETS_REPOSITORY_IMAGE_TAG_MAPPER_HPP
#define ORES_ASSETS_REPOSITORY_IMAGE_TAG_MAPPER_HPP

#include <vector>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.assets/domain/image_tag.hpp"
#include "ores.assets/repository/image_tag_entity.hpp"

namespace ores::assets::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class image_tag_mapper {
private:
    inline static std::string_view logger_name =
        "ores.assets.repository.image_tag_mapper";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::image_tag map(const image_tag_entity& v);
    static image_tag_entity map(const domain::image_tag& v);

    static std::vector<domain::image_tag>
    map(const std::vector<image_tag_entity>& v);
    static std::vector<image_tag_entity>
    map(const std::vector<domain::image_tag>& v);
};

}

#endif
