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
#ifndef ORES_ASSETS_REPOSITORY_CURRENCY_IMAGE_MAPPER_HPP
#define ORES_ASSETS_REPOSITORY_CURRENCY_IMAGE_MAPPER_HPP

#include <vector>
#include "ores.utility/log/make_logger.hpp"
#include "ores.assets/domain/currency_image.hpp"
#include "ores.assets/repository/currency_image_entity.hpp"

namespace ores::assets::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class currency_image_mapper {
private:
    inline static std::string_view logger_name =
        "ores.assets.repository.currency_image_mapper";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::currency_image map(const currency_image_entity& v);
    static currency_image_entity map(const domain::currency_image& v);

    static std::vector<domain::currency_image>
    map(const std::vector<currency_image_entity>& v);
    static std::vector<currency_image_entity>
    map(const std::vector<domain::currency_image>& v);
};

}

#endif
