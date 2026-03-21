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
#ifndef ORES_COMPUTE_REPOSITORY_APP_VERSION_MAPPER_HPP
#define ORES_COMPUTE_REPOSITORY_APP_VERSION_MAPPER_HPP

#include "ores.compute/domain/app_version.hpp"
#include "ores.compute/repository/app_version_entity.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::compute::repository {

/**
 * @brief Maps app_version domain entities to data storage layer and vice-versa.
 */
class app_version_mapper {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.app_version_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::app_version map(const app_version_entity& v);
    static app_version_entity map(const domain::app_version& v);

    static std::vector<domain::app_version>
    map(const std::vector<app_version_entity>& v);
    static std::vector<app_version_entity>
    map(const std::vector<domain::app_version>& v);
};

}

#endif
