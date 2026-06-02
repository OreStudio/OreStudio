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
#ifndef ORES_VARIABILITY_REPOSITORY_SYSTEM_SETTING_MAPPER_HPP
#define ORES_VARIABILITY_REPOSITORY_SYSTEM_SETTING_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.core/repository/system_setting_entity.hpp"

namespace ores::variability::repository {

/**
 * @brief Maps system_setting domain entities to database entities and vice-versa.
 */
class system_setting_mapper {
private:
    inline static std::string_view logger_name =
        "ores.variability.repository.system_setting_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static domain::system_setting map(const system_setting_entity& v);
    static system_setting_entity map(const domain::system_setting& v);

    static std::vector<domain::system_setting>
    map(const std::vector<system_setting_entity>& v);
    static std::vector<system_setting_entity>
    map(const std::vector<domain::system_setting>& v);
};

}

#endif
