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
#ifndef ORES_IAM_REPOSITORY_ROLE_PERMISSION_MAPPER_HPP
#define ORES_IAM_REPOSITORY_ROLE_PERMISSION_MAPPER_HPP

#include "ores.iam/domain/role_permission.hpp"
#include "ores.iam/repository/role_permission_entity.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::iam::repository {

/**
 * @brief Maps role_permission domain entities to data storage layer and vice-versa.
 */
class role_permission_mapper {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.role_permission_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::role_permission map(const role_permission_entity& v);
    static role_permission_entity map(const domain::role_permission& v);

    static std::vector<domain::role_permission>
    map(const std::vector<role_permission_entity>& v);
    static std::vector<role_permission_entity>
    map(const std::vector<domain::role_permission>& v);
};

}

#endif
