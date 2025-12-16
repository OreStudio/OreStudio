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
#ifndef ORES_ACCOUNTS_REPOSITORY_PERMISSION_MAPPER_HPP
#define ORES_ACCOUNTS_REPOSITORY_PERMISSION_MAPPER_HPP

#include "ores.accounts/domain/permission.hpp"
#include "ores.accounts/repository/permission_entity.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::accounts::repository {

/**
 * @brief Maps permission domain entities to data storage layer and vice-versa.
 */
class permission_mapper {
private:
    inline static std::string_view logger_name =
        "ores.accounts.repository.permission_mapper";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }
public:
    static domain::permission map(const permission_entity& v);
    static permission_entity map(const domain::permission& v);

    static std::vector<domain::permission>
    map(const std::vector<permission_entity>& v);
    static std::vector<permission_entity>
    map(const std::vector<domain::permission>& v);
};

}

#endif
