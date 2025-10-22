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
#ifndef ORES_ACCOUNTS_REPOSITORY_LOGIN_INFO_MAPPER_HPP
#define ORES_ACCOUNTS_REPOSITORY_LOGIN_INFO_MAPPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include "ores.accounts/domain/login_info.hpp"
#include "ores.accounts/repository/login_info_entity.hpp"

namespace ores::accounts::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class login_info_mapper {
public:
    static domain::login_info map(const login_info_entity& v);
    static login_info_entity map(const domain::login_info& v);

    static std::vector<domain::login_info>
    map(const std::vector<login_info_entity>& v);
    static std::vector<login_info_entity>
    map(const std::vector<domain::login_info>& v);
};

}

#endif
