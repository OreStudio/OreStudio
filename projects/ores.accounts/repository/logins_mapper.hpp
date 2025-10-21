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
#ifndef ORES_ACCOUNTS_REPOSITORY_LOGINS_MAPPER_HPP
#define ORES_ACCOUNTS_REPOSITORY_LOGINS_MAPPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include "ores.accounts/domain/logins.hpp"
#include "ores.accounts/repository/logins_entity.hpp"

namespace ores::accounts::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class logins_mapper {
public:
    static domain::logins map(const logins_entity& v);
    static logins_entity map(const domain::logins& v);

    static std::vector<domain::logins>
    map(const std::vector<logins_entity>& v);
    static std::vector<logins_entity>
    map(const std::vector<domain::logins>& v);
};

}

#endif
