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
#ifndef ORES_ACCOUNTS_REPOSITORY_ACCOUNT_MAPPER_HPP
#define ORES_ACCOUNTS_REPOSITORY_ACCOUNT_MAPPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/repository/account_entity.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::accounts::repository {

/**
 * @brief Maps domain model entities to data storage layer and vice-versa.
 */
class account_mapper {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance =
            make_logger("ores.accounts.repository.account_mapper");
        return instance;
    }
public:
    static domain::account map(const account_entity& v);
    static account_entity map(const domain::account& v);

    static std::vector<domain::account>
    map(const std::vector<account_entity>& v);
    static std::vector<account_entity>
    map(const std::vector<domain::account>& v);
};

}

#endif
