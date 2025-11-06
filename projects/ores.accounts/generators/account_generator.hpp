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
#ifndef ORES_ACCOUNTS_DOMAIN_ACCOUNT_GENERATORHPP
#define ORES_ACCOUNTS_DOMAIN_ACCOUNT_GENERATORHPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <generator>
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.accounts/domain/account.hpp"

namespace ores::accounts::generators {

/**
 * @brief Generates a fake account.
 */
domain::account generate_fake_account();

/**
 * @brief Generates N fake accounts, up to limit.
 */
inline std::generator<domain::account>
generate_fake_accounts(std::size_t limit = 10000) {
    std::size_t i = 0;
    while (i++ < limit)
        co_yield generate_fake_account();
}

}

#endif
