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
#ifndef ORES_IAM_DOMAIN_ACCOUNT_GENERATOR_HPP
#define ORES_IAM_DOMAIN_ACCOUNT_GENERATOR_HPP

#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.iam/domain/account.hpp"

namespace ores::iam::generators {

/**
 * @brief Generates a synthetic account.
 *
 * @param tenant_id The tenant to associate with the account.
 */
domain::account generate_synthetic_account(const boost::uuids::uuid& tenant_id);

/**
 * @brief Generates N synthetic accounts.
 *
 * @param n Number of accounts to generate.
 * @param tenant_id The tenant to associate with the accounts.
 * @note c++ 23 generators are not supported on all compilers.
 */
std::vector<domain::account>
generate_synthetic_accounts(std::size_t n, const boost::uuids::uuid& tenant_id);

}

#endif
