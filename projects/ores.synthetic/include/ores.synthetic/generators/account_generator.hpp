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
#ifndef ORES_SYNTHETIC_GENERATORS_ACCOUNT_GENERATOR_HPP
#define ORES_SYNTHETIC_GENERATORS_ACCOUNT_GENERATOR_HPP

#include <vector>
#include "ores.iam/domain/account.hpp"
#include "ores.synthetic/domain/generation_context.hpp"

namespace ores::synthetic::generators {

/**
 * @brief Generates a synthetic IAM account using random data.
 *
 * Creates a complete account with randomly generated username, email,
 * password hash, salt, and TOTP secret suitable for testing purposes.
 * Uses the faker library for random generation.
 */
iam::domain::account generate_synthetic_account();

/**
 * @brief Generates a synthetic IAM account with controlled randomness.
 *
 * @param ctx Generation context for controlled random values.
 * @return A synthetic account.
 */
iam::domain::account generate_synthetic_account(domain::generation_context& ctx);

/**
 * @brief Generates N synthetic IAM accounts.
 *
 * @param n Number of accounts to generate.
 * @return Vector of synthetic accounts.
 */
std::vector<iam::domain::account>
generate_synthetic_accounts(std::size_t n);

/**
 * @brief Generates N synthetic IAM accounts with controlled randomness.
 *
 * @param n Number of accounts to generate.
 * @param ctx Generation context for controlled random values.
 * @return Vector of synthetic accounts.
 */
std::vector<iam::domain::account>
generate_synthetic_accounts(std::size_t n, domain::generation_context& ctx);

}

#endif
