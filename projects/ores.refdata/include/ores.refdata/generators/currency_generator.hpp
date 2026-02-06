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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_GENERATORHPP
#define ORES_REFDATA_DOMAIN_CURRENCY_GENERATORHPP

#include <vector>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.refdata/domain/currency.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::generators {

/**
 * @brief Generates a synthetic currency.
 */
domain::currency generate_synthetic_currency(
    const utility::uuid::tenant_id& tenant_id);

/**
 * @brief Generates a synthetic currency from the unicode set.
 */
std::vector<domain::currency> generate_synthetic_unicode_currencies(
    const utility::uuid::tenant_id& tenant_id);

/**
 * @brief Generates N synthetic currencies. May contain duplicates.
 *
 * @note c++ 23 generators are not supported on all compilers.
 */
std::vector<domain::currency>
generate_synthetic_currencies(std::size_t n,
    const utility::uuid::tenant_id& tenant_id);

/**
 * @brief Generates N synthetic currencies. Does not contain duplicates.
 *
 * @note c++ 23 generators are not supported on all compilers.
 */
std::vector<domain::currency>
generate_unique_synthetic_currencies(std::size_t n,
    const utility::uuid::tenant_id& tenant_id);

/**
 * @brief Generates a set of fictional currencies.
 *
 * These are intentionally fake currencies with made-up codes that do not
 * correspond to any real ISO 4217 codes. Useful for testing and demo
 * purposes where real currency data should not be used.
 *
 * @param n Number of currencies to generate. If n is 0 or greater than the
 *          available set (50), returns all available fictional currencies.
 */
std::vector<domain::currency>
generate_fictional_currencies(std::size_t n,
    const utility::uuid::tenant_id& tenant_id);

}

#endif
