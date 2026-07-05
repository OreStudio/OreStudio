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
#ifndef ORES_REFDATA_API_GENERATORS_CURRENCY_GENERATOR_HPP
#define ORES_REFDATA_API_GENERATORS_CURRENCY_GENERATOR_HPP

#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <vector>

namespace ores::refdata::generators {

/**
 * @brief Generates a synthetic currency.
 */
ORES_REFDATA_API_EXPORT domain::currency
generate_synthetic_currency(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic currencies.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::currency>
generate_synthetic_currencies(std::size_t n, utility::generation::generation_context& ctx);

/**
 * @brief Generates a set of currencies using Unicode symbols.
 *
 * Used for testing internationalisation and symbol rendering.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::currency>
generate_synthetic_unicode_currencies(utility::generation::generation_context& ctx);

/**
 * @brief Generates N synthetic currencies with unique ISO codes.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::currency>
generate_unique_synthetic_currencies(std::size_t n, utility::generation::generation_context& ctx);

/**
 * @brief Generates a set of fictional currencies.
 *
 * These are intentionally fake currencies with made-up ISO codes that do not
 * correspond to any real ISO 4217 codes. Useful for testing and demo
 * purposes where real currency data should not be used.
 *
 * @param n Number of currencies to generate. If n is 0 or greater than the
 *          available set (50), returns all available fictional currencies.
 */
ORES_REFDATA_API_EXPORT std::vector<domain::currency>
generate_fictional_currencies(std::size_t n, utility::generation::generation_context& ctx);

/**
 * @brief Generates a single, randomly-selected fictional currency.
 *
 * Used by UI "generate" actions that fill a form with one sample
 * currency, rather than adding a batch to a list.
 */
ORES_REFDATA_API_EXPORT domain::currency
generate_random_fictional_currency(utility::generation::generation_context& ctx);
}

#endif
