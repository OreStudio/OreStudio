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
#ifndef ORES_RISK_DOMAIN_CURRENCY_GENERATORHPP
#define ORES_RISK_DOMAIN_CURRENCY_GENERATORHPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <generator>
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::generators {

/**
 * @brief Generates a fake currency.
 */
domain::currency generate_fake_currency();

/**
 * @brief Generates a fake currency from the unicode set.
 */
std::vector<domain::currency> generate_fake_unicode_currencies();

/**
 * @brief Generates N fake currencies, up to limit.
 */
inline std::generator<domain::currency>
generate_fake_currencies(std::size_t limit = 10000) {
    std::size_t i = 0;
    while (i++ < limit)
        co_yield generate_fake_currency();
}

}

#endif
