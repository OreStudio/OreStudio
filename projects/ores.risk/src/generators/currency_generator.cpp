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
#include "ores.risk/generators/currency_generator.hpp"

#include <unordered_set>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/finance.h>
#include "ores.utility/faker/datetime.hpp"

namespace ores::risk::generators {

domain::currency generate_synthetic_currency() {
    domain::currency r;

    auto fakerCurrency = faker::finance::currency();
    r.iso_code = fakerCurrency.code;
    r.name = std::string(fakerCurrency.name);
    r.numeric_code = std::to_string(faker::number::integer(1, 999));
    r.symbol = std::string(fakerCurrency.symbol);
    r.fraction_symbol = "";
    r.fractions_per_unit = 100;
    r.rounding_type = "Closest";
    r.rounding_precision = 2;
    r.format = "%3% %1$.2f";
    r.currency_type = "Fiat";
    r.recorded_by = std::string(faker::internet::username());
    r.recorded_at = utility::faker::datetime::past_timepoint();

    return r;
}

std::vector<domain::currency> generate_synthetic_unicode_currencies() {
    std::vector<domain::currency> r;
    r.push_back({
        .iso_code = "USD",
        .name = "United States Dollar",
        .numeric_code = "840",
        .symbol = "$",
        .fraction_symbol = "¢",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "EUR",
        .name = "Euro",
        .numeric_code = "110",
        .symbol = "€",
        .fraction_symbol = "¢",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "GBP",
        .name = "British Pound Sterling",
        .numeric_code = "110",
        .symbol = "£",
        .fraction_symbol = "p",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "JPY",
        .name = "Japanese Yen",
        .numeric_code = "110",
        .symbol = "¥",
        .fractions_per_unit = 0,
        .rounding_type = "Closest",
        .rounding_precision = 0,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "INR",
        .name = "Indian Rupee",
        .numeric_code = "110",
        .symbol = "₹",
        .fraction_symbol = "प",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "BTC",
        .name = "Bitcoin",
        .numeric_code = "110",
        .symbol = "₿",
        .fraction_symbol = "s",
        .fractions_per_unit = 100000000,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });

    r.push_back({
        .iso_code = "RUB",
        .name = "Russian Rubble",
        .numeric_code = "110",
        .symbol = "₽",
        .fraction_symbol = "к",
        .fractions_per_unit = 100,
        .rounding_type = "Closest",
        .rounding_precision = 2,
        .format = "%3% %1$.2f",
        .currency_type = "master",
        .recorded_by = std::string(faker::internet::username()),
        .recorded_at = utility::faker::datetime::past_timepoint()
    });
    return r;
}

std::vector<domain::currency>
generate_synthetic_currencies(std::size_t n) {
    std::vector<domain::currency> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_currency());

    return r;
}

std::vector<domain::currency>
generate_unique_synthetic_currencies(std::size_t n) {
    std::unordered_set<std::string> seen;
    seen.reserve(n);

    std::vector<domain::currency> r;
    r.reserve(n);

    while (r.size() < n) {
        auto currency = generate_synthetic_currency();
        bool not_seen = seen.insert(currency.iso_code).second;
        if (not_seen)
            r.push_back(std::move(currency));
    }
    return r;
}

}
