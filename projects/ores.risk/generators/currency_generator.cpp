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

#include <chrono>
#include <format>
#include <random>
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/string_generator.hpp>
#include <faker-cxx/finance.h>

namespace {

std::string fake_datetime_string() {
    // Define range: e.g., years 1970 to 2038 (avoid 9999 unless needed)
    using namespace std::chrono;
    static thread_local std::mt19937 rng{std::random_device{}()};

    // Unix time range: 0 = 1970-01-01, max ~2106 for 32-bit, but we use 64-bit
    const auto min_time = sys_days{year{1970}/1/1}.time_since_epoch();
    const auto max_time = sys_days{year{2038}/12/31}.time_since_epoch() + 24h - 1s;

    std::uniform_int_distribution dist(
        min_time.count(),
        max_time.count()
    );

    auto tp = sys_seconds{seconds{dist(rng)}};

    // Format as "YYYY-MM-DD HH:MM:SS"
    return std::format("{:%Y-%m-%d %H:%M:%S}", tp);
}

}

namespace ores::risk::generators {

domain::currency generate_fake_currency() {
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
    r.modified_by = std::string(faker::internet::username());
    r.valid_from = fake_datetime_string();
    r.valid_to = "9999-12-31 23:59:59";

    return r;
}

std::vector<domain::currency> generate_fake_unicode_currencies() {
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
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
        .modified_by = std::string(faker::internet::username()),
        .valid_from = fake_datetime_string(),
        .valid_to = "9999-12-31 23:59:59"
    });
    return r;
}

}
