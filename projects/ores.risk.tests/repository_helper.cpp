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
#include "ores.risk.tests/repository_helper.hpp"

#include "faker-cxx/faker.h" // IWYU pragma: keep.

namespace ores::risk::tests {

using risk::domain::currency;

risk::domain::currency repository_helper::
create_test_currency(const std::string& iso_code) {
    currency ccy;
    ccy.iso_code = iso_code;
    ccy.name = std::string(faker::finance::currencyName());
    ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
    ccy.symbol = std::string(faker::finance::currencySymbol());
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Fiat";
    ccy.modified_by = std::string(faker::internet::username());
    ccy.valid_from = "";
    ccy.valid_to = "";
    return ccy;
}

void repository_helper::cleanup_database() {
    truncate_table("oresdb.currencies");
}

}
