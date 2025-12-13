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
#include "ores.risk/repository/currency_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.risk/domain/currency_json_io.hpp" // IWYU pragma: keep.

namespace ores::risk::repository {

using namespace ores::utility::log;
using namespace ores::database::repository;

domain::currency currency_mapper::map(const currency_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::currency r;
    r.version = v.version;
    r.iso_code = v.iso_code.value();
    r.name = v.name;
    r.numeric_code = v.numeric_code;
    r.symbol = v.symbol;
    r.fraction_symbol = v.fraction_symbol;
    r.fractions_per_unit = v.fractions_per_unit;
    r.rounding_type = v.rounding_type;
    r.rounding_precision = v.rounding_precision;
    r.format = v.format;
    r.currency_type = v.currency_type;
    r.modified_by = v.modified_by;
    r.valid_from = v.valid_from.has_value() ? v.valid_from->str() : "";
    r.valid_to = v.valid_to.has_value() ? v.valid_from->str() : "";

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}
currency_entity currency_mapper::map(const domain::currency& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    currency_entity r;
    r.iso_code = v.iso_code;
    r.version = v.version;
    r.name = v.name;
    r.numeric_code = v.numeric_code;
    r.symbol = v.symbol;
    r.fraction_symbol = v.fraction_symbol;
    r.fractions_per_unit = v.fractions_per_unit;
    r.rounding_type = v.rounding_type;
    r.rounding_precision = v.rounding_precision;
    r.format = v.format;
    r.currency_type = v.currency_type;
    r.modified_by = v.modified_by;

    if (!v.valid_from.empty())
        r.valid_from = v.valid_from;

    if (!v.valid_to.empty())
        r.valid_to = v.valid_to;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::currency>
currency_mapper::map(const std::vector<currency_entity>& v) {
    return map_vector<currency_entity, domain::currency>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<currency_entity>
currency_mapper::map(const std::vector<domain::currency>& v) {
    return map_vector<domain::currency, currency_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
