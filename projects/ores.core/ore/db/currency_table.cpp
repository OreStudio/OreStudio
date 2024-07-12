/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <sstream>
#include <iostream>
#include <pqxx/pqxx>
#include "ores.utility/log/logger.hpp"
#include "ores.core/ore/db/currency_table.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.core.ore.db.currency_table"));

}

namespace ores::core::ore::db {

void currency_table::write(const std::vector<model::currency>& currencies) {
    // FIXME: test
    std::string connection_string("postgresql://ores:ores@localhost:5433/oresdb");
    pqxx::connection c(connection_string);
    pqxx::work w(c);

    std::ostringstream query;
    query << "insert into oresdb.currencies "
          << "(name, iso_code, numeric_code, symbol, fraction_symbol, "
          << "fractions_per_unit, rounding_type, rounding_precision, "
          << "format, currency_type)"
          << "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)";
    c.prepare("insert_to_currencies", query.str());

    for(const auto& ccy : currencies) {
        w.exec_prepared("insert_to_currencies", ccy.name(), ccy.iso_code(),
            ccy.numeric_code(), ccy.symbol(), ccy.fraction_symbol(),
            ccy.fractions_per_unit(), ccy.rounding_type(), ccy.rounding_precision(), ccy.format(),
            ccy.currency_type());
    }
    w.commit();
}

std::vector<model::currency> currency_table::read() {
    std::string connection_string("postgresql://ores:ores@localhost:5433/oresdb");
    pqxx::connection c(connection_string);
    pqxx::work w(c);

    std::ostringstream query;
    query << "select "
          << "name, iso_code, numeric_code, symbol, fraction_symbol, "
          << "fractions_per_unit, rounding_type, rounding_precision, "
          << "format, currency_type "
          << "from oresdb.currencies;";

    std::vector<model::currency> r;
    for (auto [name, iso_code, numeric_code, symbol, fraction_symbol,
            fractions_per_unit, rounding_type, rounding_precision,
            format, currency_type] : w.query<std::string, std::string,
             int, std::string, std::string, int, std::string, int, std::string,
             std::string>(query.str())) {
        model::currency ccy;
        ccy.name(name);
        ccy.iso_code(iso_code);
        ccy.numeric_code(numeric_code);
        ccy.symbol(symbol);
        ccy.fraction_symbol(fraction_symbol);
        ccy.fractions_per_unit(fractions_per_unit);
        ccy.rounding_type(rounding_type);
        ccy.rounding_precision(rounding_precision);
        ccy.format(format);
        ccy.currency_type(currency_type);
        r.push_back(ccy);
    }
    return r;
}

}
