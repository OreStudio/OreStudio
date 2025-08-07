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
#include <boost/date_time/posix_time/posix_time.hpp>
#include <pqxx/pqxx>
#include "pqxx/params"
#include <pqxx/prepared_statement>
#include "ores.utility/log/logger.hpp"
#include "ores.core/db/currency_table.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.core.db.currency_table"));

}

namespace ores::core::db {

void currency_table::write(const std::vector<types::currency>& currencies) {
    // FIXME: test
    std::string connection_string("postgresql://ores:ahV6aehuij6eingohsiajaiT0@localhost:5434/oresdb");
    pqxx::connection c(connection_string);
    pqxx::work w(c);

    std::ostringstream query;
    query << "select oresdb.currencies_insert"
          << "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)";
    c.prepare("currencies_insert_stmt", query.str());

    for (const auto& ccy : currencies) {
        pqxx::params params {
            ccy.name(), ccy.iso_code(), ccy.numeric_code(), ccy.symbol(), ccy.fraction_symbol(),
                ccy.fractions_per_unit(), ccy.rounding_type(),
                ccy.rounding_precision(), ccy.format(),
                ccy.currency_type()};

        w.exec(pqxx::prepped{"currencies_insert_stmt"}, params);
    }
    w.commit();
}

std::vector<types::currency>
currency_table::read_internal(const std::string& query) {
    BOOST_LOG_SEV(lg, debug) << "Reading using query: " << query;

    std::string connection_string("postgresql://ores:ores@localhost:5432/oresdb");
    pqxx::connection c(connection_string);
    pqxx::work w(c);

    std::vector<types::currency> r;
    for (const auto& [name, iso_code, numeric_code, symbol, fraction_symbol,
            fractions_per_unit, rounding_type, rounding_precision,
            format, currency_type, modified_by, valid_from, valid_to] :
             w.query<std::string, std::string, int, std::string, std::string,
             int, std::string, int, std::string, std::string, std::string,
             std::string, std::string>(query)) {
        types::currency ccy;
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
        ccy.modified_by(modified_by);
        ccy.valid_from(valid_from);
        ccy.valid_to(valid_to);

        r.push_back(ccy);
    }
    return r;
}

std::vector<types::currency> currency_table::
read_latest(const std::string& iso_code) {
    std::ostringstream query;
    query << "select "
          << "name, iso_code, numeric_code, symbol, fraction_symbol, "
          << "fractions_per_unit, rounding_type, rounding_precision, "
          << "format, currency_type, modified_by, valid_from, valid_to "
          << "from oresdb.currencies_latest";

    if (!iso_code.empty()) {
        query << " where iso_code = '" << iso_code << "'";
    }
    query << " order by valid_from;";

    return read_internal(query.str());
}

std::vector<types::currency> currency_table::
read_at_timepoint(const std::string& as_of, const std::string& iso_code) {
    std::ostringstream query;
    query << "select "
          << "name, iso_code, numeric_code, symbol, fraction_symbol, "
          << "fractions_per_unit, rounding_type, rounding_precision, "
          << "format, currency_type, modified_by, valid_from, valid_to "
          << "from oresdb.currencies_as_of('" << as_of << "')";

    if (!iso_code.empty()) {
        query << " where iso_code = '" << iso_code << "'";
    }
    query << " order by valid_from;";

    return read_internal(query.str());
}

std::vector<types::currency> currency_table::
read_all(const std::string& iso_code) {
    std::ostringstream query;
    query << "select "
          << "name, iso_code, numeric_code, symbol, fraction_symbol, "
          << "fractions_per_unit, rounding_type, rounding_precision, "
          << "format, currency_type, modified_by, valid_from, valid_to "
          << "from oresdb.currencies";

    if (!iso_code.empty()) {
        query << " where iso_code = '" << iso_code << "'";
    }

    return read_internal(query.str());

}


}
