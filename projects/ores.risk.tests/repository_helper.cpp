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

#include "faker-cxx/faker.h"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.utility/repository/context_factory.hpp"

namespace ores::risk::tests {

using namespace ores::utility::log;
using risk::domain::currency;
using utility::repository::context;
using utility::repository::context_factory;

repository_helper::repository_helper() : context_(make_context()) {}

context repository_helper::make_context() {
    context_factory::configuration db_cfg{
        .user = "ores",
        .password = "ahV6aehuij6eingohsiajaiT0",
        .host = "localhost",
        .database = "oresdb",
        .port = 5432,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    context ctx = context_factory::make_context(db_cfg);
    BOOST_LOG_SEV(lg(), info) << "Database context created successfully";

    risk::repository::currency_repository repo;
    const auto sql = repo.sql();
    BOOST_LOG_SEV(lg(), debug) << "Table SQL: " << sql;
    return ctx;
}

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
    BOOST_LOG_SEV(lg(), info) << "Cleaning up test database";

    const auto truncate_sql = "TRUNCATE TABLE oresdb.currencies";
    const auto execute_truncate = [&](auto&& session) {
        return session->execute(truncate_sql);
    };

    const auto r = sqlgen::session(context_.connection_pool())
        .and_then(execute_truncate);

    if (!r) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to cleanup database: " << r.error().what();
    } else {
        BOOST_LOG_SEV(lg(), info) << "Successfully cleaned up test database";
    }
}

}
