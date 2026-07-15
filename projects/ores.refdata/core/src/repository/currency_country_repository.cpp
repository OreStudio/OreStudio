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
#include "ores.refdata.core/repository/currency_country_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_country_entity.hpp"
#include "ores.refdata.core/repository/currency_country_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_country_repository::sql() {
    return generate_create_table_sql<currency_country_entity>(lg());
}

currency_country_repository::currency_country_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void currency_country_repository::write(const domain::currency_country& currency_country) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency country to database: "
                               << currency_country.currency_iso_code << "/"
                               << currency_country.country_alpha2_code;
    execute_write_query(ctx_,
                        currency_country_mapper::map(currency_country),
                        lg(),
                        "writing currency country to database");
}

void currency_country_repository::write(
    const std::vector<domain::currency_country>& currency_countries) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency countries to database. Count: "
                               << currency_countries.size();
    execute_write_query(ctx_,
                        currency_country_mapper::map(currency_countries),
                        lg(),
                        "writing currency countries to database");
}

std::vector<domain::currency_country> currency_country_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c, "country_alpha2_code"_c);

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries");
}

std::vector<domain::currency_country>
currency_country_repository::read_latest_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries. Currency: "
                               << currency_iso_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "valid_to"_c == max.value()) |
                       order_by("country_alpha2_code"_c);

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries by currency.");
}

std::vector<domain::currency_country>
currency_country_repository::read_latest_by_country(const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency countries. Country: " << alpha2_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_country_entity>> |
                       where("tenant_id"_c == tid && "country_alpha2_code"_c == alpha2_code &&
                             "valid_to"_c == max.value()) |
                       order_by("currency_iso_code"_c);

    return execute_read_query<currency_country_entity, domain::currency_country>(
        ctx_,
        query,
        [](const auto& entities) { return currency_country_mapper::map(entities); },
        lg(),
        "Reading latest currency countries by country.");
}

void currency_country_repository::remove(const std::string& currency_iso_code,
                                         const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency country from database: " << currency_iso_code
                               << "/" << alpha2_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_country_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code &&
                             "country_alpha2_code"_c == alpha2_code);

    execute_delete_query(ctx_, query, lg(), "removing currency country from database");
}

void currency_country_repository::remove_by_currency(const std::string& currency_iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency countries from database: "
                               << currency_iso_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_country_entity> |
                       where("tenant_id"_c == tid && "currency_iso_code"_c == currency_iso_code);

    execute_delete_query(ctx_, query, lg(), "removing all currency countries from database");
}

}
