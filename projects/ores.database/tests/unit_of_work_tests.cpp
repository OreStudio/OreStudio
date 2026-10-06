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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string test_suite("ores.database.tests");
const std::string tags("[unit_of_work]");

struct ip2country_probe {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_geo_ip2country_tbl";

    std::string ip_range;
    std::string tenant_id;
    std::string country_code;
};

}

using ores::database::repository::execute_raw_multi_column_query;
using ores::database::repository::unit_of_work;

TEST_CASE("a unit of work binds its transaction to the context it hands out", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    REQUIRE_FALSE(h.context().active_transaction().has_value());

    unit_of_work uow(h.context());
    REQUIRE(uow.ctx().active_transaction().has_value());
    uow.commit();
}

TEST_CASE("a unit of work that is not committed frees its connection", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    {
        unit_of_work uow(h.context());
        REQUIRE(uow.ctx().active_transaction().has_value());
    }

    // The destructor rolled the transaction back, so the next acquisition
    // finds a usable connection rather than one left in a transaction.
    const auto rows = execute_raw_multi_column_query(h.context(), "select 1", lg, "probe");
    REQUIRE(rows.size() == 1);
}

TEST_CASE("a raw insert through a unit of work is rolled back", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto sql = "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) "
                     "values ('[1,2)', '" +
                     h.context().tenant_id().to_string() + "', '" + code + "')";

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        REQUIRE((*ctx.active_transaction())->execute(sql).has_value());
    }

    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = execute_raw_multi_column_query(
        fresh,
        "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "'",
        lg,
        "probe");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0].value_or("?") == "0");
}

TEST_CASE("a raw insert through a committed unit of work is kept", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto sql = "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) "
                     "values ('[1,2)', '" +
                     h.context().tenant_id().to_string() + "', '" + code + "')";

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        REQUIRE((*ctx.active_transaction())->execute(sql).has_value());
        uow.commit();
    }

    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = execute_raw_multi_column_query(
        fresh,
        "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "'",
        lg,
        "probe");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0].value_or("?") == "1");
}

TEST_CASE("an entity insert through a unit of work is rolled back", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        const ip2country_probe probe{.ip_range = "[1,2)",
                                     .tenant_id = h.context().tenant_id().to_string(),
                                     .country_code = code};
        ores::database::repository::execute_write_op(
            ctx, sqlgen::insert(probe), lg, "Probing an entity insert");
    }

    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = execute_raw_multi_column_query(
        fresh,
        "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "'",
        lg,
        "probe");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0].value_or("?") == "0");
}

TEST_CASE("a failed insert leaves no earlier row in the unit of work", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto tid = h.context().tenant_id().to_string();

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        const ip2country_probe good{
            .ip_range = "[1,2)", .tenant_id = tid, .country_code = code + "G"};
        ores::database::repository::execute_write_op(ctx, sqlgen::insert(good), lg, "Good insert");
        const ip2country_probe bad{
            .ip_range = "not-a-range", .tenant_id = tid, .country_code = code + "B"};
        CHECK_THROWS(ores::database::repository::execute_write_op(
            ctx, sqlgen::insert(bad), lg, "Bad insert"));
    }

    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = execute_raw_multi_column_query(
        fresh,
        "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "G'",
        lg,
        "probe");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0].value_or("?") == "0");
}

/**
 * @brief Records a defect in sqlgen, not a property of the unit of work.
 *
 * sqlgen's postgres iterator wraps every read in its own transaction and ends
 * it with END, which PostgreSQL reads as COMMIT. A read inside a unit of work
 * therefore commits it. This test pins that behaviour so the day an overlay
 * for sqlgen lands, the failure here is the signal to invert the assertion and
 * drop the read-free claims the booking service states.
 */
TEST_CASE("a read inside a unit of work commits the transaction", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto tid = h.context().tenant_id().to_string();

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        using namespace sqlgen::literals;
        const auto query =
            sqlgen::read<std::vector<ip2country_probe>> | sqlgen::where("country_code"_c == code);
        const auto found =
            ores::database::repository::execute_read_query<ip2country_probe, ip2country_probe>(
                ctx, query, [](const auto& v) { return v; }, lg, "probe read");
        CHECK(found.empty());

        const ip2country_probe probe{.ip_range = "[1,2)", .tenant_id = tid, .country_code = code};
        ores::database::repository::execute_write_op(ctx, sqlgen::insert(probe), lg, "insert");
    }

    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = execute_raw_multi_column_query(
        fresh,
        "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "'",
        lg,
        "probe");
    REQUIRE(rows.size() == 1);
    CHECK(rows[0][0].value_or("?") == "1");
}
