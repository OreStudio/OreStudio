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
#include "ores.database/domain/context_transact.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

struct ip2country_probe {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_geo_ip2country_tbl";

    std::string ip_range;
    std::string tenant_id;
    std::string country_code;
};

/**
 * @brief A range no other probe in this suite uses.
 *
 * The store keys on its range, and every case here writes a probe into the same
 * table under a fresh random country code. A range fixed in the source would
 * collide with whichever case ran before it, so the range is drawn from the same
 * place the code is.
 */
std::string probe_range() {
    const auto hex = boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 12);
    const auto low = std::stoll(hex, nullptr, 16);
    return "[" + std::to_string(low) + "," + std::to_string(low + 1) + ")";
}

std::string probe_code() {
    return "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
}

std::string probe_insert(const ores::database::context& ctx, const std::string& code) {
    return "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) "
           "values ('" +
           probe_range() + "', '" + ctx.tenant_id().to_string() + "', '" + code + "')";
}

std::string probe_count(const std::string& code) {
    return "select count(*) from ores_geo_ip2country_tbl where country_code = '" + code + "'";
}

std::string committed_count(const std::string& code) {
    auto lg(ores::logging::make_logger(test_suite));
    auto fresh = ores::testing::test_database_manager::make_context();
    const auto rows = ores::database::repository::execute_raw_multi_column_query(
        fresh, probe_count(code), lg, "probe");
    return rows.at(0).at(0).value_or("?");
}

void pq_execute(const ores::database::context& ctx, const std::string& sql) {
    const ores::database::pq::text_statement s({}, sql);
    ores::database::transact(ctx, [&](ores::database::pq::transaction& tx) { tx.run(s, {}); });
}

}

using ores::database::repository::execute_raw_multi_column_query;
using ores::database::repository::unit_of_work;

TEST_CASE("a unit of work binds its transaction to the context it hands out", "[unit_of_work]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    REQUIRE_FALSE(h.context().active_transaction().has_value());

    unit_of_work uow(h.context());
    REQUIRE(uow.ctx().active_transaction().has_value());
    uow.commit();
}

TEST_CASE("a unit of work that is not committed frees its connection", "[unit_of_work]") {
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

TEST_CASE("a raw insert through a unit of work is rolled back", "[unit_of_work]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto sql = "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) "
                     "values ('" +
                     probe_range() + "', '" + h.context().tenant_id().to_string() + "', '" + code +
                     "')";

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

TEST_CASE("a raw insert through a committed unit of work is kept", "[unit_of_work]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto sql = "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) "
                     "values ('" +
                     probe_range() + "', '" + h.context().tenant_id().to_string() + "', '" + code +
                     "')";

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

TEST_CASE("an entity insert through a unit of work is rolled back", "[unit_of_work]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        const ip2country_probe probe{.ip_range = probe_range(),
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

TEST_CASE("a failed insert leaves no earlier row in the unit of work", "[unit_of_work]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto code =
        "UOW" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    const auto tid = h.context().tenant_id().to_string();

    {
        unit_of_work uow(h.context());
        const auto& ctx = uow.ctx();
        const ip2country_probe good{
            .ip_range = probe_range(), .tenant_id = tid, .country_code = code + "G"};
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
 * @brief A read joins the unit of work's transaction without ending it.
 *
 * The read runs through sqlgen's postgres iterator, which the overlay patch
 * stops from running its own BEGIN and END around a transaction it joined. The
 * insert that follows the read is therefore still part of the unit of work,
 * and destroying the unit of work without commit rolls it back.
 */
TEST_CASE("a read inside a unit of work does not commit it", "[unit_of_work]") {
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

        const ip2country_probe probe{
            .ip_range = probe_range(), .tenant_id = tid, .country_code = code};
        ores::database::repository::execute_write_op(ctx, sqlgen::insert(probe), lg, "insert");
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

TEST_CASE("a libpq write inside a unit of work is rolled back with it", "[unit_of_work]") {
    ores::testing::scoped_database_helper h;
    const auto code = probe_code();

    {
        unit_of_work uow(h.context());
        pq_execute(uow.ctx(), probe_insert(uow.ctx(), code));
    }

    CHECK(committed_count(code) == "0");
}

TEST_CASE("a libpq write inside a unit of work is kept when it commits", "[unit_of_work]") {
    ores::testing::scoped_database_helper h;
    const auto code = probe_code();

    {
        unit_of_work uow(h.context());
        pq_execute(uow.ctx(), probe_insert(uow.ctx(), code));
        uow.commit();
    }

    CHECK(committed_count(code) == "1");
}

TEST_CASE("a libpq read inside a unit of work sees its uncommitted sqlgen write",
          "[unit_of_work]") {
    ores::testing::scoped_database_helper h;
    const auto code = probe_code();

    unit_of_work uow(h.context());
    const auto& ctx = uow.ctx();
    REQUIRE((*ctx.active_transaction())->execute(probe_insert(ctx, code)).has_value());

    const auto sql = probe_count(code);
    const ores::database::pq::text_statement count({}, sql);
    const auto seen = ores::database::transact(ctx, [&](ores::database::pq::transaction& tx) {
        return std::string(tx.run(count, {}).text(0, 0));
    });

    CHECK(seen == "1");
    CHECK(committed_count(code) == "0");
}

TEST_CASE("a named libpq statement runs in every call that joins a unit of work",
          "[unit_of_work]") {
    ores::testing::scoped_database_helper h;
    const ores::database::pq::text_statement probe("ores_uow_join_probe", "select 1");

    unit_of_work uow(h.context());
    for (int i = 0; i < 2; ++i) {
        const auto one =
            ores::database::transact(uow.ctx(), [&](ores::database::pq::transaction& tx) {
                return std::string(tx.run(probe, {}).text(0, 0));
            });
        CHECK(one == "1");
    }
    uow.commit();
}
