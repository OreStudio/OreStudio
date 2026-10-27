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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/pq/store_error.hpp"
#include "ores.database/pq/unit_of_work.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using namespace ores::database::pq;
using std::chrono::system_clock;

namespace {

connection_options options_of(ores::testing::scoped_database_helper& h) {
    const auto& c = h.context().credentials();
    return {.host = c.host,
            .port = static_cast<std::uint16_t>(c.port),
            .dbname = c.dbname,
            .user = c.user,
            .password = c.password,
            .application_name = "ores.database.tests.pq"};
}

session_context context_of(ores::testing::scoped_database_helper& h) {
    return {.tenant = h.tenant_id(), .actor = h.db_user(), .service = h.db_user()};
}

ores::utility::uuid::tenant_id random_tenant() {
    return *ores::utility::uuid::tenant_id::from_uuid(boost::uuids::random_generator()());
}

template <typename T>
T echo(pool& p, const session_context& ctx, std::string_view type, const T& v) {
    const std::string name = "pq_echo_" + std::string(type);
    const std::string sql = "select $1::" + std::string(type);
    const statement<std::tuple<T>, T> s{name, sql};
    return transact(p, ctx, [&](transaction& tx) { return std::get<0>(tx.query(s, v).at(0)); });
}

ores::utility::decimal::decimal dec(std::string_view text) {
    return *ores::utility::decimal::decimal::from_string(text);
}

const statement<std::tuple<>> create_probe{
    "pq_create_probe",
    "create temp table if not exists pq_probe (tenant uuid not null, label text not null)"};
const statement<std::tuple<>> protect_probe{
    "pq_protect_probe", "alter table pq_probe enable row level security, force row level security"};
const statement<std::tuple<>> probe_policy{
    "pq_probe_policy",
    "create policy pq_probe_tenant on pq_probe"
    " using (tenant = current_setting('app.current_tenant_id')::uuid)"};
const statement<std::tuple<>, ores::utility::uuid::tenant_id, std::string> insert_probe{
    "pq_insert_probe", "insert into pq_probe (tenant, label) values ($1, $2)"};
const statement<std::tuple<std::int64_t>> count_probe{"pq_count_probe",
                                                      "select count(*) from pq_probe"};
const statement<std::tuple<std::optional<std::string>>> current_party{
    "pq_current_party", "select nullif(current_setting('app.current_party_id', true), '')"};
const statement<std::tuple<std::string>> current_tenant{
    "pq_current_tenant", "select current_setting('app.current_tenant_id', true)"};

}

namespace {

/*
 * The tenant and party a connection shows outside any transaction: the role's
 * defaults, read on the connection itself rather than through the layer.
 */
std::pair<std::string, std::string> settings_between(pool& p) {
    const auto lease = p.acquire();
    const result r(PQexec(lease.get().native(),
                          "select current_setting('app.current_tenant_id', true),"
                          " current_setting('app.current_party_id', true)"));
    REQUIRE(r.status() == PGRES_TUPLES_OK);
    return {PQgetvalue(r.native(), 0, 0), PQgetvalue(r.native(), 0, 1)};
}

}

TEST_CASE("a sqlstate's class says whether a retry can succeed", "[pq]") {
    CHECK(classify("08006") == error_kind::retryable);
    CHECK(classify("40001") == error_kind::retryable);
    CHECK(classify("40P01") == error_kind::retryable);
    CHECK(classify("53300") == error_kind::retryable);
    CHECK(classify("57014") == error_kind::retryable);
    CHECK(classify("55P03") == error_kind::retryable);
    CHECK(classify("55000") == error_kind::refused);
    CHECK(classify("23505") == error_kind::refused);
    CHECK(classify("22012") == error_kind::refused);
    CHECK(classify("42501") == error_kind::refused);
    CHECK(classify("P0001") == error_kind::refused);
    CHECK(classify("") == error_kind::refused);
}

TEST_CASE("every codec carries its value through the server unchanged", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const auto ctx = context_of(h);

    CHECK(echo(p, ctx, "bool", true));
    CHECK(echo<std::int16_t>(p, ctx, "int2", -12345) == -12345);
    CHECK(echo<std::int32_t>(p, ctx, "int4", -2'000'000'000) == -2'000'000'000);
    CHECK(echo<std::int64_t>(p, ctx, "int8", 9'000'000'000'000) == 9'000'000'000'000);
    CHECK(echo(p, ctx, "float8", 0.1) == 0.1);
    CHECK(echo<std::string>(p, ctx, "text", "zürich's \"quote\"") == "zürich's \"quote\"");
    const auto id = boost::uuids::random_generator()();
    CHECK(echo(p, ctx, "uuid", id) == id);
    const auto tenant = random_tenant();
    CHECK(echo(p, ctx, "uuid", tenant) == tenant);

    const auto now = std::chrono::time_point_cast<std::chrono::microseconds>(system_clock::now());
    CHECK(echo<system_clock::time_point>(p, ctx, "timestamptz", now) == now);
    using namespace std::chrono;
    CHECK(echo(p, ctx, "date", year_month_day(year{1999} / December / 31)) ==
          year_month_day(year{1999} / December / 31));
    CHECK(echo(p, ctx, "date", year_month_day(year{2026} / October / 27)) ==
          year_month_day(year{2026} / October / 27));

    for (const auto text : {"0",
                            "1",
                            "-1.5",
                            "10000",
                            "0.0001",
                            "123456789.000123",
                            "-0.000000001",
                            "99999999999999999999.99"})
        CHECK(echo(p, ctx, "numeric", dec(text)).to_string() == dec(text).to_string());

    const std::vector<boost::uuids::uuid> ids{id, boost::uuids::random_generator()()};
    CHECK(echo(p, ctx, "uuid[]", ids) == ids);
    CHECK(echo(p, ctx, "text[]", std::vector<std::string>{"a", "", "c"}) ==
          std::vector<std::string>{"a", "", "c"});
    CHECK(echo(p, ctx, "int8[]", std::vector<std::int64_t>{}).empty());
    CHECK(echo(p, ctx, "text", std::optional<std::string>()) == std::nullopt);
}

TEST_CASE("the maximum time point is the store's never", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const statement<std::tuple<bool>, system_clock::time_point> is_never{
        "pq_is_never", "select $1 = ores_utility_infinity_timestamp_fn()"};
    const statement<std::tuple<system_clock::time_point>> never{
        "pq_never", "select ores_utility_infinity_timestamp_fn()"};
    transact(p, context_of(h), [&](transaction& tx) {
        CHECK(std::get<0>(tx.query(is_never, system_clock::time_point::max()).at(0)));
        CHECK(std::get<0>(tx.query(never).at(0)) == system_clock::time_point::max());
    });
}

TEST_CASE("a statement is prepared once on a connection and run many times", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const statement<std::tuple<std::int32_t>, std::int32_t> twice{"pq_twice", "select $1 * 2"};
    for (std::int32_t i = 0; i < 3; ++i) {
        const auto doubled =
            transact(p, context_of(h), [&](transaction& tx) { return tx.query(twice, i); });
        CHECK(std::get<0>(doubled.at(0)) == 2 * i);
    }
    CHECK(twice.counters().prepares.load() == 1);
    CHECK(twice.counters().executions.load() == 3);
}

TEST_CASE("one connection carries no tenant or party from one transaction to the next", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const auto fresh = settings_between(p);
    auto first = context_of(h);
    first.tenant = random_tenant();
    first.party = boost::uuids::random_generator()();
    auto second = context_of(h);
    second.tenant = random_tenant();

    transact(p, first, [&](transaction& tx) {
        tx.execute(create_probe);
        tx.execute(protect_probe);
        tx.execute(probe_policy);
        tx.execute(insert_probe, first.tenant, std::string("first"));
        CHECK(std::get<0>(tx.query(current_party).at(0)) == boost::uuids::to_string(*first.party));
    });
    transact(p, second, [&](transaction& tx) {
        CHECK(std::get<0>(tx.query(current_tenant).at(0)) == second.tenant.to_string());
        CHECK_FALSE(std::get<0>(tx.query(current_party).at(0)).has_value());
        CHECK(std::get<0>(tx.query(count_probe).at(0)) == 0);
    });
    transact(
        p, first, [&](transaction& tx) { CHECK(std::get<0>(tx.query(count_probe).at(0)) == 1); });

    CHECK(settings_between(p) == fresh);
    CHECK(settings_between(p).first != first.tenant.to_string());
}

TEST_CASE("a unit of work commits on return, rolls back on a throw and always gives back", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const auto ctx = context_of(h);
    transact(p, ctx, [&](transaction& tx) { tx.execute(create_probe); });

    CHECK_THROWS_AS(transact(p,
                             ctx,
                             [&](transaction& tx) {
                                 tx.execute(insert_probe, ctx.tenant, std::string("lost"));
                                 throw std::runtime_error("the work failed");
                             }),
                    std::runtime_error);
    CHECK(p.idle() == 1);
    CHECK(transact(p, ctx, [&](transaction& tx) {
              return std::get<0>(tx.query(count_probe).at(0));
          }) == 0);

    transact(p, ctx, [&](transaction& tx) {
        tx.execute(insert_probe, ctx.tenant, std::string("kept"));
    });
    CHECK(p.idle() == 1);
    CHECK(transact(p, ctx, [&](transaction& tx) {
              return std::get<0>(tx.query(count_probe).at(0));
          }) == 1);
}

TEST_CASE("a store error says whether to retry", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const auto ctx = context_of(h);
    const statement<std::tuple<>> serialization{
        "pq_serialization", "do $$ begin raise exception using errcode = '40001'; end $$"};
    const statement<std::tuple<std::int32_t>> divide{"pq_divide", "select 1 / 0"};

    try {
        transact(p, ctx, [&](transaction& tx) { tx.execute(serialization); });
        FAIL("the serialization failure did not throw");
    } catch (const store_error& e) {
        CHECK(e.sqlstate() == "40001");
        CHECK(e.retryable());
    }
    try {
        transact(p, ctx, [&](transaction& tx) { tx.query(divide); });
        FAIL("the division by zero did not throw");
    } catch (const store_error& e) {
        CHECK(e.sqlstate() == "22012");
        CHECK_FALSE(e.retryable());
    }
    CHECK(transact(p, ctx, [&](transaction& tx) {
              return std::get<0>(tx.query(current_tenant).at(0));
          }) == ctx.tenant.to_string());
}

TEST_CASE("a batch goes out in one pipeline and a bulk load through copy", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1);
    const auto ctx = context_of(h);
    transact(p, ctx, [&](transaction& tx) { tx.execute(create_probe); });

    const auto touched = transact(p, ctx, [&](transaction& tx) {
        transaction::batch b;
        for (int i = 0; i < 3; ++i)
            b.add(insert_probe, ctx.tenant, "batched " + std::to_string(i));
        return tx.run(b);
    });
    CHECK(touched == std::vector<std::uint64_t>{1, 1, 1});

    std::vector<std::tuple<ores::utility::uuid::tenant_id, std::string>> rows;
    for (int i = 0; i < 1000; ++i)
        rows.emplace_back(ctx.tenant, "copied " + std::to_string(i));
    const auto copied = transact(
        p, ctx, [&](transaction& tx) { return tx.copy_in("pq_probe", "tenant, label", rows); });
    CHECK(copied == 1000);
    CHECK(transact(p, ctx, [&](transaction& tx) {
              return std::get<0>(tx.query(count_probe).at(0));
          }) == 1003);
}

TEST_CASE("a pool that stays busy refuses with a retryable error", "[pq]") {
    ores::testing::scoped_database_helper h;
    pool p(options_of(h), 1, std::chrono::milliseconds(20));
    const auto held = p.acquire();
    try {
        (void)p.acquire();
        FAIL("the second acquire did not throw");
    } catch (const store_error& e) {
        CHECK(e.retryable());
    }
}
