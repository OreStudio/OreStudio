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
#include "ores.database/pq/unit_of_work.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <iostream>

/*
 * The repository benchmark of the analysis story, on the layer: the same table
 * shape, the same 1,000 rows, written in one pipeline, read whole and read one
 * at a time. Hidden from the suite; run it with "[.benchmark]".
 */

using namespace ores::database::pq;
using std::chrono::system_clock;

namespace {

using bench_row = std::tuple<std::string,
                             ores::utility::uuid::tenant_id,
                             std::int32_t,
                             std::string,
                             std::string,
                             std::string,
                             std::string,
                             std::string,
                             system_clock::time_point>;

const statement<std::tuple<>> create_bench{
    "pq_bench_create",
    "create temp table if not exists pq_bench (code text not null, tenant_id uuid not null,"
    " version integer not null, description text not null, modified_by text not null,"
    " performed_by text not null, change_reason_code text not null,"
    " change_commentary text not null, valid_from timestamptz not null,"
    " valid_to timestamptz not null, primary key (tenant_id, code, valid_from, valid_to))"};
const statement<std::tuple<>> index_bench{
    "pq_bench_index", "create index if not exists pq_bench_code on pq_bench (code, valid_to)"};
const statement<std::tuple<>> truncate_bench{"pq_bench_truncate", "truncate pq_bench"};
const statement<std::tuple<>,
                std::string,
                ores::utility::uuid::tenant_id,
                std::int32_t,
                std::string,
                std::string,
                std::string,
                std::string,
                std::string,
                system_clock::time_point,
                system_clock::time_point>
    insert_bench{"pq_bench_insert",
                 "insert into pq_bench values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)"};
const statement<bench_row, system_clock::time_point> read_all{
    "pq_bench_read_all",
    "select code, tenant_id, version, description, modified_by, performed_by,"
    " change_reason_code, change_commentary, valid_from from pq_bench"
    " where valid_to = $1 order by code"};
const statement<bench_row, std::string, system_clock::time_point> read_one{
    "pq_bench_read_one",
    "select code, tenant_id, version, description, modified_by, performed_by,"
    " change_reason_code, change_commentary, valid_from from pq_bench"
    " where code = $1 and valid_to = $2"};

template <typename F>
double millis(F&& f) {
    const auto start = std::chrono::steady_clock::now();
    f();
    return std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - start)
        .count();
}

void report(std::string_view what, std::vector<double> samples) {
    std::ranges::sort(samples);
    std::cout << "pq layer: " << what << ": median " << samples[samples.size() / 2] << " ms ("
              << samples.front() << "-" << samples.back() << "), " << samples.size() << " runs\n";
}

}

TEST_CASE("the repository benchmark runs on the layer", "[.benchmark][pq]") {
    ores::testing::scoped_database_helper h;
    auto options = h.context().connection_options();
    options.application_name = "ores.database.tests.pq.benchmark";
    pool p(options, 1);
    const session_context ctx{
        .tenant = h.tenant_id(), .actor = h.db_user(), .service = h.db_user()};
    transact(p, ctx, [&](transaction& tx) {
        tx.execute(create_bench);
        tx.execute(index_bench);
    });

    const auto recorded = system_clock::time_point(std::chrono::seconds(1'767'225'600));
    const auto never = system_clock::time_point::max();
    constexpr int rows = 1000;
    constexpr int runs = 20;
    std::vector<double> writes, reads, points;
    for (int run = 0; run < runs; ++run) {
        transact(p, ctx, [&](transaction& tx) { tx.execute(truncate_bench); });
        writes.push_back(millis([&] {
            transact(p, ctx, [&](transaction& tx) {
                transaction::batch b;
                for (int i = 0; i < rows; ++i)
                    b.add(insert_bench,
                          "CODE_" + std::to_string(i),
                          ctx.tenant,
                          1,
                          std::string("A change reason category for the benchmark"),
                          std::string("bench"),
                          std::string("bench"),
                          std::string("system.test"),
                          std::string("Run ") + std::to_string(run),
                          recorded,
                          never);
                tx.run(b);
            });
        }));
        reads.push_back(millis([&] {
            const auto all =
                transact(p, ctx, [&](transaction& tx) { return tx.query(read_all, never); });
            REQUIRE(all.size() == static_cast<std::size_t>(rows));
        }));
        points.push_back(millis([&] {
            transact(p, ctx, [&](transaction& tx) {
                for (int i = 0; i < rows; ++i)
                    REQUIRE(tx.query(read_one, "CODE_" + std::to_string(i), never).size() == 1);
            });
        }));
    }
    report("write 1000 rows in one pipeline", writes);
    report("read all 1000", reads);
    report("1000 point reads", points);
}
