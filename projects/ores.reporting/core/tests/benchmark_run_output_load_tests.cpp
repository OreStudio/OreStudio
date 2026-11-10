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
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/process/memory.hpp"
#include "ores.reporting.core/repository/report_metric_repository.hpp"
#include "ores.reporting.core/results/run_output_reader.hpp"
#include "ores.testing/database_helper.hpp"
#include "run_stamp_fixture.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <string>

namespace {

const std::string benchmark_tags("[.][benchmark][run_output_load]");

using ores::reporting::repository::report_metric_repository;
using ores::reporting::results::output_file;
using ores::reporting::results::read_run_output;
using ores::reporting::results::read_run_outputs;
using ores::reporting::results::run_output_outcome;
using ores::reporting::results::run_output_sinks;
using ores::reporting::tests::a_run_stamp;

std::size_t rows_wanted() {
    const auto text = ores::platform::environment::environment::get_value("ORES_BENCH_ROWS");
    return text ? std::stoul(*text) : 500000;
}

/**
 * @brief A raw cube as a large run writes it: one row for each netting set,
 * date and sample.
 */
std::string raw_cube(std::size_t rows) {
    std::string csv = "#Id,NettingSet,DateIndex,Date,Sample,Depth,Value\n";
    csv.reserve(rows * 64);
    for (std::size_t i = 0; i < rows; ++i) {
        const auto date = i % 120;
        csv += "Swap_" + std::to_string(i % 50) + ",CPTY_A," + std::to_string(date) +
               ",2026-11-02," + std::to_string(i / 6000) + ",0," +
               std::to_string(1000.0 + static_cast<double>(i % 997) * 0.25) + "\n";
    }
    return csv;
}

long peak_mib() {
    return static_cast<long>(ores::platform::process::peak_resident_bytes() / (1024 * 1024));
}

}

TEST_CASE("load a large cube whole: read every row, then insert", benchmark_tags) {
    ores::testing::database_helper h;
    const auto rows = rows_wanted();
    auto body = raw_cube(rows);
    const auto before = peak_mib();

    const auto start = std::chrono::steady_clock::now();
    auto outcome =
        read_run_outputs({output_file{.name = "rawcube.csv", .body = std::move(body)}},
                         a_run_stamp(std::chrono::sys_days{std::chrono::year{2026} / 11 / 2}));
    REQUIRE(outcome.refusals.empty());
    report_metric_repository{}.insert(h.context(), outcome.metrics);
    const auto seconds =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();

    std::cout << "whole: rows=" << outcome.metrics.size() << " seconds=" << seconds
              << " peak_mib=" << peak_mib() << " peak_mib_before_load=" << before << "\n";
    CHECK(outcome.metrics.size() == rows);
}

TEST_CASE("load a large cube in blocks: read a block, insert it, read the next", benchmark_tags) {
    ores::testing::database_helper h;
    const auto rows = rows_wanted();
    const auto body = raw_cube(rows);
    const auto before = peak_mib();

    const auto start = std::chrono::steady_clock::now();
    ores::database::repository::unit_of_work uow(h.context());
    std::size_t loaded = 0;
    std::size_t blocks = 0;
    const run_output_sinks sinks{
        .metrics =
            [&](std::vector<ores::reporting::domain::report_metric>& block) {
                report_metric_repository{}.insert(uow.ctx(), block);
                loaded += block.size();
                ++blocks;
            },
        .cashflows = [](auto&) {},
        .text_values =
            [](auto&) {
            }};
    run_output_outcome outcome;
    read_run_output(output_file{.name = "rawcube.csv", .body = body},
                    a_run_stamp(std::chrono::sys_days{std::chrono::year{2026} / 11 / 2}),
                    10000,
                    sinks,
                    outcome);
    REQUIRE(outcome.refusals.empty());
    uow.commit();
    const auto seconds =
        std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();

    std::cout << "blocks: rows=" << loaded << " blocks=" << blocks << " seconds=" << seconds
              << " peak_mib=" << peak_mib() << " peak_mib_before_load=" << before << "\n";
    CHECK(loaded == rows);
}
