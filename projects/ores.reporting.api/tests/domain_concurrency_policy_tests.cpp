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
#include "ores.reporting.api/domain/concurrency_policy.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/concurrency_policy_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/concurrency_policy_table.hpp"
#include "ores.reporting.api/domain/concurrency_policy_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::reporting::domain::concurrency_policy;

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[domain]");

concurrency_policy make_concurrency_policy(const std::string& code,
    const std::string& name, int display_order) {
    concurrency_policy cp;
    cp.version = 1;
    cp.code = code;
    cp.name = name;
    cp.description = name + " concurrency policy";
    cp.display_order = display_order;
    cp.modified_by = "system";
    cp.performed_by = "system";
    cp.change_reason_code = "system.new";
    cp.change_commentary = "Test data";
    cp.recorded_at = std::chrono::system_clock::now();
    return cp;
}

}

using ores::reporting::domain::concurrency_policy;
using namespace ores::logging;

TEST_CASE("create_concurrency_policy_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    concurrency_policy sut;
    sut.version = 1;
    sut.code = "skip";
    sut.name = "Skip";
    sut.description = "Skip new trigger if instance is running";
    sut.display_order = 1;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Concurrency policy: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "skip");
    CHECK(sut.name == "Skip");
    CHECK(sut.display_order == 1);
    CHECK(sut.modified_by == "system");
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("concurrency_policy_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    concurrency_policy sut;
    sut.version = 1;
    sut.code = "queue";
    sut.name = "Queue";
    sut.description = "Queue trigger if instance is running";
    sut.display_order = 2;
    sut.modified_by = "system";
    sut.performed_by = "system";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Test";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Concurrency policy: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("queue") != std::string::npos);
}

TEST_CASE("create_concurrency_policy_with_faker", tags) {
    auto lg(make_logger(test_suite));

    concurrency_policy sut;
    sut.version = faker::number::integer(1, 10);
    sut.code = std::string(faker::word::noun()) + "_policy";
    sut.name = std::string(faker::word::adjective());
    sut.description = std::string(faker::lorem::sentence());
    sut.display_order = faker::number::integer(1, 100);
    sut.modified_by = std::string(faker::internet::username());
    sut.performed_by = std::string(faker::internet::username());
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Synthetic test data";
    sut.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Concurrency policy: " << sut;

    CHECK(sut.version >= 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_concurrency_policies", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::tuple<std::string, std::string, int>> policies = {
        {"skip", "Skip", 1}, {"queue", "Queue", 2}, {"fail", "Fail", 3}};
    for (const auto& [code, name, order] : policies) {
        auto sut = make_concurrency_policy(code, name, order);
        BOOST_LOG_SEV(lg, info) << "Concurrency policy: " << sut;
        CHECK(!sut.code.empty());
        CHECK(sut.version == 1);
    }
}

TEST_CASE("concurrency_policy_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<concurrency_policy> items = {
        make_concurrency_policy("fail", "Fail", 3)};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("fail") != std::string::npos);
}

TEST_CASE("concurrency_policy_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<concurrency_policy> items;
    const std::vector<std::string> codes = {"skip", "queue", "fail"};
    for (int i = 0; i < 3; ++i)
        items.push_back(make_concurrency_policy(codes[i], codes[i], i + 1));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("skip") != std::string::npos);
    CHECK(table.find("queue") != std::string::npos);
    CHECK(table.find("fail") != std::string::npos);
}

TEST_CASE("concurrency_policy_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<concurrency_policy> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("concurrency_policy_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<concurrency_policy> items;
    for (int i = 0; i < 5; ++i) {
        concurrency_policy cp;
        cp.version = 1;
        cp.code = std::string(faker::word::noun()) + "_" + std::to_string(i);
        cp.name = std::string(faker::word::adjective());
        cp.description = std::string(faker::lorem::sentence());
        cp.display_order = i + 1;
        cp.modified_by = "system";
        cp.performed_by = "system";
        cp.change_reason_code = "system.new";
        cp.change_commentary = "Test";
        cp.recorded_at = std::chrono::system_clock::now();
        items.push_back(cp);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& item : items)
        CHECK(table.find(item.code) != std::string::npos);
}
