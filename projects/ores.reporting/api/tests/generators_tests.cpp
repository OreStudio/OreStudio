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
#include "ores.reporting.api/generators/concurrency_policy_generator.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.api/generators/report_type_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[generators]");

}

using namespace ores::reporting::generators;
using namespace ores::logging;
using ores::utility::generation::generation_context;

// --- concurrency_policy ---

TEST_CASE("concurrency_policy_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_concurrency_policy(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated concurrency_policy code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("concurrency_policy_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_concurrency_policies(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- report_type ---

TEST_CASE("report_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_report_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated report_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("report_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_report_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- report_definition ---

TEST_CASE("report_definition_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_report_definition(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated report_definition id: " << sut.id;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("report_definition_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_report_definitions(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}

// --- report_instance ---

TEST_CASE("report_instance_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_report_instance(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated report_instance id: " << sut.id;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("report_instance_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_report_instances(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}
