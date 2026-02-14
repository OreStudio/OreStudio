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
#include "ores.dq/domain/dataset_bundle.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/datetime.hpp"
#include "ores.dq/domain/dataset_bundle_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_bundle_table.hpp"
#include "ores.dq/domain/dataset_bundle_table_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/dataset_bundle_generator.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

using ores::utility::faker::datetime;
using ores::utility::generation::generation_context;

}

using ores::dq::domain::dataset_bundle;
using namespace ores::logging;

TEST_CASE("create_dataset_bundle_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    dataset_bundle sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.code = "slovaris";
    sut.name = "Slovaris Bundle";
    sut.description = "Synthetic reference data for development and testing";
    sut.modified_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 1);

    BOOST_LOG_SEV(lg, info) << "Dataset bundle: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "slovaris");
    CHECK(sut.name == "Slovaris Bundle");
    CHECK(sut.description == "Synthetic reference data for development and testing");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("dataset_bundle_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    dataset_bundle db;
    db.version = 1;
    db.id = boost::uuids::random_generator()();
    db.code = "base";
    db.name = "Base Bundle";
    db.description = "Industry-standard reference data (ISO + FpML)";
    db.modified_by = "system";
    db.change_reason_code = "system.new";
    db.change_commentary = "Initial setup";
    db.recorded_at = datetime::make_timepoint(2025, 1, 1);

    std::vector<dataset_bundle> bundles = {db};
    auto table = convert_to_table(bundles);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Name"));
    CHECK(table.contains("Code"));
    CHECK(table.contains("Base Bundle"));
    CHECK(table.contains("base"));
}

TEST_CASE("dataset_bundle_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    auto sut = ores::dq::generators::generate_synthetic_dataset_bundle(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated bundle: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("dataset_bundle_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));

    generation_context ctx;
    const std::size_t count = 5;
    auto bundles = ores::dq::generators::generate_synthetic_dataset_bundles(count, ctx);

    BOOST_LOG_SEV(lg, info) << "Generated " << bundles.size() << " bundles";
    BOOST_LOG_SEV(lg, info) << bundles;

    CHECK(bundles.size() == count);
    for (const auto& b : bundles) {
        CHECK(!b.code.empty());
        CHECK(!b.name.empty());
    }
}
