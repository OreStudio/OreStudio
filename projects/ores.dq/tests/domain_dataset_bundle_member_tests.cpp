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
#include "ores.dq/domain/dataset_bundle_member.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/datetime.hpp"
#include "ores.dq/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_bundle_member_table.hpp"
#include "ores.dq/domain/dataset_bundle_member_table_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/dataset_bundle_member_generator.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

using ores::utility::faker::datetime;

}

using ores::dq::domain::dataset_bundle_member;
using namespace ores::logging;

TEST_CASE("create_dataset_bundle_member_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    dataset_bundle_member sut;
    sut.version = 1;
    sut.bundle_code = "slovaris";
    sut.dataset_code = "slovaris.countries";
    sut.display_order = 1;
    sut.recorded_by = "admin";
    sut.change_reason_code = "system.new";
    sut.change_commentary = "Initial creation";
    sut.recorded_at = datetime::make_timepoint(2025, 1, 1);

    BOOST_LOG_SEV(lg, info) << "Dataset bundle member: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.bundle_code == "slovaris");
    CHECK(sut.dataset_code == "slovaris.countries");
    CHECK(sut.display_order == 1);
    CHECK(sut.recorded_by == "admin");
    CHECK(sut.change_reason_code == "system.new");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("dataset_bundle_member_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    dataset_bundle_member dbm;
    dbm.version = 1;
    dbm.bundle_code = "base";
    dbm.dataset_code = "iso.currencies";
    dbm.display_order = 10;
    dbm.recorded_by = "system";
    dbm.change_reason_code = "system.new";
    dbm.change_commentary = "Initial setup";
    dbm.recorded_at = datetime::make_timepoint(2025, 1, 1);

    std::vector<dataset_bundle_member> members = {dbm};
    auto table = convert_to_table(members);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Bundle"));
    CHECK(table.contains("Dataset"));
    CHECK(table.contains("base"));
    CHECK(table.contains("iso.currencies"));
}

TEST_CASE("dataset_bundle_member_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));

    auto sut = ores::dq::generators::generate_synthetic_dataset_bundle_member();

    BOOST_LOG_SEV(lg, info) << "Generated bundle member: " << sut;

    CHECK(sut.version == 1);
    CHECK(!sut.bundle_code.empty());
    CHECK(!sut.dataset_code.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_reason_code == "system.new_record");
}

TEST_CASE("dataset_bundle_member_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));

    const std::size_t count = 5;
    auto members = ores::dq::generators::generate_synthetic_dataset_bundle_members(count);

    BOOST_LOG_SEV(lg, info) << "Generated " << members.size() << " bundle members";
    BOOST_LOG_SEV(lg, info) << members;

    CHECK(members.size() == count);
    for (const auto& m : members) {
        CHECK(!m.bundle_code.empty());
        CHECK(!m.dataset_code.empty());
    }
}
