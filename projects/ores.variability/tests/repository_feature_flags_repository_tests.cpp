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
#include "ores.variability/repository/feature_flags_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.variability/domain/feature_flags.hpp"
#include "ores.variability/domain/feature_flags_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string database_table("ores_variability_feature_flags_tbl");
const std::string tags("[repository]");

ores::variability::domain::feature_flags generate_feature_flag() {
    ores::variability::domain::feature_flags flag;
    flag.name = std::string(faker::word::noun()) + "_" +
        std::string(faker::word::verb()) + "_" +
        faker::string::alphanumeric(4);
    flag.enabled = faker::datatype::boolean();
    flag.description = std::string(faker::lorem::sentence());
    flag.recorded_by = std::string(faker::internet::username());
    flag.change_reason_code = "system.test";
    flag.change_commentary = "Synthetic test data";
    return flag;
}

std::vector<ores::variability::domain::feature_flags>
generate_feature_flags(int count) {
    std::vector<ores::variability::domain::feature_flags> flags;
    flags.reserve(count);
    for (int i = 0; i < count; ++i) {
        flags.push_back(generate_feature_flag());
    }
    return flags;
}

}

using namespace ores::logging;
using ores::testing::database_helper;
using ores::variability::domain::feature_flags;
using ores::variability::repository::feature_flags_repository;

TEST_CASE("write_single_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());
    auto flag = generate_feature_flag();

    BOOST_LOG_SEV(lg, debug) << "Feature flag: " << flag;
    CHECK_NOTHROW(repo.write(flag));

    auto read_flags = repo.read_latest(flag.name);
    REQUIRE(read_flags.size() == 1);
    const auto& read_flag = read_flags[0];
    CHECK(read_flag.name == flag.name);
    CHECK(read_flag.enabled == flag.enabled);
    CHECK(read_flag.description == flag.description);
}

TEST_CASE("write_multiple_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());
    auto flags = generate_feature_flags(5);
    BOOST_LOG_SEV(lg, debug) << "Generated " << flags.size() << " feature flags";

    const auto initial_count = repo.read_latest().size();
    BOOST_LOG_SEV(lg, debug) << "Initial feature flags count: " << initial_count;

    CHECK_NOTHROW(repo.write(flags));

    auto read_flags = repo.read_latest();
    CHECK(read_flags.size() == initial_count + flags.size());
}

TEST_CASE("read_latest_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());
    const auto initial_count = repo.read_latest().size();
    BOOST_LOG_SEV(lg, debug) << "Initial feature flags count: " << initial_count;

    auto written_flags = generate_feature_flags(3);
    BOOST_LOG_SEV(lg, debug) << "Writing " << written_flags.size() << " feature flags";

    repo.write(written_flags);

    auto read_flags = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    CHECK(!read_flags.empty());
    CHECK(read_flags.size() == initial_count + written_flags.size());
}

TEST_CASE("read_latest_feature_flag_by_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());
    auto flags = generate_feature_flags(5);

    const auto target = flags.front();
    BOOST_LOG_SEV(lg, debug) << "Write feature flags, target: " << target.name;
    repo.write(flags);

    BOOST_LOG_SEV(lg, debug) << "Target flag: " << target;

    auto read_flags = repo.read_latest(target.name);
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    REQUIRE(read_flags.size() == 1);
    CHECK(read_flags[0].name == target.name);
    CHECK(read_flags[0].enabled == target.enabled);
    CHECK(read_flags[0].description == target.description);
}

TEST_CASE("read_all_feature_flags", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());
    const auto initial_count = repo.read_all().size();
    BOOST_LOG_SEV(lg, debug) << "Initial feature flags count (all versions): " << initial_count;

    auto written_flags = generate_feature_flags(5);
    BOOST_LOG_SEV(lg, debug) << "Writing " << written_flags.size() << " feature flags";

    repo.write(written_flags);

    auto read_flags = repo.read_all();
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    CHECK(!read_flags.empty());
    CHECK(read_flags.size() == initial_count + written_flags.size());
}

TEST_CASE("read_all_feature_flags_by_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    // Create a flag and write multiple versions
    auto flag1 = generate_feature_flag();
    const std::string test_name = flag1.name;
    BOOST_LOG_SEV(lg, debug) << "Flag version 1: " << flag1;

    auto flag2 = flag1;
    flag2.version = 1;
    flag2.enabled = !flag1.enabled;
    flag2.description = "Updated description version 2";
    BOOST_LOG_SEV(lg, debug) << "Flag version 2: " << flag2;

    repo.write({flag1});
    repo.write({flag2});

    // Read all versions
    auto read_flags = repo.read_all(test_name);
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    CHECK(read_flags.size() == 2);

    // Verify different versions exist
    bool found_v1 = false, found_v2 = false;
    for (const auto& flag : read_flags) {
        if (flag.name == test_name && flag.version == 1) found_v1 = true;
        if (flag.name == test_name && flag.version == 2) found_v2 = true;
    }

    CHECK(found_v1);
    CHECK(found_v2);
}

TEST_CASE("read_nonexistent_feature_flag_by_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    const std::string nonexistent_name = "nonexistent_flag_" +
        faker::string::alphanumeric(10);
    BOOST_LOG_SEV(lg, debug) << "Non-existent name: " << nonexistent_name;

    auto read_flags = repo.read_latest(nonexistent_name);
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    CHECK(read_flags.size() == 0);
}

TEST_CASE("remove_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    // Write a flag
    auto flag = generate_feature_flag();
    BOOST_LOG_SEV(lg, debug) << "Feature flag: " << flag;
    repo.write(flag);

    // Verify it exists
    auto read_before = repo.read_latest(flag.name);
    REQUIRE(read_before.size() == 1);

    // Remove it
    repo.remove(flag.name);

    // Verify it's no longer in latest
    auto read_after = repo.read_latest(flag.name);
    BOOST_LOG_SEV(lg, debug) << "Read after remove: " << read_after.size();
    CHECK(read_after.empty());
}

TEST_CASE("write_and_read_enabled_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    // Create enabled flag
    auto enabled_flag = generate_feature_flag();
    enabled_flag.enabled = true;
    BOOST_LOG_SEV(lg, debug) << "Enabled flag: " << enabled_flag;

    repo.write({enabled_flag});

    // Read back and verify enabled flag
    auto read_flags = repo.read_latest(enabled_flag.name);
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    REQUIRE(read_flags.size() == 1);
    CHECK(read_flags[0].enabled == true);
    CHECK(read_flags[0].name == enabled_flag.name);
}

TEST_CASE("write_and_read_disabled_feature_flag", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    // Create disabled flag
    auto disabled_flag = generate_feature_flag();
    disabled_flag.enabled = false;
    BOOST_LOG_SEV(lg, debug) << "Disabled flag: " << disabled_flag;

    repo.write({disabled_flag});

    // Read back and verify disabled flag
    auto read_flags = repo.read_latest(disabled_flag.name);
    BOOST_LOG_SEV(lg, debug) << "Read " << read_flags.size() << " feature flags";

    REQUIRE(read_flags.size() == 1);
    CHECK(read_flags[0].enabled == false);
    CHECK(read_flags[0].name == disabled_flag.name);
}

TEST_CASE("feature_flag_version_increment", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    feature_flags_repository repo(h.context());

    // Create initial version
    auto flag = generate_feature_flag();
    flag.version = 0;
    const std::string flag_name = flag.name;
    BOOST_LOG_SEV(lg, debug) << "Initial flag: " << flag;
    repo.write({flag});

    // Update with incremented version
    flag.version = 1;
    flag.enabled = !flag.enabled;
    flag.recorded_by = "version_updater";
    BOOST_LOG_SEV(lg, debug) << "Updated flag: " << flag;
    repo.write({flag});

    // Read latest should return version 1
    auto read_flags = repo.read_latest(flag_name);
    BOOST_LOG_SEV(lg, debug) << "Read latest: " << read_flags.size() << " flags";

    REQUIRE(read_flags.size() == 1);
    CHECK(read_flags[0].version == 2);
    CHECK(read_flags[0].recorded_by == "version_updater");
}
