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
#include "ores.variability.core/repository/system_settings_repository.hpp"

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include "ores.variability.api/domain/system_setting_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[repository]");

ores::variability::domain::system_setting make_system_setting(
    const std::string& name,
    const std::string& value = "test_value",
    const std::string& data_type = "string") {

    ores::variability::domain::system_setting s;
    s.name = name;
    s.value = value;
    s.data_type = data_type;
    s.description = "Test setting for " + name;
    s.modified_by = "test_user";
    s.performed_by = "test_user";
    s.recorded_at = std::chrono::system_clock::now();
    return s;
}

}

using ores::variability::domain::system_setting;
using ores::variability::repository::system_settings_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_system_setting", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto s = make_system_setting("system.write_single_test");
    BOOST_LOG_SEV(lg, debug) << "System setting: " << s;

    system_settings_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), s));
}

TEST_CASE("write_multiple_system_settings", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    std::vector<system_setting> settings = {
        make_system_setting("system.write_multi_test_1", "value1", "string"),
        make_system_setting("system.write_multi_test_2", "42", "integer"),
        make_system_setting("system.write_multi_test_3", "true", "boolean")
    };
    BOOST_LOG_SEV(lg, debug) << "Writing " << settings.size() << " settings";

    system_settings_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), settings));
}

TEST_CASE("read_latest_system_settings", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto s = make_system_setting("system.read_latest_test");

    system_settings_repository repo;
    repo.write(h.context(), s);

    auto read_settings = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read " << read_settings.size() << " settings";

    CHECK(!read_settings.empty());
    auto it = std::ranges::find_if(read_settings,
        [&s](const system_setting& r) { return r.name == s.name; });
    CHECK(it != read_settings.end());
}

TEST_CASE("read_latest_system_setting_by_name", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto s = make_system_setting("system.read_by_name_test", "hello", "string");

    system_settings_repository repo;
    repo.write(h.context(), s);

    auto read_settings = repo.read_latest(h.context(), s.name);
    BOOST_LOG_SEV(lg, debug) << "Read settings by name: " << read_settings.size();

    REQUIRE(read_settings.size() == 1);
    CHECK(read_settings[0].name == s.name);
    CHECK(read_settings[0].value == s.value);
    CHECK(read_settings[0].data_type == s.data_type);
}

TEST_CASE("read_latest_with_pagination", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    std::vector<system_setting> settings = {
        make_system_setting("system.pagination_test_1"),
        make_system_setting("system.pagination_test_2"),
        make_system_setting("system.pagination_test_3")
    };

    system_settings_repository repo;
    repo.write(h.context(), settings);

    auto total = repo.get_total_count(h.context());
    BOOST_LOG_SEV(lg, debug) << "Total count: " << total;

    CHECK(total >= 3);

    auto page = repo.read_latest(h.context(), std::uint32_t{0}, std::uint32_t{2});
    BOOST_LOG_SEV(lg, debug) << "Page size: " << page.size();

    CHECK(page.size() <= 2);
}

TEST_CASE("read_all_versions_of_system_setting", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto s = make_system_setting("system.read_all_test", "initial", "string");

    system_settings_repository repo;
    repo.write(h.context(), s);

    s.value = "updated";
    repo.write(h.context(), s);

    auto all_versions = repo.read_all(h.context(), s.name);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all_versions.size();

    CHECK(all_versions.size() >= 2);
}

TEST_CASE("remove_system_setting", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto s = make_system_setting("system.remove_test");

    system_settings_repository repo;
    repo.write(h.context(), s);

    auto before = repo.read_latest(h.context(), s.name);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), s.name));

    auto after = repo.read_latest(h.context(), s.name);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("read_nonexistent_system_setting", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    system_settings_repository repo;

    const std::string nonexistent = "system.this_does_not_exist_xyz_12345";
    auto result = repo.read_latest(h.context(), nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Result size: " << result.size();

    CHECK(result.empty());
}
