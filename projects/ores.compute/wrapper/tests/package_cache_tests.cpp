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
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.wrapper/app/package_cache.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>

namespace {

using ores::compute::wrapper::app::package_cache_dir;
using ores::compute::wrapper::app::package_cache_key;
using ores::compute::wrapper::app::unpack_package;
using ores::platform::filesystem::scoped_temp_directory;

namespace fs = std::filesystem;

void write_file(const fs::path& path, const std::string& content) {
    fs::create_directories(path.parent_path());
    std::ofstream out(path, std::ios::binary);
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
}

ores::compute::messaging::work_assignment_event an_assignment(const std::string& app_version_id,
                                                              const std::string& package_sha256) {
    ores::compute::messaging::work_assignment_event evt;
    evt.app_version_id = app_version_id;
    evt.package_sha256 = package_sha256;
    return evt;
}

}

TEST_CASE("a_package_is_named_by_its_digest_so_two_versions_share_one_copy") {
    const auto digest =
        std::string("9f2c0f7d5b1a4e3c8d6f0a2b4c6d8e0f1a3b5c7d9e1f3a5b7c9d1e3f5a7b9c1d");
    const auto first = an_assignment("7e2c05d6-bd2d-41d1-aa51-3b9dd09d056a", digest);
    const auto second = an_assignment("f2ce9b84-20d3-47e4-8abb-faf86fac2249", digest);

    CHECK(package_cache_key(first) == digest);
    CHECK(package_cache_key(second) == digest);

    // Two versions of one package land in one directory on the host, which is
    // what makes the five wrappers read one unpacked copy.
    const fs::path packages_root("/var/tmp/ores-packages");
    CHECK(package_cache_dir(packages_root, first) == package_cache_dir(packages_root, second));
    CHECK(package_cache_dir(packages_root, first) == packages_root / digest);
}

TEST_CASE("a_message_that_states_no_digest_falls_back_to_its_app_version") {
    const auto evt = an_assignment("7e2c05d6-bd2d-41d1-aa51-3b9dd09d056a", "");
    CHECK(package_cache_key(evt) == "7e2c05d6-bd2d-41d1-aa51-3b9dd09d056a");
}

TEST_CASE("unpacking_leaves_the_tree_and_does_not_keep_the_archive") {
    scoped_temp_directory root;
    const auto packages_root = root.path() / "packages";

    scoped_temp_directory payload;
    write_file(payload.path() / "manifest.json", R"({"executable":"ore.exe"})");
    write_file(payload.path() / "lib" / "engine.so", "engine");

    const fs::path archive = packages_root / "package.tar.gz";
    fs::create_directories(packages_root);
    ores::storage::filesystem::archiver::pack(payload.path(), archive);
    REQUIRE(fs::exists(archive));

    const auto pkg_cache_dir =
        packages_root / "9f2c0f7d5b1a4e3c8d6f0a2b4c6d8e0f1a3b5c7d9e1f3a5b7c9d1e3f5a7b9c1d";
    unpack_package(pkg_cache_dir, archive);

    CHECK(fs::exists(pkg_cache_dir / "manifest.json"));
    CHECK(fs::exists(pkg_cache_dir / "lib" / "engine.so"));
    CHECK_FALSE(fs::exists(archive));
    CHECK_FALSE(fs::exists(pkg_cache_dir.string() + ".tmp"));
}

TEST_CASE("unpacking_over_a_previous_tree_leaves_the_new_one_and_no_archive") {
    scoped_temp_directory root;
    const auto packages_root = root.path() / "packages";
    const auto pkg_cache_dir =
        packages_root / "9f2c0f7d5b1a4e3c8d6f0a2b4c6d8e0f1a3b5c7d9e1f3a5b7c9d1e3f5a7b9c1d";
    fs::create_directories(pkg_cache_dir);
    write_file(pkg_cache_dir / "stale.txt", "from an older unpack");

    scoped_temp_directory payload;
    write_file(payload.path() / "manifest.json", R"({"executable":"ore.exe"})");
    const fs::path archive = packages_root / "package.tar.gz";
    ores::storage::filesystem::archiver::pack(payload.path(), archive);

    unpack_package(pkg_cache_dir, archive);

    CHECK(fs::exists(pkg_cache_dir / "manifest.json"));
    CHECK_FALSE(fs::exists(pkg_cache_dir / "stale.txt"));
    CHECK_FALSE(fs::exists(archive));
}
