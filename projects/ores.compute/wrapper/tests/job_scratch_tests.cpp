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
#include "ores.compute.wrapper/app/job_scratch.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <string>

namespace {

using ores::compute::wrapper::app::release_job_folder;
using ores::platform::filesystem::scoped_temp_directory;

namespace fs = std::filesystem;

void write_scratch_file(const fs::path& path, const std::string& content) {
    fs::create_directories(path.parent_path());
    std::ofstream out(path, std::ios::binary);
    out.write(content.data(), static_cast<std::streamsize>(content.size()));
}

}

TEST_CASE("releasing_a_jobs_folder_removes_it_and_states_the_bytes_it_held") {
    scoped_temp_directory root;
    const auto job_folder = root.path() / "jobs" / "6f1c2a44-9d2b-4a51-9b3e-2f4b6c8d0e1a";
    write_scratch_file(job_folder / "input.tar.gz", std::string(120, 'i'));
    write_scratch_file(job_folder / "Output" / "log.txt", std::string(30, 'l'));
    write_scratch_file(job_folder / "output.tar.gz", std::string(400, 'o'));

    const auto released = release_job_folder(job_folder);

    REQUIRE(released);
    CHECK(*released == 550);
    CHECK_FALSE(fs::exists(job_folder));
    // The scratch root the folder hung under is the node's, not the job's.
    CHECK(fs::exists(root.path() / "jobs"));
}

TEST_CASE("releasing_a_folder_that_is_already_gone_is_the_end_state_asked_for") {
    scoped_temp_directory root;
    const auto job_folder = root.path() / "jobs" / "6f1c2a44-9d2b-4a51-9b3e-2f4b6c8d0e1a";

    const auto released = release_job_folder(job_folder);

    REQUIRE(released);
    CHECK(*released == 0);
}
