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
#include "ores.orgmode/parser/parser.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <rfl/json.hpp>
#include <vector>

namespace {

namespace fs = std::filesystem;

/**
 * @brief Every `.org` file under the repo root, excluding build output
 * and vendored third-party trees this component has no business
 * parsing (or that are large enough to slow the test for no benefit).
 */
std::vector<fs::path> collect_org_files(const fs::path& repo_root) {
    static const std::vector<std::string> excluded_dirs = {
        "build", "external", ".git", "vcpkg", "vcpkg_installed"};

    std::vector<fs::path> files;
    for (auto it = fs::recursive_directory_iterator(repo_root,
                                                    fs::directory_options::skip_permission_denied);
         it != fs::recursive_directory_iterator();
         ++it) {
        if (it->is_directory()) {
            const auto name = it->path().filename().string();
            if (std::find(excluded_dirs.begin(), excluded_dirs.end(), name) !=
                excluded_dirs.end()) {
                it.disable_recursion_pending();
            }
            continue;
        }
        if (it->path().extension() == ".org")
            files.push_back(it->path());
    }
    return files;
}

}

TEST_CASE("every .org doc in the repo parses without crashing, and dumps to JSON",
          "[ores.orgmode][corpus]") {
    const fs::path repo_root(ORES_REPO_ROOT);
    const auto files = collect_org_files(repo_root);

    // A silent zero would read as "covered everything" when the glob
    // just found nothing — fail loudly instead so a broken exclusion
    // list or a moved repo root is caught immediately.
    REQUIRE(files.size() > 100);

    const fs::path out_dir = fs::path(ORES_TEST_OUTPUT_DIR) / "orgmode_corpus_json";
    fs::create_directories(out_dir);

    std::vector<std::string> failures;
    std::size_t parsed = 0;
    for (const auto& file : files) {
        try {
            const auto doc = ores::orgmode::parser::parse_file(file);
            const auto json = rfl::json::write(doc);

            auto rel = fs::relative(file, repo_root).string();
            std::replace(rel.begin(), rel.end(), '/', '_');
            std::ofstream out(out_dir / (rel + ".json"));
            out << json;

            ++parsed;
        } catch (const std::exception& e) {
            failures.push_back(file.string() + ": " + e.what());
        }
    }

    INFO("Parsed " << parsed << "/" << files.size() << " files. JSON written to "
                   << out_dir.string());
    if (!failures.empty()) {
        std::string msg = std::to_string(failures.size()) + " file(s) failed to parse:\n";
        for (const auto& f : failures)
            msg += "  " + f + "\n";
        FAIL(msg);
    }
}
