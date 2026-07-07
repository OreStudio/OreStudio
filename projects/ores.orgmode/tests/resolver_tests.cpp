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
#include "ores.orgmode/indexing/resolver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sqlite3.h>

namespace {

using ores::orgmode::indexing::resolver;

/**
 * @brief Build a tiny throwaway org-roam-shaped SQLite database so the
 * resolver's SQL and quote-handling can be tested without depending on
 * the real repo's .org-roam.db (used separately, in the integration
 * test below, to prove this also works against the real thing).
 */
std::filesystem::path make_fixture_db(const std::filesystem::path& path) {
    if (std::filesystem::exists(path))
        std::filesystem::remove(path);
    sqlite3* db = nullptr;
    REQUIRE(sqlite3_open(path.string().c_str(), &db) == SQLITE_OK);
    const char* ddl = "create table files (file unique primary key, title, hash not null, "
                      "atime not null, mtime not null);"
                      "create table nodes (id not null primary key, file not null, level not null, "
                      "pos not null, todo, priority, scheduled text, deadline text, title, "
                      "properties, olp);"
                      "insert into files values ('\"/tmp/fixture.org\"', '\"Fixture\"', 'h', 0, 0);"
                      "insert into nodes (id, file, level, pos, title) values "
                      "('\"F7C39C66-DFA0-4A10-B473-F3827B8A7187\"', '\"/tmp/fixture.org\"', 0, 0, "
                      "'\"Task: Something\"');";
    char* err = nullptr;
    REQUIRE(sqlite3_exec(db, ddl, nullptr, nullptr, &err) == SQLITE_OK);
    sqlite3_close(db);
    return path;
}

}

TEST_CASE("resolver resolves a known id, unquoting the elisp-literal columns",
          "[ores.orgmode][resolver]") {
    const auto path = make_fixture_db(std::filesystem::temp_directory_path() /
                                      "ores_orgmode_resolver_fixture.db");
    const resolver r(path);

    const auto result = r.resolve("F7C39C66-DFA0-4A10-B473-F3827B8A7187");
    REQUIRE(result.has_value());
    REQUIRE(result->id == "F7C39C66-DFA0-4A10-B473-F3827B8A7187");
    REQUIRE(result->path == "/tmp/fixture.org");
    REQUIRE(result->title == "Task: Something");
    // /tmp/fixture.org doesn't actually exist on disk (only its row does,
    // in this throwaway fixture db) — type lookup fails closed to "".
    REQUIRE(result->type.empty());
}

TEST_CASE("resolver returns nullopt for a dangling link, not a throw", "[ores.orgmode][resolver]") {
    const auto path = make_fixture_db(std::filesystem::temp_directory_path() /
                                      "ores_orgmode_resolver_fixture2.db");
    const resolver r(path);

    REQUIRE_FALSE(r.resolve("00000000-0000-0000-0000-000000000000").has_value());
}

TEST_CASE("resolver throws on a missing database file", "[ores.orgmode][resolver]") {
    REQUIRE_THROWS_AS(resolver("/no/such/org-roam.db"), std::filesystem::filesystem_error);
}

TEST_CASE("resolver resolves against the real repo org-roam index (integration)",
          "[ores.orgmode][resolver][integration]") {
    // Skips cleanly when the checkout has no .org-roam.db yet (e.g. a
    // fresh clone before `compass index` has ever run).
    const std::filesystem::path db_path = std::filesystem::path(ORES_REPO_ROOT) / ".org-roam.db";
    if (!std::filesystem::exists(db_path))
        return;

    const resolver r(db_path);
    // This story's own id: resolving it against the live index should
    // find this very file.
    const auto result = r.resolve("A85B16E3-5A42-4484-A570-13EE7CFA9F50");
    REQUIRE(result.has_value());
    REQUIRE(result->title.starts_with("Story: ores.orgmode"));
    REQUIRE(result->type == "story");
}
