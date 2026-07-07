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
#ifndef ORES_ORGMODE_INDEXING_RESOLVER_HPP
#define ORES_ORGMODE_INDEXING_RESOLVER_HPP

#include "ores.orgmode/domain/link.hpp"
#include "ores.orgmode/domain/resolved_target.hpp"
#include "ores.orgmode/export.hpp"
#include <filesystem>
#include <memory>
#include <optional>
#include <string>

struct sqlite3;

namespace ores::orgmode::indexing {

/**
 * @brief Resolves `id:` links against the org-roam SQLite index that
 * `compass index` already builds and maintains (`.org-roam.db` at the
 * repo root).
 *
 * Read-only: opens the database with `SQLITE_OPEN_READONLY` and never
 * writes to it. `ores.orgmode` builds and maintains no index of its
 * own — this class is a thin, read-only query layer over the one
 * compass already produces.
 */
class ORES_ORGMODE_EXPORT resolver final {
public:
    /**
     * @brief Open the org-roam index at @p db_path.
     *
     * Throws std::filesystem::filesystem_error if the database file
     * does not exist, or std::runtime_error if it exists but sqlite3
     * cannot open it (e.g. not actually a SQLite file).
     */
    explicit resolver(const std::filesystem::path& db_path);
    ~resolver();

    resolver(const resolver&) = delete;
    resolver& operator=(const resolver&) = delete;
    resolver(resolver&&) noexcept;
    resolver& operator=(resolver&&) noexcept;

    /**
     * @brief Resolve a single link's target_id.
     *
     * @return std::nullopt if no node in the index has this id (a
     * dangling link) — never throws for that case.
     */
    [[nodiscard]] std::optional<domain::resolved_target> resolve(const std::string& id) const;

    /**
     * @brief Convenience overload taking the link directly.
     */
    [[nodiscard]] std::optional<domain::resolved_target> resolve(const domain::link& l) const {
        return resolve(l.target_id);
    }

private:
    struct handle_deleter final {
        void operator()(sqlite3* db) const noexcept;
    };
    std::unique_ptr<sqlite3, handle_deleter> db_;
};

}

#endif
