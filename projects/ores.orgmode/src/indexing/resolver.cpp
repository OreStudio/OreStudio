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
#include "ores.orgmode/parser/parser.hpp"
#include <sqlite3.h>
#include <stdexcept>

namespace ores::orgmode::indexing {

namespace {

/**
 * @brief org-roam stores every text column as an elisp `prin1`-printed
 * string literal — including the surrounding double quotes and
 * backslash-escaping of `"` and `\` inside. Undo both so callers see
 * the plain value, e.g. `"a\"b"` (7 characters, quotes included)
 * becomes `a"b` (3 characters).
 */
std::string unquote(const std::string& raw) {
    if (raw.size() < 2 || raw.front() != '"' || raw.back() != '"')
        return raw;
    std::string out;
    out.reserve(raw.size() - 2);
    for (std::size_t i = 1; i + 1 < raw.size(); ++i) {
        if (raw[i] == '\\' && i + 2 < raw.size())
            ++i;
        out.push_back(raw[i]);
    }
    return out;
}

}

void resolver::handle_deleter::operator()(sqlite3* db) const noexcept {
    sqlite3_close(db);
}

resolver::resolver(const std::filesystem::path& db_path) {
    if (!std::filesystem::exists(db_path)) {
        throw std::filesystem::filesystem_error(
            "ores.orgmode: org-roam index not found",
            db_path,
            std::make_error_code(std::errc::no_such_file_or_directory));
    }
    sqlite3* raw = nullptr;
    const int rc = sqlite3_open_v2(db_path.string().c_str(), &raw, SQLITE_OPEN_READONLY, nullptr);
    db_.reset(raw);
    if (rc != SQLITE_OK) {
        const std::string msg = raw ? sqlite3_errmsg(raw) : "unknown error";
        throw std::runtime_error("ores.orgmode: could not open org-roam index: " + msg);
    }
}

resolver::~resolver() = default;
resolver::resolver(resolver&&) noexcept = default;
resolver& resolver::operator=(resolver&&) noexcept = default;

std::optional<domain::resolved_target> resolver::resolve(const std::string& id) const {
    static constexpr const char* query = "select file, title from nodes where id = ?1 limit 1";

    sqlite3_stmt* stmt = nullptr;
    if (sqlite3_prepare_v2(db_.get(), query, -1, &stmt, nullptr) != SQLITE_OK)
        throw std::runtime_error(std::string("ores.orgmode: failed to prepare query: ") +
                                 sqlite3_errmsg(db_.get()));

    // org-roam stores ids as quoted elisp string literals (e.g.
    // "F7C39C66-..."), so the lookup key must be quoted the same way.
    const std::string quoted_id = "\"" + id + "\"";
    sqlite3_bind_text(
        stmt, 1, quoted_id.c_str(), static_cast<int>(quoted_id.size()), SQLITE_TRANSIENT);

    std::optional<domain::resolved_target> result;
    if (sqlite3_step(stmt) == SQLITE_ROW) {
        const auto* file_text = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 0));
        const auto* title_text = reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1));
        result = domain::resolved_target{.id = id,
                                         .path = unquote(file_text ? file_text : ""),
                                         .title = unquote(title_text ? title_text : "")};
    }
    sqlite3_finalize(stmt);

    if (result) {
        // Best-effort: a target file that's gone missing or become
        // unreadable since the index was last built shouldn't turn a
        // successful id lookup into a hard failure — leave type empty.
        try {
            const auto target_doc = parser::parse_file(result->path);
            result->type = target_doc.find_keyword("type").value_or("");
        } catch (const std::exception&) {
        }
    }
    return result;
}

}
