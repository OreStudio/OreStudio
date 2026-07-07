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
#include "ores.shell/app/commands/orgmode_commands.hpp"
#include "ores.orgmode/indexing/resolver.hpp"
#include "ores.orgmode/parser/parser.hpp"
#include <cli/cli.h>
#include <filesystem>
#include <ostream>

namespace ores::shell::app::commands {

namespace {

/**
 * @brief Walk up from the current directory looking for `.org-roam.db`
 * (compass's index, at the repo root). Returns an empty path if none
 * is found within a handful of levels.
 */
std::filesystem::path find_org_roam_db() {
    auto dir = std::filesystem::current_path();
    for (int i = 0; i < 10; ++i) {
        const auto candidate = dir / ".org-roam.db";
        if (std::filesystem::exists(candidate))
            return candidate;
        if (!dir.has_parent_path() || dir == dir.parent_path())
            break;
        dir = dir.parent_path();
    }
    return {};
}

/**
 * @brief First `id:` link found via depth-first search, or nullptr.
 */
const orgmode::domain::link* first_link(const orgmode::domain::heading& h) {
    if (!h.links.empty())
        return &h.links.front();
    for (const auto& child : h.children) {
        if (const auto* found = first_link(child))
            return found;
    }
    return nullptr;
}

const orgmode::domain::link* first_link(const orgmode::domain::document& doc) {
    for (const auto& h : doc.headings) {
        if (const auto* found = first_link(h))
            return found;
    }
    return nullptr;
}

}

void orgmode_commands::register_commands(cli::Menu& root_menu) {
    root_menu.Insert(
        "doc-show",
        [](std::ostream& out, const std::string& path) { process_doc_show(out, path); },
        "Parse a .org file and show its summary, resolving its first outgoing link");
}

void orgmode_commands::process_doc_show(std::ostream& out, const std::string& path) {
    orgmode::domain::document doc;
    try {
        doc = orgmode::parser::parse_file(path);
    } catch (const std::exception& e) {
        out << "Could not parse '" << path << "': " << e.what() << "\n";
        return;
    }

    out << "id:      " << doc.id().value_or("<none>") << "\n";
    out << "type:    " << doc.find_keyword("type").value_or("<none>") << "\n";
    out << "title:   " << doc.find_keyword("title").value_or("<none>") << "\n";
    out << "headings: " << doc.headings.size() << " top-level\n";

    const auto* link = first_link(doc);
    if (link == nullptr) {
        out << "No outgoing id: links found.\n";
        return;
    }
    out << "First link -> id:" << link->target_id << " [" << link->text << "]\n";

    const auto db_path = find_org_roam_db();
    if (db_path.empty()) {
        out << "Could not find .org-roam.db (not under a compass-indexed checkout?).\n";
        return;
    }
    try {
        const orgmode::indexing::resolver resolver(db_path);
        const auto resolved = resolver.resolve(*link);
        if (!resolved) {
            out << "Link target not found in the index (dangling link).\n";
            return;
        }
        out << "Resolved -> " << resolved->type << " \"" << resolved->title << "\" ("
            << resolved->path << ")\n";
    } catch (const std::exception& e) {
        out << "Could not resolve link: " << e.what() << "\n";
    }
}

}
