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
#include <cctype>
#include <fstream>
#include <regex>
#include <sstream>

namespace ores::orgmode::parser {

namespace {

using ores::orgmode::domain::heading;
using ores::orgmode::domain::keyword;
using ores::orgmode::domain::link;
using ores::orgmode::domain::property;
using ores::orgmode::domain::table;

const std::regex& heading_re() {
    static const std::regex re(R"(^(\*+)\s+(.+?)\s*$)");
    return re;
}
const std::regex& frontmatter_re() {
    static const std::regex re(R"(^#\+([A-Za-z_][A-Za-z_0-9]*)\s*:\s*(.*)$)");
    return re;
}
const std::regex& drawer_prop_re() {
    static const std::regex re(R"(^\s*:([A-Za-z_][A-Za-z0-9_.-]*)\s*:\s*(.*)$)");
    return re;
}
const std::regex& bullet_re() {
    static const std::regex re(R"(^\s*-\s+(.*)$)");
    return re;
}
const std::regex& table_row_re() {
    static const std::regex re(R"(^\s*\|(.+)\|\s*$)");
    return re;
}
const std::regex& table_sep_re() {
    static const std::regex re(R"(^\s*\|[-+|]+\|\s*$)");
    return re;
}
const std::regex& link_re() {
    static const std::regex re(R"(\[\[id:([^\]]+)\]\[([^\]]*)\]\])");
    return re;
}

std::string trim(const std::string& s) {
    const auto begin = s.find_first_not_of(" \t\r\n");
    if (begin == std::string::npos)
        return "";
    const auto end = s.find_last_not_of(" \t\r\n");
    return s.substr(begin, end - begin + 1);
}

std::string upper(std::string s) {
    for (auto& c : s)
        c = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
    return s;
}

std::vector<std::string> split_row(const std::string& inner) {
    std::vector<std::string> cells;
    std::stringstream ss(inner);
    std::string cell;
    while (std::getline(ss, cell, '|'))
        cells.push_back(trim(cell));
    return cells;
}

/**
 * @brief Extract every `[[id:UUID][text]]` link from a block of text.
 */
void extract_links(const std::string& text, std::vector<link>& out) {
    auto begin = std::sregex_iterator(text.begin(), text.end(), link_re());
    const auto end = std::sregex_iterator();
    for (auto it = begin; it != end; ++it) {
        const auto& m = *it;
        out.push_back(link{.target_id = m[1].str(), .text = m[2].str()});
    }
}

void extract_links(const std::vector<std::string>& lines, std::vector<link>& out) {
    for (const auto& line : lines)
        extract_links(line, out);
}

void extract_links(const std::vector<table>& tables, std::vector<link>& out) {
    for (const auto& t : tables) {
        for (const auto& row : t.rows) {
            for (const auto& cell : row)
                extract_links(cell, out);
        }
    }
}

/**
 * @brief Turn the raw rows collected for one table (header row first,
 * separator rows already dropped) into a `table` with the header split
 * out, mirroring `_normalise_table` in `org_loader.py`.
 */
table normalise_table(const std::vector<std::vector<std::string>>& raw_rows) {
    table t;
    if (raw_rows.empty())
        return t;
    t.headers = raw_rows.front();
    t.rows.assign(raw_rows.begin() + 1, raw_rows.end());
    return t;
}

/**
 * @brief One flat heading record produced by the line-scanning pass,
 * before it is nested into a tree by level.
 */
struct flat_heading final {
    unsigned int level = 0;
    std::string title;
    std::vector<property> properties;
    std::vector<std::string> body_lines;
    std::vector<std::vector<std::string>> bullet_lists;
    std::vector<table> tables;
};

heading finish(flat_heading&& f) {
    heading h;
    h.level = f.level;
    h.title = std::move(f.title);
    h.properties = std::move(f.properties);
    for (const auto& p : h.properties) {
        if (p.key == "ID") {
            h.id = p.value;
            break;
        }
    }
    h.body_lines = std::move(f.body_lines);
    h.bullet_lists = std::move(f.bullet_lists);
    h.tables = std::move(f.tables);
    extract_links(h.body_lines, h.links);
    extract_links(h.tables, h.links);
    return h;
}

/**
 * @brief Consume every flat record with `level > parent_level` from
 * `flat[idx...]`, nesting each one under the nearest preceding record
 * with a smaller level — a heading of level L becomes a child of the
 * nearest earlier heading with level < L, tolerating skipped levels
 * (e.g. `*` followed directly by `***`), same as the stack-popping
 * logic in `org_loader.py`'s `parse_org`.
 */
std::vector<heading>
build_level(std::vector<flat_heading>& flat, std::size_t& idx, unsigned int parent_level) {
    std::vector<heading> result;
    while (idx < flat.size() && flat[idx].level > parent_level) {
        const unsigned int this_level = flat[idx].level;
        heading node = finish(std::move(flat[idx]));
        ++idx;
        node.children = build_level(flat, idx, this_level);
        result.push_back(std::move(node));
    }
    return result;
}

}

domain::document parse(const std::string& text) {
    domain::document doc;

    std::vector<flat_heading> flat;
    // file_properties/frontmatter accumulate until the first heading;
    // afterwards, drawer entries and body lines belong to `flat.back()`.
    bool seen_first_heading = false;
    bool in_drawer = false;

    std::vector<std::vector<std::string>> pending_table;
    std::vector<std::string> pending_bullets;

    auto close_table_if_open = [&]() {
        if (!pending_table.empty()) {
            table t = normalise_table(pending_table);
            if (seen_first_heading)
                flat.back().tables.push_back(std::move(t));
            pending_table.clear();
        }
    };
    auto flush_bullets = [&]() {
        if (!pending_bullets.empty()) {
            if (seen_first_heading)
                flat.back().bullet_lists.push_back(pending_bullets);
            pending_bullets.clear();
        }
    };

    std::istringstream stream(text);
    std::string raw_line;
    while (std::getline(stream, raw_line)) {
        // Strip a trailing '\r' so CRLF-saved docs parse the same as LF.
        if (!raw_line.empty() && raw_line.back() == '\r')
            raw_line.pop_back();
        const std::string& line = raw_line;

        if (in_drawer) {
            if (upper(trim(line)) == ":END:") {
                in_drawer = false;
                continue;
            }
            std::smatch m;
            if (std::regex_match(line, m, drawer_prop_re())) {
                property p{.key = m[1].str(), .value = trim(m[2].str())};
                if (seen_first_heading)
                    flat.back().properties.push_back(std::move(p));
                else
                    doc.file_properties.push_back(std::move(p));
            }
            continue;
        }
        if (upper(trim(line)) == ":PROPERTIES:") {
            in_drawer = true;
            continue;
        }

        if (!seen_first_heading) {
            std::smatch m;
            if (std::regex_match(line, m, frontmatter_re())) {
                doc.keywords.push_back(keyword{.key = m[1].str(), .value = trim(m[2].str())});
                continue;
            }
        }

        std::smatch hm;
        if (std::regex_match(line, hm, heading_re())) {
            close_table_if_open();
            flush_bullets();
            seen_first_heading = true;
            flat_heading f;
            f.level = static_cast<unsigned int>(hm[1].str().size());
            f.title = trim(hm[2].str());
            flat.push_back(std::move(f));
            continue;
        }

        if (std::regex_match(line, table_sep_re()))
            continue;
        std::smatch tm;
        if (std::regex_match(line, tm, table_row_re())) {
            pending_table.push_back(split_row(tm[1].str()));
            continue;
        }
        close_table_if_open();

        std::smatch bm;
        if (std::regex_match(line, bm, bullet_re())) {
            pending_bullets.push_back(trim(bm[1].str()));
            if (seen_first_heading)
                flat.back().body_lines.push_back(line);
            continue;
        }
        flush_bullets();

        if (seen_first_heading)
            flat.back().body_lines.push_back(line);
    }
    close_table_if_open();
    flush_bullets();

    std::size_t idx = 0;
    doc.headings = build_level(flat, idx, 0);
    return doc;
}

domain::document parse_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    if (!in)
        throw std::filesystem::filesystem_error(
            "ores.orgmode: could not open file",
            path,
            std::make_error_code(std::errc::no_such_file_or_directory));
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return parse(buffer.str());
}

}
