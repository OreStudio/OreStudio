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
#include "ores.orgmode.core/service/knowledge_index.hpp"
#include "ores.orgmode.parser/domain/heading.hpp"
#include "ores.orgmode.parser/parsing/parser.hpp"
#include <cctype>
#include <set>
#include <sstream>

namespace ores::orgmode::service {

namespace {

namespace org = ores::orgmode::parser::domain;

/**
 * @brief The tags of a #+filetags: value, ":domain:finance:" giving both.
 */
std::vector<std::string> split_tags(const std::string& filetags) {
    std::vector<std::string> out;
    std::stringstream ss(filetags);
    std::string tag;
    while (std::getline(ss, tag, ':')) {
        if (!tag.empty() && tag.find_first_not_of(" \t") != std::string::npos)
            out.push_back(tag);
    }
    return out;
}

std::string slug(const std::string& s) {
    std::string out;
    for (const unsigned char c : s) {
        if (std::isalnum(c))
            out += static_cast<char>(std::tolower(c));
        else if (!out.empty() && out.back() != '-')
            out += '-';
    }
    while (!out.empty() && out.back() == '-')
        out.pop_back();
    return out.empty() ? "section" : out;
}

/**
 * @brief The text with each org link replaced by what it shows: its
 * description, or its target when it has none.
 */
std::string without_links(const std::string& s) {
    std::string out;
    std::size_t pos = 0;
    while (pos < s.size()) {
        const auto open = s.find("[[", pos);
        if (open == std::string::npos)
            break;
        const auto close = s.find("]]", open);
        if (close == std::string::npos)
            break;
        out.append(s, pos, open - pos);
        const auto inner = s.substr(open + 2, close - open - 2);
        const auto split = inner.find("][");
        out += split == std::string::npos ? inner : inner.substr(split + 2);
        pos = close + 2;
    }
    out.append(s, pos);
    return out;
}

std::string section_text(const org::heading& h) {
    auto out = without_links(org::join_paragraph_lines(h.body_lines));
    for (const auto& t : h.tables) {
        for (const auto& row : t.rows) {
            for (const auto& cell : row) {
                if (cell.empty())
                    continue;
                if (!out.empty())
                    out += ' ';
                out += cell;
            }
        }
    }
    return out;
}

struct page_builder {
    const std::string& page_id;
    knowledge_index& index;
    std::set<std::string> anchors;
    std::int32_t next_ord = 1;

    std::string unique_anchor(std::string anchor) {
        if (anchors.insert(anchor).second)
            return anchor;
        for (int n = 2;; ++n) {
            auto candidate = anchor + "-" + std::to_string(n);
            if (anchors.insert(candidate).second)
                return candidate;
        }
    }

    void add(const org::heading& h, const std::string& parent_path) {
        const auto path = parent_path.empty() ? h.title : parent_path + " / " + h.title;
        const auto ord = next_ord++;
        index.sections.push_back({.page_id = page_id,
                                  .ord = ord,
                                  .anchor = unique_anchor(h.id ? *h.id : slug(path)),
                                  .heading_path = path,
                                  .level = static_cast<std::int32_t>(h.level),
                                  .text = section_text(h)});
        std::int32_t link_ord = 0;
        for (const auto& l : h.links) {
            index.links.push_back({.page_id = page_id,
                                   .section_ord = ord,
                                   .ord = link_ord++,
                                   .target_id = l.target_id,
                                   .text = l.text});
        }
        for (const auto& child : h.children)
            add(child, path);
    }
};

}

knowledge_index build_knowledge_index(const std::vector<domain::knowledge_page>& pages) {
    knowledge_index index;
    for (const auto& page : pages) {
        const auto doc = ores::orgmode::parser::parsing::parse(page.org);
        index_page p{.page_id = page.id,
                     .path = page.path,
                     .title = doc.find_keyword("title").value_or(page.path),
                     .description = doc.find_keyword("description").value_or(""),
                     .tags = split_tags(doc.find_keyword("filetags").value_or(""))};
        page_builder builder{.page_id = page.id, .index = index, .anchors = {""}};
        index.sections.push_back({.page_id = page.id,
                                  .ord = 0,
                                  .anchor = "",
                                  .heading_path = "",
                                  .level = 0,
                                  .text = p.description});
        index.pages.push_back(std::move(p));
        for (const auto& h : doc.headings)
            builder.add(h, "");
    }
    return index;
}

}
