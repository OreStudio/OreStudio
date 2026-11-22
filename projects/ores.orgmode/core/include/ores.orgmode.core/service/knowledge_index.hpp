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
#ifndef ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_INDEX_HPP
#define ORES_ORGMODE_CORE_SERVICE_KNOWLEDGE_INDEX_HPP

#include "ores.orgmode.api/domain/knowledge_page.hpp"
#include "ores.orgmode.core/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstdint>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::orgmode::service {

/**
 * @brief One loaded page, as the index describes it.
 */
struct index_page final {
    std::string page_id;
    std::string path;
    std::string title;
    std::string description;
    std::vector<std::string> tags;

    friend bool operator==(const index_page&, const index_page&) = default;
};

/**
 * @brief One searchable part of a page.
 *
 * Order 0 is the page itself, its text the page's description, so a page with
 * no headings is still found. Each heading follows in document order, its text
 * its own body and tables, not its children's.
 */
struct index_section final {
    std::string page_id;
    std::int32_t ord = 0;
    std::string anchor;
    std::string heading_path;
    std::int32_t level = 0;
    std::string text;

    friend bool operator==(const index_section&, const index_section&) = default;
};

/**
 * @brief One link from a section to another page, by the target's org id.
 */
struct index_link final {
    std::string page_id;
    std::int32_t section_ord = 0;
    std::int32_t ord = 0;
    std::string target_id;
    std::string text;

    friend bool operator==(const index_link&, const index_link&) = default;
};

/**
 * @brief The whole index, in the shape ores_orgmode_replace_knowledge_index_fn
 * reads.
 */
struct knowledge_index final {
    std::vector<index_page> pages;
    std::vector<index_section> sections;
    std::vector<index_link> links;

    friend bool operator==(const knowledge_index&, const knowledge_index&) = default;
};

/**
 * @brief Parses each page and derives its index rows.
 *
 * Pure: the same pages give the same index, which is what lets the indexing
 * step run again without a reload.
 */
[[nodiscard]] ORES_ORGMODE_CORE_EXPORT knowledge_index
build_knowledge_index(const std::vector<domain::knowledge_page>& pages);

[[nodiscard]] constexpr auto wire_fields(std::type_identity<index_page>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"page_id", &index_page::page_id},
        field{"path", &index_page::path},
        field{"title", &index_page::title},
        field{"description", &index_page::description},
        field{"tags", &index_page::tags},
    };
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<index_section>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"page_id", &index_section::page_id},
        field{"ord", &index_section::ord},
        field{"anchor", &index_section::anchor},
        field{"heading_path", &index_section::heading_path},
        field{"level", &index_section::level},
        field{"text", &index_section::text},
    };
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<index_link>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"page_id", &index_link::page_id},
        field{"section_ord", &index_link::section_ord},
        field{"ord", &index_link::ord},
        field{"target_id", &index_link::target_id},
        field{"text", &index_link::text},
    };
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<knowledge_index>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"pages", &knowledge_index::pages},
        field{"sections", &knowledge_index::sections},
        field{"links", &knowledge_index::links},
    };
}

}

#endif
