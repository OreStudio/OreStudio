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
#ifndef ORES_ORGMODE_DOMAIN_DOCUMENT_HPP
#define ORES_ORGMODE_DOMAIN_DOCUMENT_HPP

#include "ores.orgmode/domain/heading.hpp"
#include "ores.orgmode/domain/keyword.hpp"
#include "ores.orgmode/domain/property.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::orgmode::domain {

/**
 * @brief A parsed `.org` file: frontmatter, any file-level property
 * drawer, and the tree of top-level headings.
 */
struct document final {
    /**
     * @brief `#+key: value` lines before the first heading, in file
     * order. Duplicate keys keep every occurrence (unlike a map).
     */
    std::vector<keyword> keywords;

    /**
     * @brief A `:PROPERTIES:` drawer appearing before the first
     * heading (rare; most docs put `:ID:` here and nothing else).
     */
    std::vector<property> file_properties;

    /**
     * @brief Top-level headings (level 1, normally — a document with
     * no headings at all has an empty list).
     */
    std::vector<heading> headings;

    friend bool operator==(const document&, const document&) = default;

    /**
     * @brief First keyword value matching @p key (case-sensitive), if any.
     *
     * Convenience for the common lookups: `find_keyword("type")`,
     * `find_keyword("title")`. org-mode itself treats keywords
     * case-insensitively, but this project's docs consistently write
     * them lowercase (`#+type:`, not `#+TYPE:`), so an exact match is
     * enough in practice; a doc that broke that convention would just
     * look up as not found rather than erroring.
     */
    [[nodiscard]] std::optional<std::string> find_keyword(const std::string& key) const {
        for (const auto& kw : keywords) {
            if (kw.key == key)
                return kw.value;
        }
        return std::nullopt;
    }

    /**
     * @brief The file-level `:ID:` property, if present.
     *
     * Case-sensitive exact match on `"ID"`, same caveat as
     * `find_keyword` — this project's docs are consistently uppercase
     * here.
     */
    [[nodiscard]] std::optional<std::string> id() const {
        for (const auto& p : file_properties) {
            if (p.key == "ID")
                return p.value;
        }
        return std::nullopt;
    }
};

}

#endif
