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
#ifndef ORES_ORGMODE_DOMAIN_HEADING_HPP
#define ORES_ORGMODE_DOMAIN_HEADING_HPP

#include "ores.orgmode/domain/link.hpp"
#include "ores.orgmode/domain/property.hpp"
#include "ores.orgmode/domain/table.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::orgmode::domain {

/**
 * @brief One `* Title` heading and everything nested under it.
 *
 * Mirrors the shape `ores.compass`'s Python org parser
 * (`ores.codegen/src/codegen/org_loader.py`) already produces for this
 * project's docs — the reference for scope, not `dogen.org`'s richer
 * model (no TODO-keyword/priority/tag extraction here; this project's
 * docs keep those, when present, as part of the plain title text).
 */
struct heading final {
    /**
     * @brief Number of leading `*` characters.
     */
    unsigned int level = 0;

    /**
     * @brief Heading text after the stars, unparsed (may itself contain
     * a TODO keyword or trailing `:tags:` — this project's docs don't
     * rely on those being split out).
     */
    std::string title;

    /**
     * @brief The `:ID:` property's value, if this heading has one.
     */
    std::optional<std::string> id;

    /**
     * @brief All `:PROPERTIES:` drawer entries, `:ID:` included.
     */
    std::vector<property> properties;

    /**
     * @brief Plain body lines (paragraphs and bullet-list lines,
     * interleaved in original order) between this heading and its
     * first child heading or the next sibling.
     */
    std::vector<std::string> body_lines;

    /**
     * @brief Consecutive bullet-list runs, each as one list of items.
     */
    std::vector<std::vector<std::string>> bullet_lists;

    /**
     * @brief Org tables under this heading.
     */
    std::vector<domain::table> tables;

    /**
     * @brief `[[id:UUID][text]]` links found anywhere in this heading's
     * own body (not its children's) — new to `ores.orgmode`; `dogen.org`
     * has no link entity.
     */
    std::vector<link> links;

    /**
     * @brief Nested headings with a greater level than this one.
     */
    std::vector<heading> children;

    friend bool operator==(const heading&, const heading&) = default;
};

}

#endif
