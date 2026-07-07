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
#ifndef ORES_ORGMODE_DOMAIN_RESOLVED_TARGET_HPP
#define ORES_ORGMODE_DOMAIN_RESOLVED_TARGET_HPP

#include <string>

namespace ores::orgmode::domain {

/**
 * @brief Where an `id:` link points, as looked up in the org-roam index.
 *
 * `path` and `title` come straight from the org-roam `nodes` table.
 * `type` is not something org-roam stores in a queryable column (its
 * `CATEGORY` is the file's basename, not our `#+type:` keyword), so the
 * resolver gets it by parsing just the target file's frontmatter with
 * `ores.orgmode`'s own parser — a small, one-off cost per resolution,
 * not a second index.
 */
struct resolved_target final {
    std::string id;
    std::string path;
    std::string title;

    /**
     * @brief The target document's `#+type:` (e.g. "story", "task"),
     * empty if the file has no such keyword or could not be read.
     */
    std::string type;

    friend bool operator==(const resolved_target&, const resolved_target&) = default;
};

}

#endif
