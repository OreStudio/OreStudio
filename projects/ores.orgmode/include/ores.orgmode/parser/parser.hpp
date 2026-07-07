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
#ifndef ORES_ORGMODE_PARSER_PARSER_HPP
#define ORES_ORGMODE_PARSER_PARSER_HPP

#include "ores.orgmode/domain/document.hpp"
#include "ores.orgmode/export.hpp"
#include <filesystem>
#include <string>

namespace ores::orgmode::parser {

/**
 * @brief Parse org-mode source text into a document structure.
 *
 * Supports the restricted subset of org-mode this project's agile and
 * knowledge docs actually use — the same subset
 * `ores.codegen/src/codegen/org_loader.py` (`parse_org`) already
 * parses for codegen, which is this function's reference for scope:
 * frontmatter `#+key: value` lines, a `:PROPERTIES:` drawer (file-level
 * or per-heading), `* Title` headings of any depth, plain paragraph and
 * bullet-list body lines, pipe-delimited tables, and `[[id:UUID][text]]`
 * links found anywhere in a heading's body or table cells.
 *
 * Does not implement: TODO keywords, priority cookies, or tags as
 * separate fields (this project's docs don't rely on those being split
 * out of the title); babel source blocks; any other org-mode construct
 * outside the above list. Malformed input (an unterminated drawer, a
 * stray `:END:`) degrades gracefully rather than throwing — see the
 * corpus-wide parse test for what "gracefully" means in practice.
 */
ORES_ORGMODE_EXPORT domain::document parse(const std::string& text);

/**
 * @brief Read @p path and parse it. Throws std::filesystem::filesystem_error
 * if the file cannot be read.
 */
ORES_ORGMODE_EXPORT domain::document parse_file(const std::filesystem::path& path);

}

#endif
