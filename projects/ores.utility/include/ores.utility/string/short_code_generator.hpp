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
#ifndef ORES_UTILITY_STRING_SHORT_CODE_GENERATOR_HPP
#define ORES_UTILITY_STRING_SHORT_CODE_GENERATOR_HPP

#include <string>
#include <unordered_set>

namespace ores::utility::string {

/**
 * @brief Strips corporate/legal suffixes from an entity name.
 *
 * Mirrors the SQL function ores_utility_strip_corporate_suffix_fn.
 * Removes dotted abbreviations (S.A., B.V., etc.) and word suffixes
 * (PLC, LLC, LTD, INC, CORP, etc.).
 */
std::string strip_corporate_suffixes(const std::string& name);

/**
 * @brief Generates a 3-6 character mnemonic short code from an entity name.
 *
 * Reimplements the SQL function ores_utility_generate_short_code_fn:
 *   1. Strip corporate suffixes.
 *   2. Uppercase, remove non-alpha (keep spaces).
 *   3. Single word: first letter + consonants, up to 6.
 *   4. Two words: 3+3.
 *   5. Three+ words: 2+2+2.
 *   6. Pad to minimum 3 chars with 'X'.
 */
std::string generate_short_code(const std::string& name);

/**
 * @brief Generates a unique short code, appending numeric suffixes on collision.
 *
 * Mirrors the deduplication pattern in dq_lei_counterparties_publish_create.sql.
 * If the base code already exists in @p used_codes, appends 2, 3, ... until
 * unique. Inserts the final code into @p used_codes.
 */
std::string generate_unique_short_code(const std::string& name,
    std::unordered_set<std::string>& used_codes);

}

#endif
