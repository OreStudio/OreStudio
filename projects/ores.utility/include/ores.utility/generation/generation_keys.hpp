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
#ifndef ORES_UTILITY_GENERATION_GENERATION_KEYS_HPP
#define ORES_UTILITY_GENERATION_GENERATION_KEYS_HPP

#include <string_view>

namespace ores::utility::generation {

/**
 * @brief Well-known keys for generation environment entries.
 */
struct generation_keys {
    static inline constexpr std::string_view modified_by = "modified_by";
    static inline constexpr std::string_view tenant_id = "tenant_id";
    static inline constexpr std::string_view party_id = "party_id";
    static inline constexpr std::string_view counterparty_id = "counterparty_id";
    static inline constexpr std::string_view account_id = "account_id";
    static inline constexpr std::string_view catalog_id = "catalog_id";
};

}

#endif
