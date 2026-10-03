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
#ifndef ORES_MARKETDATA_CORE_DATUM_ORE_INDEX_CODEC_HPP
#define ORES_MARKETDATA_CORE_DATUM_ORE_INDEX_CODEC_HPP

#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.core/export.hpp"
#include <expected>
#include <string>
#include <string_view>

namespace ores::marketdata::datum {

/**
 * @brief Reads ORE index names into market indices and writes them back.
 *
 * The reader follows ORE's parseIndex: it tries the families in ORE's order,
 * each by its prefix or token pattern, and splits the name into the fields
 * ORE's parser reads. It accepts by structure, as ORE does once a conventions
 * file defines an IBOR or inflation index, so CZK-CZEONIA reads without a list
 * of names.
 *
 * write(read(n)) is n, except for the spaced aliases of ORE's inflation indices,
 * such as UK RPI, which are written unspaced.
 *
 * A name ORE completes or corrects from outside the name is refused, because
 * the index could not write it back: an empty name after EQ-, BOND- or COMM-, a
 * power index with no delivery date, which ORE takes from the evaluation date,
 * and a power index with a DST flag, which ORE's own catalogue refuses.
 */
class ORES_MARKETDATA_CORE_EXPORT ore_index_codec final {
public:
    /// The index @p name names, or why ORE's grammar does not admit it.
    [[nodiscard]] static std::expected<market_index, std::string> read(std::string_view name);

    /// The ORE name of @p index.
    [[nodiscard]] static std::string write(const market_index& index);
};

}

#endif
