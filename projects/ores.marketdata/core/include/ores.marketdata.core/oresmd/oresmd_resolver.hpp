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
#ifndef ORES_MARKETDATA_CORE_ORESMD_ORESMD_RESOLVER_HPP
#define ORES_MARKETDATA_CORE_ORESMD_ORESMD_RESOLVER_HPP

#include "ores.marketdata.api/domain/market_data_identifier.hpp"
#include "ores.marketdata.api/domain/market_data_requirement.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::core {

/**
 * @brief Resolves a market_data_requirement (a logical, possibly-partial description)
 * into a fully-resolved market_data_identifier, per
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1's "Logical vs. physical" section.
 *
 * @p defaults supplies whatever additional information narrows the requirement (an
 * explicit choice among the fields still open) and is expected to be the same asset
 * class as @p requirement, though this isn't a precondition whose violation is
 * undefined: a mismatched @p defaults is simply treated as absent (via std::get_if),
 * so every field the requirement itself leaves unset falls straight through to the
 * "unresolved" case below. Every field the requirement leaves unset is otherwise taken
 * from @p defaults; a field left unset in both (accounting for the mismatch case above)
 * throws oresmd_exception naming it, rather than silently defaulting a mandatory field.
 */
class ORES_MARKETDATA_CORE_EXPORT oresmd_resolver final {
public:
    [[nodiscard]] static domain::market_data_identifier
    resolve(const domain::market_data_requirement& requirement,
            const domain::market_data_identifier& defaults);
};

}

#endif
