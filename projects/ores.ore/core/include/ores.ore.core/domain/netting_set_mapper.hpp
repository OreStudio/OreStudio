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
#ifndef ORES_ORE_CORE_DOMAIN_NETTING_SET_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_NETTING_SET_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.refdata.api/domain/csa.hpp"
#include "ores.refdata.api/domain/csa_eligible_currency.hpp"
#include "ores.refdata.api/domain/netting_set.hpp"
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE NettingSetDefinitions document, mapped to the refdata entities.
 *
 * The three lists are the tables the document decomposes into: one netting set
 * per NettingSet, one CSA per set that carries CSADetails, and one eligible
 * currency per Currency of a CSA's EligibleCollaterals, in document order.
 */
struct mapped_netting_sets {
    std::vector<refdata::domain::netting_set> sets;
    std::vector<refdata::domain::csa> csas;
    std::vector<refdata::domain::csa_eligible_currency> eligible_currencies;
};

/**
 * @brief Maps between an ORE NettingSetDefinitions document and the refdata
 * netting set, CSA and eligible currency entities.
 *
 * ORE names a netting set by its id alone, so an imported set has no
 * agreement and no parties. A document whose NettingSetDetails names an
 * agreement type or a legal entity is refused: both must resolve to entities,
 * and the mapper has no store to resolve them against.
 */
class ORES_ORE_CORE_EXPORT netting_set_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.netting_set_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Maps an ORE NettingSetDefinitions document to the entities.
     */
    static mapped_netting_sets map(const nettingsetdefinitions& v);

    /**
     * @brief Reconstructs an ORE NettingSetDefinitions document from the
     * entities, sets in the order given and currencies by position.
     *
     * Every set states ActiveCSAFlag, false when it has no active CSA, as
     * every example document does. A CSA that states half of its independent
     * amount or one of its two margining frequencies is refused, because ORE
     * needs both and the mapper will not invent the other.
     */
    static nettingsetdefinitions reverse(const mapped_netting_sets& v);
};

}

#endif
