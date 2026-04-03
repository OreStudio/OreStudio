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
#ifndef ORES_ORE_DOMAIN_CONVENTIONS_MAPPER_HPP
#define ORES_ORE_DOMAIN_CONVENTIONS_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.refdata.api/domain/zero_convention.hpp"
#include "ores.refdata.api/domain/deposit_convention.hpp"
#include "ores.refdata.api/domain/swap_convention.hpp"
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/fra_convention.hpp"
#include "ores.refdata.api/domain/ibor_index_convention.hpp"
#include "ores.refdata.api/domain/overnight_index_convention.hpp"
#include "ores.refdata.api/domain/fx_convention.hpp"
#include "ores.refdata.api/domain/cds_convention.hpp"

namespace ores::ore::domain {

/**
 * @brief All convention types extracted from a single ORE conventions.xml file.
 *
 * The nine fields correspond to the nine convention categories currently
 * modelled in the ORES refdata domain. ORE conventions.xml contains additional
 * types (AverageOIS, TenorBasisSwap, CrossCurrencyBasis, InflationSwap, etc.)
 * that are not yet modelled and are silently skipped during import.
 */
struct mapped_conventions {
    std::vector<refdata::domain::zero_convention>            zero;
    std::vector<refdata::domain::deposit_convention>         deposit;
    std::vector<refdata::domain::swap_convention>            swap;
    std::vector<refdata::domain::ois_convention>             ois;
    std::vector<refdata::domain::fra_convention>             fra;
    std::vector<refdata::domain::ibor_index_convention>      ibor_index;
    std::vector<refdata::domain::overnight_index_convention> overnight_index;
    std::vector<refdata::domain::fx_convention>              fx;
    std::vector<refdata::domain::cds_convention>             cds;
};

/**
 * @brief Maps between ORE XML convention types and refdata domain types.
 *
 * The mapper normalises all ORE enum aliases (e.g. "F", "Following",
 * "FOLLOWING") to canonical FpML/CDM codes before storing them in the domain
 * types. Individual @c map_* methods are exposed for unit testing.
 */
class conventions_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.conventions_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Normalisation helpers — collapse ORE aliases to canonical codes.
    static std::string normalize_bdc(domain::businessDayConvention v);
    static std::string normalize_day_counter(domain::dayCounter v);
    static std::string normalize_frequency(domain::frequencyType v);
    static std::string normalize_compounding(domain::compounding v);
    static std::string normalize_date_rule(domain::dateRule v);
    static bool parse_bool(domain::bool_ v);

public:
    /**
     * @brief Maps all recognised convention types from an ORE conventions doc.
     */
    static mapped_conventions map(const conventions& v);

    static refdata::domain::zero_convention
    map_zero(const zeroType& v);

    static refdata::domain::deposit_convention
    map_deposit(const depositType& v);

    static refdata::domain::swap_convention
    map_swap(const swapType& v);

    static refdata::domain::ois_convention
    map_ois(const oisType& v);

    static refdata::domain::fra_convention
    map_fra(const fraType& v);

    static refdata::domain::ibor_index_convention
    map_ibor_index(const iborIndexType& v);

    static refdata::domain::overnight_index_convention
    map_overnight_index(const overnightIndexType& v);

    static refdata::domain::fx_convention
    map_fx(const fxType& v);

    static refdata::domain::cds_convention
    map_cds(const cdsConventionsType& v);
};

}

#endif
