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
#ifndef ORES_ORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES bond domain type.
 */
struct bond_mapping_result {
    ores::trading::domain::bond_instrument instrument;
};

/**
 * @brief Maps ORE XSD bond trade types to ORES domain types and back.
 *
 * Handles:
 *   - Bond              (BondData)
 *   - ForwardBond       (ForwardBondData.BondData)
 *   - CallableBond      (CallableBondData.BondData)
 *   - ConvertibleBond   (ConvertibleBondData.BondData)
 *   - BondOption        (BondOptionData)
 *   - BondTRS           (BondTRSData)
 *
 * The first four types share the same bondData structure; BondOption and
 * BondTRS add option/TRS-specific fields on top of the base bond fields.
 *
 * Forward mapping captures the economic fields stored in the ORES relational
 * model. Fields not yet modelled are silently dropped; gaps are reported by
 * ore_coverage_check.py (Thing 2).
 *
 * Reverse mapping reconstructs ORE types from the fields captured by the
 * forward mapping.
 */
class bond_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.bond_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static void map_bond_data(const bondData& bd,
        ores::trading::domain::bond_instrument& instr);

    static bondData reverse_bond_data(
        const ores::trading::domain::bond_instrument& instr);

public:
    static bond_mapping_result forward_bond(const trade& t);
    static bond_mapping_result forward_forward_bond(const trade& t);
    static bond_mapping_result forward_callable_bond(const trade& t);
    static bond_mapping_result forward_convertible_bond(const trade& t);
    static bond_mapping_result forward_bond_option(const trade& t);
    static bond_mapping_result forward_bond_trs(const trade& t);
    static bond_mapping_result forward_bond_repo(const trade& t);

    static trade reverse_bond(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_forward_bond(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_callable_bond(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_convertible_bond(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_bond_option(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_bond_trs(
        const ores::trading::domain::bond_instrument& instr);
    static trade reverse_bond_repo(
        const ores::trading::domain::bond_instrument& instr);
};

}

#endif
