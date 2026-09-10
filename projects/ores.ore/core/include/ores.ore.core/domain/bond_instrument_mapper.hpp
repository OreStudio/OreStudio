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
#ifndef ORES_ORE_CORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_BOND_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"

namespace ores::ore::domain {

/**
 * @brief Maps ORE XSD bond trade types to ORES domain rows and back.
 *
 * Handles the seven products the flattening mapper handled: Bond,
 * ForwardBond, CallableBond and ConvertibleBond over the plain
 * bondData; BondOption and BondTRS add their product's fact row on top
 * of the base bond fields; BondRepo stores the repo leg economics in
 * its fact row. BondFuture, BondPosition and Ascot keep no forward
 * mapper until the mapping task decides their coverage.
 *
 * Forward mapping retargets the storage the flattening mapper filled
 * into the wide legacy struct: the bondData fields land in the issue
 * row of the assembled bond_instrument_data, the per-product economics
 * in the fact row of the product, and the option exercise date in the
 * container remainder. Reverse reconstruction emits what the flattening
 * mapper emitted for each product; fields a fact row cannot carry yet
 * ride the container remainder.
 */
class ORES_ORE_CORE_EXPORT bond_instrument_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.bond_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static void map_bond_data(const bondData& bd, ores::trading::domain::bond_issue& issue);

    static bondData reverse_bond_data(const ores::trading::domain::bond_issue& issue);

public:
    static trading::domain::bond_instrument_data forward_bond(const trade& t);
    static trading::domain::bond_instrument_data forward_forward_bond(const trade& t);
    static trading::domain::bond_instrument_data forward_callable_bond(const trade& t);
    static trading::domain::bond_instrument_data forward_convertible_bond(const trade& t);
    static trading::domain::bond_instrument_data forward_bond_option(const trade& t);
    static trading::domain::bond_instrument_data forward_bond_trs(const trade& t);
    static trading::domain::bond_instrument_data forward_bond_repo(const trade& t);

    static trade reverse_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_forward_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_callable_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_convertible_bond(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_option(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_trs(const trading::domain::bond_instrument_data& data);
    static trade reverse_bond_repo(const trading::domain::bond_instrument_data& data);
};

}

#endif
