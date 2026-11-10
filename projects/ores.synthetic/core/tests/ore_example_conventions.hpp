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
#ifndef ORES_SYNTHETIC_CORE_TESTS_ORE_EXAMPLE_CONVENTIONS_HPP
#define ORES_SYNTHETIC_CORE_TESTS_ORE_EXAMPLE_CONVENTIONS_HPP

#include "ores.marketdata.api/conventions/convention_lookup.hpp"

namespace ores::synthetic::tests {

/**
 * @brief The conventions of ORE's Products example the generated trades build
 * from: the standard CDS convention and the EUR six month Euribor swap.
 */
inline marketdata::conventions::convention_set ore_example_conventions() {
    refdata::domain::cds_convention cds;
    cds.id = "CDS-STANDARD-CONVENTIONS";
    cds.calendar = "WeekendsOnly";
    cds.frequency = "Quarterly";
    cds.payment_convention = "Following";
    cds.rule = "CDS2015";
    cds.day_count_fraction = "ACT/360";
    cds.settles_accrual = true;
    cds.pays_at_default_time = true;

    refdata::domain::ibor_index_convention index;
    index.id = "EUR-EURIBOR";
    index.fixing_calendar = "TARGET";
    index.settlement_days = 2;
    index.day_count_fraction = "A360";
    index.business_day_convention = "MF";
    index.oresmd_uri = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR";

    refdata::domain::swap_convention swap;
    swap.id = "EUR-EURIBOR-6M-SWAP";
    swap.fixed_calendar = "TARGET";
    swap.fixed_frequency = "Annual";
    swap.fixed_convention = "MF";
    swap.fixed_day_count_fraction = "30/360";
    swap.index = "EUR-EURIBOR-6M";
    swap.oresmd_uri = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";
    return {.ibor_indices = {index}, .swaps = {swap}, .cds = {cds}};
}

}

#endif
