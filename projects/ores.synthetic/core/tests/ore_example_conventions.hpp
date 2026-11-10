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
#include <string>

namespace ores::synthetic::tests {

/**
 * @brief The conventions of ORE's Products example the generated trades build
 * from: the standard CDS convention, the EUR six month Euribor and USD three
 * month Libor swaps, and the seed's inflation swaps of the indices the tests draw.
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
    auto libor = index;
    libor.id = "USD-LIBOR";
    libor.fixing_calendar = "US,UK";
    libor.oresmd_uri = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR";

    auto usd_swap = swap;
    usd_swap.id = "USD-LIBOR-3M-SWAP";
    usd_swap.fixed_calendar = "US,UK";
    usd_swap.fixed_frequency = "Semiannual";
    usd_swap.index = "USD-LIBOR-3M";
    usd_swap.oresmd_uri = "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M";

    // The seed's inflation swap conventions of the indices the tests draw.
    const auto inflation =
        [](std::string index, std::string calendar, std::string lag, bool interpolated) {
            refdata::domain::inflation_swap_convention row;
            row.id = index + "_INFLATIONSWAP";
            row.fix_calendar = calendar;
            row.fix_convention = "ModifiedFollowing";
            row.day_count_fraction = "30/360";
            row.index = index;
            row.interpolated = interpolated;
            row.observation_lag = std::move(lag);
            row.inflation_calendar = std::move(calendar);
            row.inflation_convention = "ModifiedFollowing";
            row.oresmd_uri = "oresmd://inflation/" + index + "?type=fixing&index=inflation";
            return row;
        };
    return {.ibor_indices = {index, libor},
            .swaps = {swap, usd_swap},
            .cds = {cds},
            .inflation_swaps = {inflation("UKRPIX", "UK", "2M", false),
                                inflation("USCPIA", "US", "3M", true),
                                inflation("BECPIHL", "TARGET", "3M", false)}};
}

}

#endif
