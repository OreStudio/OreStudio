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
#ifndef ORES_QT_IINSTRUMENTFORMPOPULATOR_HPP
#define ORES_QT_IINSTRUMENTFORMPOPULATOR_HPP

#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/fx_instrument_variant.hpp"
#include "ores.trading.api/domain/rates_instrument_variant.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include <vector>

namespace ores::qt {

/**
 * @brief Visitor interface for concrete instrument types on the display path.
 *
 * getTradeInstrument calls the matching populate() overload directly after
 * reading the concrete leaf struct from JSON — no trade_instrument variant is
 * ever constructed on the display path.
 *
 * All overloads are no-ops by default so forms only override the ones they handle.
 * With-legs types receive the leg collection alongside the instrument.
 */
struct IInstrumentFormPopulator {
    virtual ~IInstrumentFormPopulator() = default;

    // --- Flat types ---
    virtual void populate(const trading::domain::bond_instrument&) {}
    virtual void populate(const trading::domain::credit_instrument&) {}
    virtual void populate(const trading::domain::commodity_instrument&) {}
    virtual void populate(const trading::domain::scripted_instrument&) {}

    // --- Composite (with composite legs) ---
    virtual void populate(const trading::domain::composite_instrument&,
                          std::vector<trading::domain::composite_leg>) {}

    // --- FX types ---
    virtual void populate(const trading::domain::fx_forward_instrument&) {}
    virtual void populate(const trading::domain::fx_vanilla_option_instrument&) {}
    virtual void populate(const trading::domain::fx_barrier_option_instrument&) {}
    virtual void populate(const trading::domain::fx_digital_option_instrument&) {}
    virtual void populate(const trading::domain::fx_asian_forward_instrument&) {}
    virtual void populate(const trading::domain::fx_accumulator_instrument&) {}
    virtual void populate(const trading::domain::fx_variance_swap_instrument&) {}

    // --- Rates / swap types (with swap legs) ---
    virtual void populate(const trading::domain::fra_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::vanilla_swap_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::cap_floor_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::swaption_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::balance_guaranteed_swap_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::callable_swap_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::knock_out_swap_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::inflation_swap_instrument&,
                          std::vector<trading::domain::swap_leg>) {}
    virtual void populate(const trading::domain::rpa_instrument&,
                          std::vector<trading::domain::swap_leg>) {}

    // --- Equity types ---
    virtual void populate(const trading::domain::equity_option_instrument&) {}
    virtual void populate(const trading::domain::equity_forward_instrument&) {}
    virtual void populate(const trading::domain::equity_swap_instrument&) {}
    virtual void populate(const trading::domain::equity_variance_swap_instrument&) {}
    virtual void populate(const trading::domain::equity_barrier_option_instrument&) {}
    virtual void populate(const trading::domain::equity_asian_option_instrument&) {}
    virtual void populate(const trading::domain::equity_digital_option_instrument&) {}
    virtual void populate(const trading::domain::equity_accumulator_instrument&) {}
    virtual void populate(const trading::domain::equity_position_instrument&) {}
};

} // namespace ores::qt

#endif
