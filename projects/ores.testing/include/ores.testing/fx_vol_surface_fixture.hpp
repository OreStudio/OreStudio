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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TESTING_FX_VOL_SURFACE_FIXTURE_HPP
#define ORES_TESTING_FX_VOL_SURFACE_FIXTURE_HPP

#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_blueprint.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_pillar_source.hpp"
#include "ores.testing/uuid_of.hpp"
#include <boost/uuid/uuid.hpp>
#include <format>
#include <string>
#include <vector>

namespace ores::testing {

/**
 * @brief EUR-USD-FXOPTION as the reference data seeds it: delta-neutral ATM, spot delta up to
 * the 2Y switch and forward delta from it, risk reversals in favour of the call, broker
 * butterflies. A test of another delta convention names its two delta types.
 */
inline refdata::domain::fx_option_convention
eur_usd_fx_option_convention(std::string delta = "Spot", std::string long_term_delta = "Fwd") {
    refdata::domain::fx_option_convention c;
    c.id = "EUR-USD-FXOPTION";
    c.atm_type = "AtmDeltaNeutral";
    c.delta_type = std::move(delta);
    c.switch_tenor = "2Y";
    c.long_term_atm_type = "AtmDeltaNeutral";
    c.long_term_delta_type = std::move(long_term_delta);
    c.risk_reversal_in_favor_of = "Call";
    c.butterfly_style = "Broker";
    return c;
}

/// A surface blueprint with its pillars and, for each pillar, one source.
struct fx_vol_surface_rows final {
    refdata::domain::fx_vol_surface_blueprint blueprint;
    std::vector<refdata::domain::fx_vol_surface_pillar> pillars;
    std::vector<refdata::domain::fx_vol_surface_pillar_source> sources;
};

/**
 * @brief The EURUSD_SSVI blueprint of @p party, with a pillar for each expiry and label and each
 * pillar reading @p quotes_series as the derived contributor at weight 1.
 *
 * The ids are fixed, so two calls give the same rows. The grid is 1M, 6M and 2Y by the 25 delta
 * put, ATM and the 25 delta call.
 */
inline fx_vol_surface_rows eur_usd_ssvi_surface(const boost::uuids::uuid& party,
                                                const boost::uuids::uuid& quotes_series,
                                                const std::vector<std::string>& expiries,
                                                const std::vector<std::string>& labels) {
    fx_vol_surface_rows rows;
    auto& b = rows.blueprint;
    b.id = uuid_of("00000000-0000-4000-8000-0000000000c1");
    b.party_id = party;
    b.output_series_id = uuid_of("00000000-0000-4000-8000-0000000000d1");
    b.name = "EURUSD_SSVI";
    b.base_currency_code = "EUR";
    b.quote_currency_code = "USD";
    b.fx_option_convention_id = "EUR-USD-FXOPTION";
    b.smile_model = "ssvi";
    b.expiry_interpolation = "linear_total_variance";
    b.output_expiries = "1M,6M,2Y";
    b.output_strikes = "DEL/Spot/Put/25,ATM,DEL/Spot/Call/25";
    int n = 0;
    for (const auto& expiry : expiries)
        for (const auto& label : labels) {
            ++n;
            refdata::domain::fx_vol_surface_pillar p;
            p.id = uuid_of(std::format("00000000-0000-4000-8000-1{:011d}", n));
            p.party_id = party;
            p.blueprint_id = b.id;
            p.expiry_code = expiry;
            p.strike_label = label;
            refdata::domain::fx_vol_surface_pillar_source s;
            s.id = uuid_of(std::format("00000000-0000-4000-8000-2{:011d}", n));
            s.party_id = party;
            s.pillar_id = p.id;
            s.market_series_id = quotes_series;
            s.contributor_code = "derived";
            s.weight = 1.0;
            rows.pillars.push_back(std::move(p));
            rows.sources.push_back(std::move(s));
        }
    return rows;
}

}

#endif
