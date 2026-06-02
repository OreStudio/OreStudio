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
#ifndef ORES_MARKETDATA_API_DOMAIN_ASSET_CLASS_HPP
#define ORES_MARKETDATA_API_DOMAIN_ASSET_CLASS_HPP

namespace ores::marketdata::domain {

/**
 * @brief Top-level asset class classification for a market series.
 *
 * Groups market series by the type of underlying asset. Used together with
 * series_subclass to enable efficient queries without parsing ORE key strings.
 */
enum class asset_class {
    fx,          ///< Foreign exchange rates, forwards and vol surfaces.
    rates,       ///< Interest rate curves, vol surfaces and spreads.
    credit,      ///< Credit spreads, hazard rates and recovery rates.
    equity,      ///< Equity spot prices, forwards, dividends and vol surfaces.
    commodity,   ///< Commodity spot prices, forwards and vol surfaces.
    inflation,   ///< Inflation swap rates, cap/floor vols and seasonality.
    bond,        ///< Bond prices and yield spreads.
    cross_asset  ///< Cross-asset correlations.
};

}

#endif
