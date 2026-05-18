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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_CLASSIFICATION_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_CLASSIFICATION_HPP

#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.trading.api/domain/product_type.hpp"

namespace ores::trading::domain {

/**
 * @brief Product and instrument classification fields for a trade.
 *
 * One of five field-groups that compose trading::domain::trade via
 * rfl::Flatten<T>.  See trade_identity_field_group.json for the
 * rationale behind the sub-struct decomposition.
 * 
 * This group carries the product/instrument classification fields
 * (trade type, product type, instrument, asset class, netting set,
 * activity type, and status).
 */
struct trade_classification {
    /**
     * @brief Trade type code (e.g. Swap, FRA, Option).
     */
    std::string trade_type;

    /**
     * @brief Broad product classification enum.
     */
    domain::product_type product_type = domain::product_type::unknown;

    /**
     * @brief Optional instrument (soft FK to ores_refdata_instruments_tbl).
     */
    std::optional<boost::uuids::uuid> instrument_id;

    /**
     * @brief Optional asset class override (e.g. Rates, Credit, FX).
     */
    std::optional<std::string> asset_class;

    /**
     * @brief Netting set identifier for collateral/margin grouping.
     */
    std::string netting_set_id;

    /**
     * @brief Lifecycle activity code (e.g. New, Amendment, Novation, Termination).
     */
    std::string activity_type_code;

    /**
     * @brief Trade status (soft FK to ores_refdata_trade_statuses_tbl).
     */
    boost::uuids::uuid status_id;

};

}

#endif
