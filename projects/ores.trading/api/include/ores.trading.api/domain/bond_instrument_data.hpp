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
#ifndef ORES_TRADING_DOMAIN_BOND_INSTRUMENT_DATA_HPP
#define ORES_TRADING_DOMAIN_BOND_INSTRUMENT_DATA_HPP

#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/domain/bond_repo.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>

namespace ores::trading::domain {

/**
 * @brief The assembled bond instrument: the header row, its issue row and the product's fact row.
 *
 * Handcrafted carrier for the bond family, the way the rates family
 * assembles its generated part types with with_legs (instrument.hpp);
 * codegen absorbs it only when the parent story's container grammar
 * lands. The variant trade_instrument carries this type between import
 * and export, so it must hold every row the export path needs: the
 * slim instrument header (identity with the ten-code trade_type_code,
 * issue_id), the issue row with the bond terms map_bond_data reads,
 * and the fact row of the product (option, trs or repo). The
 * instrument.issue_id member pins issue.issue_id: one container is one
 * mapped trade with its own minted issue, and deduplication over
 * security_id happens at the database boundary. option_expiry_date
 * rides as a document remainder: the exercise date has no row in the
 * nine tables (bond_option carries only option_type and option_strike;
 * the dates land in the shared schedule tables of the parent story,
 * recorded scope limit of task D7943D7E wave 1.3), and reverse_bond_option
 * must keep emitting it.
 */
struct bond_instrument_data final {
    /**
     * @brief The instrument header row: identity, issue_id and audit.
     */
    bond_instrument instrument;

    /**
     * @brief The issue row holding the bond terms of this trade's security.
     */
    bond_issue issue;

    /**
     * @brief The bond_option fact row, engaged for BondOption products.
     */
    std::optional<bond_option> option;

    /**
     * @brief The bond_trs fact row, engaged for BondTRS products.
     */
    std::optional<bond_trs> trs;

    /**
     * @brief The bond_repo fact row, engaged for BondRepo products.
     */
    std::optional<bond_repo> repo;

    /**
     * @brief Option expiry date (ISO 8601), the exercise date reverse_bond_option emits.
     *
     * Document remainder: no row carries this field yet.
     */
    std::string option_expiry_date;
};

inline void stamp_ids(bond_instrument_data& data,
                      boost::uuids::uuid instrument_id,
                      boost::uuids::uuid trade_id) {
    stamp_ids(data.instrument, instrument_id, trade_id);
    if (data.option)
        data.option->instrument_id = instrument_id;
    if (data.trs)
        data.trs->instrument_id = instrument_id;
    if (data.repo)
        data.repo->instrument_id = instrument_id;
}

}

#endif
