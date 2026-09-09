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
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The reshaped bond instrument: one trade of the bond family, keyed by instrument_id with
 * the ten-code trade_type_code check and the issue_id foreign key to the bond issue.
 *
 * One row per bond trade, reshaped from the wide legacy table per the
 * bond relational model deliverable
 * (doc/knowledge/architecture/trading_bond_relational_model.org). The
 * row carries the instrument's identity only: instrument_id,
 * trade_type_code over the ten bond codes, party_id, the optional
 * trade_id soft link, and issue_id, the NOT NULL foreign key to the
 * bond issue row that holds every term of the bond. The economics
 * moved to the issue (one row per ISIN, shared by every instrument of
 * it), so an amendment to a term touches the issue row once, not every
 * open instrument (finding B11 of the deliverable). A product change is
 * a cancel and a rebook: the service closes the row's validity and
 * inserts a new instrument row with its fact rows; trade_type_code
 * never changes in place (review answer 2).
 *
 * The ten codes, in seed order (trading_trade_types_populate.sql):
 * Bond, ForwardBond, BondFuture, BondOption, BondRepo, BondTRS,
 * BondPosition, CallableBond, ConvertibleBond, Ascot. The trade_type_code
 * check is the membership check against ores_trading_trade_types_tbl
 * through ores_trading_validate_trade_type_fn, the mechanism every
 * generated trading instrument uses, plus the in-list coverage check
 * over the ten codes below. For Bond, ForwardBond, CallableBond,
 * ConvertibleBond and BondPosition the issue is the bond itself; for
 * BondRepo the issue is the collateral the financing runs against; for
 * BondOption, BondFuture, BondTRS and Ascot the issue is the bond the
 * product is written on (review answers 3 and 4).
 */
struct bond_instrument final {
    instrument_identity identity;

    /**
     * @brief UUID of the bond issue this instrument trades (FK to ores_trading_bond_issues_tbl).
     *
     * NOT NULL: a bond product row with no issue is not a bond product. The issue holds the bond's
     * terms; the instrument row reads them through a temporal join on this key, so an amended term
     * reads as of the instrument row's own validity.
     */
    boost::uuids::uuid issue_id;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for bond_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_instrument&) {
    return "ores.trading.bond_instrument";
}

}

#endif
