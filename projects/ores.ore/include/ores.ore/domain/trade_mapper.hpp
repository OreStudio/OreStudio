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
#ifndef ORES_ORE_DOMAIN_TRADE_MAPPER_HPP
#define ORES_ORE_DOMAIN_TRADE_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.trading/domain/trade.hpp"
#include "ores.ore/domain/domain.hpp"

namespace ores::ore::domain {

/**
 * @brief Maps ORE XML trade types to trading domain entities.
 *
 * Performs a partial mapping from the ORE XSD portfolio/trade structure to the
 * ORES trading domain. Fields that require external context to resolve (e.g.
 * book_id, counterparty_id, portfolio_id) are left as nil UUIDs and must be
 * populated by the calling code (typically a mapping dialog or import
 * configuration).
 *
 * Fields mapped directly:
 * - trade.id -> external_id
 * - trade.TradeType -> trade_type (enum to string)
 * - Envelope.NettingSetId -> netting_set_id
 * - lifecycle_event set to "New"
 * - Audit fields (modified_by, change_reason_code, change_commentary)
 *
 * Fields left as nil (require external mapping):
 * - book_id (ORE has no book concept)
 * - portfolio_id (ORE PortfolioIds are string labels, not ORES UUIDs)
 * - counterparty_id (ORE CounterParty is a string name, not an ORES UUID)
 * - party_id (derived from book_id in ORES)
 */
class trade_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.trade_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Maps a single ORE XSD trade to an ORES trading domain trade.
     */
    static trading::domain::trade map(const trade& v);

    /**
     * @brief Maps an ORE portfolio (collection of trades) to a vector of ORES
     * trading domain trades.
     */
    static std::vector<trading::domain::trade> map(const portfolio& v);
};

}

#endif
