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
#ifndef ORES_ORE_CORE_DOMAIN_PORTFOLIO_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_PORTFOLIO_PROJECTION_HPP

#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/portfolio.hpp"
#include "ores.ore.core/xml/trade_import_item.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Projects an ORE Portfolio document onto the trading entities, and back.
 *
 * The document is one list of trades. Each trade states its type, an envelope
 * of names, and one product member, and the projection dispatches the product
 * to the family that holds it. The product tables carry the recursion: a
 * composite trade's components are index rows into the arena its record owns.
 *
 * An element the projection does not know is refused rather than dropped,
 * because a dropped envelope field would pass as a round trip while losing data.
 */
class ORES_ORE_CORE_EXPORT portfolio_projection {
public:
    using document = xml::generated::portfolio::portfolio;

    /**
     * @brief Reads a Portfolio document into the record the table describes.
     *
     * @param text The document.
     * @param origin The file the text came from, named in the failure.
     * @throws std::invalid_argument naming the origin and the line where the
     * document does not bind.
     */
    static document read(std::string_view text, std::string_view origin = {});

    /**
     * @brief Writes a Portfolio record as a document.
     *
     * @throws std::runtime_error when the record cannot produce one.
     */
    static std::string write(const document& v);

    /**
     * @brief Projects every trade the document states, in document order.
     *
     * The anchor's trade type and the ORE id are the projection's; the booking,
     * the party and the dates belong to the import planner.
     */
    static std::vector<xml::trade_import_item> map(const document& v);

    /**
     * @brief Builds a Portfolio document from the trades and the instruments.
     *
     * A trade whose instrument is in the batch is rebuilt through its family.
     * A trade with no instrument, or whose type no family handles, is written
     * as its type and its envelope, because the envelope is keyed by the trade
     * and not by the product.
     *
     * @throws std::runtime_error for a trade type the schema does not name.
     */
    static document reverse(const std::vector<trading::messaging::trade_export_item>& items,
                            const trading::domain::instrument_batch& instruments,
                            const portfolio_reference_curves& reference_curves = {});
};

}

#endif
