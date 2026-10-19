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
#ifndef ORES_ORE_CORE_DOMAIN_TODAYS_MARKET_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_TODAYS_MARKET_PROJECTION_HPP

#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/todays_market.hpp"
#include <string>
#include <string_view>

namespace ores::ore::domain {

/**
 * @brief Projects an ORE TodaysMarket record into the analytics today's market
 * entities.
 *
 * The record holds twenty-four collections, and they are four shapes rather
 * than twenty-four: eighteen entries keyed by a plain =name=, three keyed by
 * another attribute (=currency= or =pair=), two keyed twice by an optional
 * =key= and an optional =currency=, and =SwapIndexCurves=, keyed by =name= with
 * no reference text because its value is its nested =Discounting= child.
 *
 * A collection name or a binding name that is not one of the twenty-four is an
 * error on the way back, not a skip, so a bad row cannot pass as a round trip.
 */
class ORES_ORE_CORE_EXPORT todays_market_projection {
public:
    using document = xml::generated::todays_market::todaysmarket;

    /**
     * @brief Maps an ORE TodaysMarket record to the analytics entities.
     */
    static ores::analytics::messaging::todays_market_document map(const document& v);

    /**
     * @brief Reconstructs an ORE TodaysMarket record from mapped entities.
     *
     * Every collection and every configuration is emitted in =position= order,
     * and so is every entry within its collection, because a collection may
     * name the same key twice and a reference cannot be sorted back into
     * document order.
     *
     * @throws std::runtime_error for a collection or a binding the document
     * has no place for, and for an entry that lacks the key its collection
     * requires.
     */
    static document reverse(const ores::analytics::messaging::todays_market_document& v);

    /**
     * @brief Reads a TodaysMarket document into the record the table describes.
     *
     * @param text The document.
     * @param origin The file the text came from, named in the failure.
     * @throws std::invalid_argument naming the origin and the line where the
     * document does not bind.
     */
    static document read(std::string_view text, std::string_view origin = {});

    /**
     * @brief Writes a TodaysMarket record as a document.
     *
     * @throws std::runtime_error when the record cannot produce one.
     */
    static std::string write(const document& v);
};

}

#endif
