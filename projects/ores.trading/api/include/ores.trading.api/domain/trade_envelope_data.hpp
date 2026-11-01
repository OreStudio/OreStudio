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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_DATA_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_ENVELOPE_DATA_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::domain {

/**
 * @brief One entry of the ORE trade envelope's AdditionalFields block.
 *
 * The ORE schema types AdditionalFields as xs:any, so the block is open:
 * a document carries whatever names it chose. A name and the text of the
 * element are the whole of an entry. The generated binding reads and
 * writes the block as name and value pairs, so nothing else survives it.
 */
struct trade_envelope_field final {
    std::string name;
    std::string value;
};

/**
 * @brief The ORE trade envelope: trade-level data that sits above the product.
 *
 * The envelope is keyed by the trade, not by the instrument, so one
 * carrier serves every product family. Each member is an optional
 * because the document distinguishes an element it states empty from one
 * it omits, and the writer re-emits on presence: a document that states
 * <Envelope/> or <PortfolioIds/> keeps that element.
 *
 * The four members are the four the schema declares. CounterParty,
 * PortfolioIds and AdditionalFields come across whole. The netting set
 * group is carried as its NettingSetId alone. The schema heads that
 * group with an abstract element and gives it two substitution-group
 * members: NettingSetId, a string, and NettingSetDetails, which holds a
 * NettingSetId of its own plus AgreementType, CallType,
 * InitialMarginType and LegalEntityId. This carrier has a home for the
 * first form only. No document in the corpus states the second, so the
 * bound is open and nothing exercises it.
 */
struct trade_envelope_data final {
    std::optional<std::string> counter_party;
    std::optional<std::string> netting_set_id;
    std::optional<std::vector<std::string>> portfolio_ids;
    std::optional<std::vector<trade_envelope_field>> additional_fields;
};

[[nodiscard]] constexpr auto wire_fields(const trade_envelope_field&) {
    return std::tuple{
        utility::codec::field{"name", &trade_envelope_field::name},
        utility::codec::field{"value", &trade_envelope_field::value},
    };
}

[[nodiscard]] constexpr auto wire_fields(const trade_envelope_data&) {
    return std::tuple{
        utility::codec::field{"counter_party", &trade_envelope_data::counter_party},
        utility::codec::field{"netting_set_id", &trade_envelope_data::netting_set_id},
        utility::codec::field{"portfolio_ids", &trade_envelope_data::portfolio_ids},
        utility::codec::field{"additional_fields", &trade_envelope_data::additional_fields},
    };
}

}

#endif
