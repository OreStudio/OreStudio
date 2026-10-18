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
#ifndef ORES_ORE_CORE_XML_TRADE_IMPORT_ITEM_HPP
#define ORES_ORE_CORE_XML_TRADE_IMPORT_ITEM_HPP

#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include <filesystem>
#include <optional>
#include <string>

namespace ores::ore::xml {

/**
 * @brief A trade read from an ORE document, ready to book.
 *
 * Carries what the import books: the trade's anchor and booking, the
 * activity that books it, and its ORE id, which the import writes as the
 * trade's ORE identifier. The projection fills the ORE id and the trade type;
 * the planner mints the trade id and fills the party, the book and the
 * dates. The counterparty and the netting set are resolved from the
 * envelope when the trade is booked.
 *
 * The source file lets a batch import report each trade's provenance
 * without keeping a separate index.
 */
struct trade_import_item {
    std::string ore_id;
    trading::domain::trade anchor;
    trading::domain::trade_booking booking;
    std::string activity_type_code = "new_booking";
    std::optional<trading::domain::trade_envelope_data> envelope;
    std::filesystem::path source_file;
    trading::domain::trade_instrument instrument;
};

}

#endif
