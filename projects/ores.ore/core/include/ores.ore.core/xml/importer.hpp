/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_XML_IMPORTER_HPP
#define ORES_ORE_CORE_XML_IMPORTER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief A trade read from an ORE document, ready to book.
 *
 * Carries what the import books: the trade's anchor and booking, the
 * activity that books it, and its ORE id, which the import writes as the
 * trade's ORE identifier. The importer fills the ORE id and the trade type;
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

/**
 * @brief Imports domain objects from their ORE XML representation.
 */
class ORES_ORE_CORE_EXPORT importer {
private:
    inline static std::string_view logger_name = "ores.ore.xml.importer";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Validates a currency against XSD schema requirements.
     *
     * Performs lightweight validation checking required fields per
     * assets/xsds/currencyconfig.xsd without requiring external libraries.
     *
     * @param currency Currency to validate
     * @return Empty string if valid, otherwise error message describing issues
     */
    static std::string validate_currency(const refdata::domain::currency& currency);

    static std::vector<refdata::domain::currency>
    import_currency_config(const std::filesystem::path& path);

    /**
     * @brief Validates a calendar adjustment entry.
     *
     * @param ca Calendar adjustment to validate
     * @return Empty string if valid, otherwise error message
     */
    static std::string
    validate_calendar_adjustment(const refdata::messaging::calendar_adjustment& ca);

    /**
     * @brief Imports calendar adjustments from an ORE calendaradjustment XML file.
     *
     * Reads additional holidays and business day overrides for named ORE
     * calendars. The file format is defined by @c calendaradjustment.xsd.
     *
     * @param path Path to the calendaradjustment.xml file
     * @return Vector of calendar adjustments, one per @c <Calendar> element
     */
    static std::vector<refdata::messaging::calendar_adjustment>
    import_calendar_adjustments(const std::filesystem::path& path);

    /**
     * @brief Imports all recognised conventions from an ORE conventions XML file.
     *
     * Parses the ORE @c conventions.xml and maps the nine supported convention
     * categories (Zero, Deposit, Swap, OIS, FRA, IborIndex, OvernightIndex,
     * FX, CDS) to ORES refdata domain types. Additional ORE convention types
     * (AverageOIS, TenorBasisSwap, CrossCurrencyBasis, InflationSwap, etc.)
     * are present in the file but are not yet modelled and are silently
     * skipped.
     *
     * @param path Path to the conventions.xml file
     * @return @c ores::refdata::domain::conventions_document struct containing one vector per
     * convention type
     */
    static ores::refdata::domain::conventions_document
    import_conventions(const std::filesystem::path& path);

    /**
     * @brief Imports trades from an ORE portfolio XML file with mapping context.
     *
     * Parses the portfolio XML, maps each trade to the ORES trading domain,
     * and captures the raw ORE CounterParty string from each trade envelope.
     * All UUID fields (trade.id, trade_id, book_id, etc.) are left nil;
     * the caller (e.g. ore_import_planner) is responsible for minting UUIDs
     * via trading::domain::stamp_ids and populating context fields.
     *
     * @param path Path to the ORE portfolio XML file
     * @return Vector of trade_import_item with product_type set, all UUIDs nil
     */
    static std::vector<trade_import_item>
    import_portfolio_with_context(const std::filesystem::path& path);
};

}

#endif
