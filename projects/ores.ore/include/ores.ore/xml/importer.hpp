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
#ifndef ORES_ORE_XML_IMPORTER_HPP
#define ORES_ORE_XML_IMPORTER_HPP

#include <vector>
#include <filesystem>
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/domain/calendar_adjustment.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.ore/domain/trade_mapper.hpp"

namespace ores::ore::xml {

/**
 * @brief A trade with its ORE source context for import mapping.
 *
 * Pairs a partially-mapped ORES trading domain trade with the raw ORE
 * CounterParty string from the trade envelope and the path of the file it
 * came from. The counterparty_id in the trade is left nil; callers must
 * resolve it via ore_counterparty_name.
 *
 * The source_file field enables callers (e.g. batch directory import) to
 * report per-trade provenance without having to keep a separate index.
 */
struct trade_import_item {
    trading::domain::trade trade;
    std::string ore_counterparty_name;             ///< ORE CounterParty string, empty if absent
    std::filesystem::path source_file;             ///< ORE XML file this trade was read from
    domain::instrument_mapping_result instrument;  ///< monostate if trade type not yet mapped
};

/**
 * @brief Imports domain objects from their ORE XML representation.
 */
class importer {
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
    static std::string validate_calendar_adjustment(
        const refdata::domain::calendar_adjustment& ca);

    /**
     * @brief Imports calendar adjustments from an ORE calendaradjustment XML file.
     *
     * Reads additional holidays and business day overrides for named ORE
     * calendars. The file format is defined by @c calendaradjustment.xsd.
     *
     * @param path Path to the calendaradjustment.xml file
     * @return Vector of calendar adjustments, one per @c <Calendar> element
     */
    static std::vector<refdata::domain::calendar_adjustment>
    import_calendar_adjustments(const std::filesystem::path& path);

    /**
     * @brief Validates a trade against minimum import requirements.
     *
     * Checks that the trade has at least the fields that can be directly
     * mapped from ORE XML: external_id and trade_type. Fields that require
     * external mapping (book_id, counterparty_id, etc.) are not validated
     * here.
     *
     * @param trade Trade to validate
     * @return Empty string if valid, otherwise error message describing issues
     */
    static std::string validate_trade(const trading::domain::trade& trade);

    /**
     * @brief Imports trades from an ORE portfolio XML file.
     *
     * Parses the portfolio XML and maps each trade to the ORES trading
     * domain. UUID fields that require external context (book_id,
     * counterparty_id, portfolio_id, party_id) are left as nil and must be
     * populated by the calling code.
     *
     * @param path Path to the ORE portfolio XML file
     * @return Vector of partially-mapped trading domain trades
     */
    static std::vector<trading::domain::trade>
    import_portfolio(const std::filesystem::path& path);

    /**
     * @brief Imports trades from an ORE portfolio XML file with mapping context.
     *
     * Like import_portfolio() but also captures the raw ORE CounterParty
     * string from each trade envelope so that callers can present a
     * counterparty mapping dialog before resolving UUIDs.
     *
     * @param path Path to the ORE portfolio XML file
     * @return Vector of trade_import_item, each pairing a partially-mapped
     *         trade with its ORE counterparty name (empty if not present)
     */
    static std::vector<trade_import_item>
    import_portfolio_with_context(const std::filesystem::path& path);
};

}

#endif
