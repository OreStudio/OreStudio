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
#ifndef ORES_ORE_XML_EXPORTER_HPP
#define ORES_ORE_XML_EXPORTER_HPP

#include <string>
#include <vector>
#include <filesystem>
#include "ores.ore/export.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/domain/calendar_adjustment.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.ore/domain/conventions_mapper.hpp"

namespace ores::ore::xml {

struct roundtrip_summary {
    int       total_xml_files      = 0;
    int       skipped              = 0;
    int       output_files_written = 0;
    int       trades_mapped        = 0;
    int       trades_passthrough   = 0;
    int       currency_files       = 0;
    int       calendar_files       = 0;
    int       convention_files     = 0;
    long long import_ms            = 0;  ///< wall time inside importer calls
    long long export_ms            = 0;  ///< wall time inside exporter calls
    long long total_ms             = 0;  ///< wall time for the full roundtrip
};

/**
 * @brief Exports domain objects to their ORE XML representation.
 */
class ORES_ORE_EXPORT exporter {
private:
    inline static std::string_view logger_name = "ores.ore.xml.exporter";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static std::string
    export_currency_config(const std::vector<refdata::domain::currency>& v);

    static std::string export_calendar_adjustments(
        const std::vector<refdata::domain::calendar_adjustment>& v);

    static std::string export_conventions(
        const domain::mapped_conventions& mc);

    /**
     * @brief Reconstructs an ORE portfolio XML from a vector of
     * (trade, instrument) pairs returned by export_portfolio_response.
     *
     * Each item is reverse-mapped through the appropriate instrument mapper.
     * Items with monostate instruments (unmapped trade types) are skipped.
     */
    static std::string export_portfolio(
        const std::vector<trading::messaging::trade_export_item>& items);

    /**
     * @brief Walks input_dir recursively, roundtrips every supported ORE XML
     * through the import→export pipeline, and writes mirrored outputs under
     * output_dir. Detects Portfolio, CurrencyConfig, CalendarAdjustments, and
     * Conventions files by inspecting the first 4 KiB of each file.
     * No DB or network access; purely file-level.
     */
    static roundtrip_summary roundtrip(
        const std::filesystem::path& input_dir,
        const std::filesystem::path& output_dir);
};

}

#endif
