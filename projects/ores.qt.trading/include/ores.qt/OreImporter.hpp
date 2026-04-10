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
#ifndef ORES_QT_ORE_IMPORTER_HPP
#define ORES_QT_ORE_IMPORTER_HPP

#include <set>
#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/planner/import_choices.hpp"
#include "ores.ore/planner/ore_import_result.hpp"
#include "ores.ore/scanner/scan_result.hpp"
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Executes an ORE import: builds the plan, resolves name collisions,
 * and saves currencies, portfolios, books, trades, and instruments via NATS.
 *
 * Designed to run on a background thread. Returns ore_import_result which
 * carries per-instrument error details independently of the overall success flag.
 */
class OreImporter {
private:
    inline static std::string_view logger_name = "ores.qt.ore_importer";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreImporter(ClientManager* cm);

    /**
     * @brief Runs the full import pipeline and returns the result.
     *
     * @param scan_result       Output of ore_directory_scanner::scan().
     * @param existing_iso_codes ISO codes already present in the database.
     * @param choices           User-supplied options (target book, defaults, etc.).
     */
    ore::planner::ore_import_result execute(
        ore::scanner::scan_result scan_result,
        std::set<std::string> existing_iso_codes,
        ore::planner::import_choices choices);

private:
    ClientManager* cm_;
};

}

#endif
