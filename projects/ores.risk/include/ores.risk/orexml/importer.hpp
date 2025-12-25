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
#ifndef ORES_RISK_OREXML_IMPORTER_HPP
#define ORES_RISK_OREXML_IMPORTER_HPP

#include <vector>
#include <filesystem>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::orexml {

/**
 * @brief Imports domain objects from their ORE XML representation.
 */
class importer {
private:
    inline static std::string_view logger_name = "ores.risk.orexml.importer";

    static auto& lg() {
        using namespace ores::telemetry::log;
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
    static std::string validate_currency(const domain::currency& currency);

    static std::vector<domain::currency>
    import_currency_config(const std::filesystem::path& path);
};

}

#endif
