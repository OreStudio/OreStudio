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
#ifndef ORES_RISK_OREXML_EXPORTER_HPP
#define ORES_RISK_OREXML_EXPORTER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::orexml {

/**
 * @brief Exports domain objects from their ORE XML representation.
 */
class exporter {
private:
    inline static std::string_view logger_name = "ores.risk.orexml.exporter";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static std::string
    export_currency_config(const std::vector<domain::currency>& v);
};

}

#endif
