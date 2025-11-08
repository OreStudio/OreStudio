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
#ifndef ORES_RISK_CSV_CURRENCY_MAPPER_HPP
#define ORES_RISK_CSV_CURRENCY_MAPPER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include "ores.utility/log/make_logger.hpp"
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::csv {

/**
 * @brief Maps domain currency objects to structures suitable for CSV export.
 */
class currency_mapper {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.risk.csv.currency_mapper");
        return instance;
    }

public:
    static std::vector<domain::currency>
    map(const std::vector<domain::currency>& v);
};

}

#endif