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
#ifndef ORES_ORE_PLANNER_ORE_IMPORT_PLAN_HPP
#define ORES_ORE_PLANNER_ORE_IMPORT_PLAN_HPP

#include <vector>
#include "ores.ore/xml/importer.hpp"
#include "ores.refdata/domain/currency.hpp"
#include "ores.refdata/domain/portfolio.hpp"
#include "ores.refdata/domain/book.hpp"

namespace ores::ore::planner {

/**
 * @brief Complete, ready-to-execute import plan produced by ore_import_planner.
 *
 * All entities carry assigned UUIDs and fully stamped fields. The wizard
 * sends each vector in one batch save request.
 */
struct ore_import_plan {
    /**
     * @brief Currencies to save (filtered by import_choices::currency_mode).
     */
    std::vector<refdata::domain::currency>  currencies;

    /**
     * @brief Portfolios to save (in dependency order — parents before children).
     */
    std::vector<refdata::domain::portfolio> portfolios;

    /**
     * @brief Books to save.
     */
    std::vector<refdata::domain::book>      books;

    /**
     * @brief Trades to save, each paired with its raw ORE counterparty name.
     */
    std::vector<xml::trade_import_item>     trades;
};

}

#endif
