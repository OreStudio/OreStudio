/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_VERSION_HISTORY_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_VERSION_HISTORY_HPP

#include <string>
#include <vector>
#include "ores.refdata/domain/currency_version.hpp"

namespace ores::refdata::domain {

/**
 * @brief Contains the full version history for a currency.
 */
struct currency_version_history final {
    /**
     * @brief ISO code of the currency.
     */
    std::string iso_code;

    /**
     * @brief All versions of this currency, ordered from newest to oldest.
     */
    std::vector<currency_version> versions;
};

}

#endif
