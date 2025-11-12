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
#ifndef ORES_RISK_DOMAIN_CURRENCY_VERSION_HPP
#define ORES_RISK_DOMAIN_CURRENCY_VERSION_HPP

#include <string>
#include <vector>
#include <iosfwd>
#include "ores.risk/domain/currency.hpp"

namespace ores::risk::domain {

/**
 * @brief Represents a specific version of a currency with metadata.
 */
struct currency_version final {
    /**
     * @brief The currency data at this version.
     */
    currency data;

    /**
     * @brief Version number (1-based, higher is newer).
     */
    int version_number;

    /**
     * @brief Username of the person who created this version.
     */
    std::string modified_by;

    /**
     * @brief Timestamp when this version was created (ISO 8601 format).
     */
    std::string modified_at;

    /**
     * @brief Summary of changes made in this version.
     *
     * Examples: "Created currency", "Modified 2 fields", "Updated name and symbol"
     */
    std::string change_summary;
};

std::ostream& operator<<(std::ostream& s, const currency_version& v);

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

std::ostream& operator<<(std::ostream& s, const currency_version_history& v);

}

#endif
