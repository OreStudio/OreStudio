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
#ifndef ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_DOCUMENT_HPP
#define ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_DOCUMENT_HPP

#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/domain/todays_market_config.hpp"
#include "ores.analytics.api/domain/todays_market_configuration.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding.hpp"
#include "ores.analytics.api/domain/todays_market_entry.hpp"
#include <string>
#include <vector>

namespace ores::analytics::domain {

/**
 * @brief One ORE today's market document as the rows analytics stores.
 *
 * The header row and its child rows. Analytics stores and reads the document
 * whole; another component reaches it through analytics' operations.
 */
struct todays_market_document {
    todays_market_config config;
    std::vector<todays_market_collection> collections;
    std::vector<todays_market_entry> entries;
    std::vector<todays_market_configuration> configurations;
    std::vector<todays_market_configuration_binding> bindings;

    friend bool operator==(const todays_market_document&, const todays_market_document&) = default;
};

}

#endif
