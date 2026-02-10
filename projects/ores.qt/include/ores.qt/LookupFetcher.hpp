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
#ifndef ORES_QT_LOOKUP_FETCHER_HPP
#define ORES_QT_LOOKUP_FETCHER_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::qt {

class ClientManager;

/**
 * @brief Fixed party category values (foundation data, not server-fetched).
 */
namespace party_categories {
inline constexpr std::string_view operational = "operational";
inline constexpr std::string_view system = "system";
}

/**
 * @brief Result of fetching type and status lookup codes from the server.
 *
 * Used by party, counterparty, and tenant detail dialogs to populate
 * combo boxes.
 */
struct lookup_result {
    std::vector<std::string> type_codes;
    std::vector<std::string> status_codes;
};

/**
 * @brief Fetches party type and status codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vectors on failure.
 */
lookup_result fetch_party_lookups(ClientManager* cm);

/**
 * @brief Fetches tenant type and status codes from the server.
 *
 * Synchronous call intended to be run from within QtConcurrent::run.
 * Returns empty vectors on failure.
 */
lookup_result fetch_tenant_lookups(ClientManager* cm);

}

#endif
