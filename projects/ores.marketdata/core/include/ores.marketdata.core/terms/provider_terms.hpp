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
#ifndef ORES_MARKETDATA_CORE_TERMS_PROVIDER_TERMS_HPP
#define ORES_MARKETDATA_CORE_TERMS_PROVIDER_TERMS_HPP

#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <string>

namespace ores::marketdata::terms {

/**
 * @brief Whether the provider states terms the system must hold before it takes data: a
 * provider with a terms page. A vendor whose licence is a contract outside the system has
 * none.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT bool has_terms(const domain::provider& p);

/**
 * @brief The digest of the provider's terms as they stand: the SHA-256 of the terms page,
 * the credit and the storage permission.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT std::string terms_digest(const domain::provider& p);

/**
 * @brief Whether the terms that stand were accepted. A change of the page, the credit or the
 * storage permission since the acceptance leaves them unaccepted.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT bool terms_accepted(const domain::provider& p);

/**
 * @brief Whether the provider's data may be fetched and stored: always for a provider with
 * no terms, and for one with terms only when they are accepted and allow storage.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT bool may_fetch(const domain::provider& p);

/**
 * @brief The provider with its terms as they stand accepted by @p who at @p when.
 */
[[nodiscard]] ORES_MARKETDATA_CORE_EXPORT domain::provider
accept_terms(domain::provider p, std::string who, std::chrono::system_clock::time_point when);

}

#endif
