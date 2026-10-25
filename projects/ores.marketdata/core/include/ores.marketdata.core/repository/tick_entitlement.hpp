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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_TICK_ENTITLEMENT_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_TICK_ENTITLEMENT_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.core/export.hpp"
#include <set>
#include <string>

namespace ores::marketdata::repository {

/**
 * @brief The providers whose ticks the session's party may read.
 *
 * The tick store is shared by every party of a provider and no policy filters it by party, so
 * the subscription is the door to it. The ticks are raw values, so the subscription must hold
 * display or non-display: a derived right alone lets a party compute new values from the data
 * and not read them. A session with no party holds nothing.
 *
 * One read of the subscriptions answers it, so a caller that reads many series asks once. The
 * codes come back so that a read of the ticks of many series does not look one up for each.
 */
ORES_MARKETDATA_CORE_EXPORT std::set<std::string>
readable_providers(const ores::database::context& ctx);

/**
 * @brief Whether the session's party may read the ticks of @p provider_code.
 */
ORES_MARKETDATA_CORE_EXPORT bool may_read_ticks(const ores::database::context& ctx,
                                                const std::string& provider_code);

}

#endif
