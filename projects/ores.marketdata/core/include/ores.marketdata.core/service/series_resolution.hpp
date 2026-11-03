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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_RESOLUTION_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_RESOLUTION_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/fixing_read_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <string>
#include <variant>

namespace ores::marketdata::service {

/**
 * @brief The one series an item names for a party, or the refusal of a read that cannot
 * name one.
 *
 * A read that names a party narrows the session to it, and only to a party the session
 * can see; one that names none reads across the parties the session sees, and a session
 * that sees none is refused, since an empty reply would read as an item with no values.
 * An item that more than one party or provider carries is refused as ambiguous, naming
 * both counts. @p entity names what is read, as fixings or observations, and @p item_name
 * what the item is, as a fixing item, in a refusal.
 */
ORES_MARKETDATA_CORE_EXPORT std::variant<repository::fixing_series, utility::domain::result>
resolve_series(ores::database::context ctx,
               const std::string& item,
               const std::string& provider_code,
               const std::string& party_id,
               const std::string& entity,
               const std::string& item_name);

}

#endif
