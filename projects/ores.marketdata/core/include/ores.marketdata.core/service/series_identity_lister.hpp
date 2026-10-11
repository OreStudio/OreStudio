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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_IDENTITY_LISTER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_IDENTITY_LISTER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstddef>

namespace ores::marketdata::service {

/**
 * @brief Lists the typed identity of each series the caller can see.
 *
 * The identity is the one the curve reads take, so a screen passes a listed
 * identity back to the snapshot, slice or evolution read without building or
 * parsing an oresmd URI. The URI is read through the codec here, in the one place
 * that turns a stored series into the typed words, and the party is stated on the
 * identity so a choice names the series of the party it was listed under.
 *
 * A party reads only its own series, so the list is the caller's view. The view is the
 * identity projection's, which is isolated by party; the series table is isolated by
 * tenant alone. A series
 * the curve reads cannot name is left out: a fixing, and a series whose URI does
 * not read.
 */
class ORES_MARKETDATA_CORE_EXPORT series_identity_lister final {
public:
    /// The most series one answer holds. A caller that can see more is told the
    /// list was cut.
    static constexpr std::size_t max_series = 5000;

    /**
     * @brief Lists the series the caller can see, in a stable order.
     *
     * @param ctx Database context, already tenant- and party-scoped by the caller.
     * @param request Optionally the one party whose series are wanted.
     * @return The list. It never fails for want of series.
     */
    static messaging::list_series_identities_response
    list(ores::database::context ctx, const messaging::list_series_identities_request& request);
};

}

#endif
