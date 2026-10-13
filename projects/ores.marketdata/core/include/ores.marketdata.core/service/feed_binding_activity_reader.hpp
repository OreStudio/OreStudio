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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_ACTIVITY_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FEED_BINDING_ACTIVITY_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <optional>
#include <string>

namespace ores::marketdata::service {

/**
 * @brief Says what each feed binding of the caller has written.
 *
 * A binding names a source and not a series, and the ingest loop keeps its tick count
 * and last tick in memory with no read. A source writes one series, and a series is
 * stamped with the source of the observations it holds, so the newest observation a
 * source has written names both the series the binding feeds and when the source
 * last ticked. The reader asks for that one observation for each binding, which the
 * observations_source index serves, and reads no series or axis of its own.
 *
 * A party reads only its own bindings and its own observations, so the answer is the
 * caller's view. A binding whose source has written nothing for the party stays in
 * the answer with no series and no tick, so the screen can say so.
 */
class ORES_MARKETDATA_CORE_EXPORT feed_binding_activity_reader final {
public:
    /**
     * @brief Reads the activity of each binding the caller can see, in the order of
     * the bindings' source names.
     *
     * @param ctx Database context, already tenant- and party-scoped by the caller.
     * @return The activity. It never fails for want of bindings.
     */
    static messaging::list_feed_binding_activity_response read(ores::database::context ctx);

    /**
     * @brief The activity of one binding, from the newest observation its source has
     * written, or from none.
     */
    static messaging::feed_binding_activity
    activity_of(const std::string& source_name,
                const std::optional<domain::market_observation>& newest);
};

}

#endif
