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
#ifndef ORES_MARKETDATA_CORE_SERVICE_SERIES_TICK_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_SERIES_TICK_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>
#include <cstddef>

namespace ores::marketdata::service {

/**
 * @brief Reads every tick of one series in a span of time, in one reply.
 *
 * The reader decides three things before it reads: the session has a party, the span does not
 * end before it starts, and the span holds no more ticks than the ceiling. It reports each as an
 * outcome in the reply. It never returns part of a span as if it were the whole.
 *
 * The reply states the coordinate URIs once, the first instant once, and then a gap in
 * microseconds, a coordinate index and a value for each tick. The client adds the gaps up to
 * get the times.
 */
class ORES_MARKETDATA_CORE_EXPORT series_tick_reader final {
public:
    /**
     * @brief The most ticks one reply carries.
     *
     * A tick costs about 15 bytes on the wire, so the ceiling keeps a reply at a little more
     * than a third of the message cap of the broker. The service never truncates a span, so a
     * span over the ceiling is refused and the refusal names it.
     */
    static constexpr std::size_t max_ticks = 25'000;

    /**
     * @brief Reads the ticks @p request names.
     *
     * @param ctx Database context, already scoped to the tenant and the party by the caller.
     * @param request The series, the span and the coordinates to read.
     * @param now The end of a span that states none.
     * @return The ticks, or an outcome that says why the span is refused.
     */
    static messaging::get_series_ticks_response
    read(ores::database::context ctx,
         const messaging::get_series_ticks_request& request,
         std::chrono::system_clock::time_point now = std::chrono::system_clock::now());
};

}

#endif
