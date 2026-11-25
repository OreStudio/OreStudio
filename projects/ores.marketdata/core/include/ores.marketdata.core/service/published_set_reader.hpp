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
#ifndef ORES_MARKETDATA_CORE_SERVICE_PUBLISHED_SET_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_PUBLISHED_SET_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::service {

/**
 * @brief Reads the newest published set of a set of series, for a view that joins them.
 *
 * A caller that draws a table of curves needs, for each curve, when its set was built
 * and whether that set is the one in use. The read answers those facts for the whole
 * set at once, so the caller joins in memory rather than asking once per curve.
 */
class ORES_MARKETDATA_CORE_EXPORT published_set_reader final {
public:
    /**
     * @brief The most series one read takes.
     *
     * The set is the batch, and the answer is three columns of one row each: a thousand
     * series is a few tens of kilobytes, well under the message cap of the broker. A
     * caller with more sends them in batches of its own.
     */
    static constexpr std::size_t max_batch = 1'000;

    /**
     * @brief The newest set of each series in the request that holds one.
     */
    static ::ores::marketdata::messaging::get_latest_published_sets_response
    latest(const ores::database::context& ctx,
           const ::ores::marketdata::messaging::get_latest_published_sets_request& request);
};

}

#endif
