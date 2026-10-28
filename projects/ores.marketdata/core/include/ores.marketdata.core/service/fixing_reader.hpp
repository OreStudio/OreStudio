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
#ifndef ORES_MARKETDATA_CORE_SERVICE_FIXING_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_FIXING_READER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"

namespace ores::marketdata::service {

/**
 * @brief Reads the durable fixings of an item: the latest on or before a date, and a window.
 *
 * The item is the fixing's oresmd URI. It names one series among the parties the session
 * sees, or the read is refused as ambiguous, and each answer names the provider and the
 * vendor id behind the item.
 */
class ORES_MARKETDATA_CORE_EXPORT fixing_reader final {
public:
    /**
     * @brief The most calendar days one window spans.
     *
     * Ten years of daily fixings is about 3,650 values, a reply of a few hundred kilobytes,
     * well under the message cap of the broker. A longer window is refused.
     */
    static constexpr int max_days = 3'660;

    static messaging::get_fixing_response latest(ores::database::context ctx,
                                                 const messaging::get_fixing_request& request);

    static messaging::get_fixing_window_response
    window(ores::database::context ctx, const messaging::get_fixing_window_request& request);
};

}

#endif
