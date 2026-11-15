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
#ifndef ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_READER_HPP
#define ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/observation_read_operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <cstddef>

namespace ores::marketdata::service {

/**
 * @brief Reads the durable observations of a series over a window of days.
 *
 * The item is the series' oresmd URI, resolved among the parties the session sees as a
 * fixing item is, so one read returns every point of the series on every day of the
 * window, each with its coordinate.
 */
class ORES_MARKETDATA_CORE_EXPORT observation_reader final {
public:
    /// The most calendar days one window spans, as for fixings.
    static constexpr int max_days = 3'660;

    /**
     * @brief The most observations one reply carries.
     *
     * An observation is about 150 bytes on the wire, most of it its coordinate, and the
     * broker takes a megabyte, so a reply stays well under it. A window holding more is
     * refused, and the reader asks for fewer days.
     */
    static constexpr std::size_t max_observations = 5'000;

    static messaging::get_observation_window_response
    window(const ores::database::context& ctx,
           const messaging::get_observation_window_request& request);
};

}

#endif
