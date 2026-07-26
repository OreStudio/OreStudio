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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_MATERIALISATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_MATERIALISATION_PROTOCOL_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::messaging {

/**
 * @brief On-demand "regenerate up to <year>" command for calendar_dates.
 *
 * Extends (never rewrites below the existing watermark) a single
 * calendar's materialised calendar_dates, or every calendar's, up to
 * end_year. No cron job triggers this -- it is user- or save-action-
 * triggered only (see the parent task's plan).
 */
struct regenerate_calendar_dates_request {
    using response_type = struct regenerate_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.regenerate";

    /// Calendar to regenerate; absent means every calendar.
    std::optional<std::string> calendar_code;
    /// Last year (inclusive) to materialise up to; absent falls back to
    /// the calendar.materialisation.end_horizon system setting.
    std::optional<int> end_year;
};

struct regenerate_calendar_dates_response {
    bool success = false;
    std::string message;
    std::uint64_t rows_written = 0;
};

}

#endif
