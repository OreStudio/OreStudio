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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_MATERIALISATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_MATERIALISATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.core/export.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Instantiates a calendar template's rules/exceptions into
 * materialised calendar_dates rows, on demand.
 *
 * Two-stage design (see the parent task's plan): calendar (+
 * calendar_rule / calendar_exception for base-less templates) is the
 * *rule* -- what to compute. calendar_dates is the *instantiation* --
 * the computed result every reader (UI, tenor/schedule date-math)
 * actually queries. This service is the only writer of calendar_dates;
 * regeneration is on-demand (a "regenerate up to <year>" command), not
 * a cron job, and only ever extends the materialised horizon forward --
 * it never recomputes or discards dates already stored below the
 * existing watermark.
 *
 * Base-less calendars (calendar.base_calendar_code is null --
 * QuantLib-transcribed or a genuinely new user template) are
 * instantiated directly via ores.analytics.quant's batch rule engine
 * from their own calendar_rule / calendar_exception rows. A calendar
 * with a base (a user template layered on another template via
 * base_calendar_code) is instantiated by copying its base's already-
 * materialised calendar_dates and overlaying its own calendar_exception
 * rows on top -- the base must already be materialised at least as far
 * as the requested horizon, resolved recursively for chains of
 * user templates based on other user templates.
 */
class ORES_REFDATA_CORE_EXPORT calendar_materialisation_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.calendar_materialisation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit calendar_materialisation_service(context ctx);

    /**
     * @brief Regenerates (extends) a single calendar's materialised
     * dates up to @p end_year, resolving its base chain if it has one.
     *
     * @param calendar_code The calendar to regenerate.
     * @param end_year Last year (inclusive) to materialise up to. When
     * absent, falls back to the
     * calendar.materialisation.end_horizon system setting (or a
     * built-in default if that setting is not configured).
     * @return Number of calendar_date rows written.
     * @throws std::runtime_error if calendar_code does not exist, or a
     * base_calendar_code cycle is detected.
     */
    std::size_t regenerate(const std::string& calendar_code,
                           std::optional<std::chrono::year> end_year = std::nullopt);

    /**
     * @brief Regenerates (extends) every calendar's materialised
     * dates up to @p end_year.
     *
     * @return Number of calendar_date rows written, summed across all
     * calendars.
     */
    std::size_t regenerate_all(std::optional<std::chrono::year> end_year = std::nullopt);

private:
    context ctx_;
};

}

#endif
