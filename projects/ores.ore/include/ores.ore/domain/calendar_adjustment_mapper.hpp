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
#ifndef ORES_ORE_DOMAIN_CALENDAR_ADJUSTMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_CALENDAR_ADJUSTMENT_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_adjustment.hpp"
#include "ores.ore/domain/domain.hpp"

namespace ores::ore::domain {

/**
 * @brief Maps between ORE XML calendar adjustment types and refdata domain types.
 */
class calendar_adjustment_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.calendar_adjustment_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Maps a single ORE @c newcalendar element to a domain type.
     */
    static refdata::domain::calendar_adjustment map(const newcalendar& v);

    /**
     * @brief Maps all calendars in an ORE @c calendaradjustment document.
     */
    static std::vector<refdata::domain::calendar_adjustment>
    map(const calendaradjustment& v);
};

}

#endif
