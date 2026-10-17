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
#ifndef ORES_ORE_CORE_DOMAIN_CALENDAR_ADJUSTMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_CALENDAR_ADJUSTMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include <vector>

namespace ores::ore::domain {

/**
 * @brief Projects one calendar adjustment record into the refdata message.
 *
 * The XML is read by the codec, not by this class: a calendar adjustment
 * arrives as the record the generated table describes, and the work left is
 * the part no table can carry -- which ORE element becomes which field of our
 * own message, and what a date in the document means.
 */
class ORES_ORE_CORE_EXPORT calendar_adjustment_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.calendar_adjustment_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using record = xml::generated::calendar_adjustments::newcalendar;
    using document = xml::generated::calendar_adjustments::calendaradjustment;

    static refdata::messaging::calendar_adjustment map(const record& v);
    static std::vector<refdata::messaging::calendar_adjustment> map(const document& v);

    static record reverse(const refdata::messaging::calendar_adjustment& v);
    static document reverse(const std::vector<refdata::messaging::calendar_adjustment>& v);
};

}

#endif
