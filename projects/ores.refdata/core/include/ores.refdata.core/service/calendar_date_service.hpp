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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_SERVICE_CALENDAR_DATE_SERVICE_HPP
#define ORES_REFDATA_SERVICE_CALENDAR_DATE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.api/messaging/calendar_date_protocol.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendar dates.
 *
 * Provides a higher-level interface for calendar date operations,
 * wrapping the underlying repository.
 */
class calendar_date_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_date_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_date_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_date_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_calendar_dates_response
    list_calendar_dates(const messaging::list_calendar_dates_request& request);
    messaging::get_calendar_date_response
    get_calendar_date(const messaging::get_calendar_date_request& request);
    messaging::get_many_calendar_dates_response
    get_many_calendar_dates(const messaging::get_many_calendar_dates_request& request);
    messaging::list_by_calendar_code_calendar_dates_response list_by_calendar_code_calendar_dates(
        const messaging::list_by_calendar_code_calendar_dates_request& request);
    /**@}*/

private:
    context ctx_;
    repository::calendar_date_repository repo_;
};

}

#endif
