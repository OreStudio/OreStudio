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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar.hpp"
#include "ores.refdata.api/messaging/calendar_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing calendars.
 *
 * Provides a higher-level interface for calendar operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT calendar_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a calendar_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit calendar_service(context ctx);

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
    messaging::list_calendars_response
    list_calendars(const messaging::list_calendars_request& request);
    messaging::get_calendar_response get_calendar(const messaging::get_calendar_request& request);
    messaging::get_many_calendars_response
    get_many_calendars(const messaging::get_many_calendars_request& request);
    messaging::put_calendar_response put_calendar(const messaging::put_calendar_request& request);
    messaging::put_many_calendars_response
    put_many_calendars(const messaging::put_many_calendars_request& request);
    messaging::delete_calendar_response
    delete_calendar(const messaging::delete_calendar_request& request);
    messaging::delete_many_calendars_response
    delete_many_calendars(const messaging::delete_many_calendars_request& request);
    messaging::list_calendar_versions_response
    list_calendar_versions(const messaging::list_calendar_versions_request& request);
    messaging::get_calendar_version_response
    get_calendar_version(const messaging::get_calendar_version_request& request);
    /**@}*/

    /**
     * @brief Lists calendars with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of calendars for the requested page.
     */
    std::vector<domain::calendar> list_calendars(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active calendars.
     *
     * @return Total number of active calendars.
     */
    std::uint32_t count_calendars();


    /**
     * @brief Retrieves a single calendar as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The calendar at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar> get_calendar_at_version(const std::string& code,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single calendar by its primary key.
     *
     * @return The calendar if found, std::nullopt otherwise.
     */
    std::optional<domain::calendar> get_calendar(const std::string& code);

    /**
     * @brief Retrieves a batch of calendars by primary key.
     */
    std::vector<domain::calendar> get_calendars(const std::vector<std::string>& codes);

    /**
     * @brief Saves a calendar (creates or updates).
     *
     * @param calendar The calendar to save.
     * @throws std::exception on failure.
     */
    void save_calendar(const domain::calendar& calendar);

    /**
     * @brief Saves a batch of calendars.
     *
     * @param calendars The calendars to save.
     * @throws std::exception on failure.
     */
    void save_calendars(const std::vector<domain::calendar>& calendars);

    /**
     * @brief Deletes a calendar by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_calendar(const std::string& code);

    /**
     * @brief Deletes calendars by their primary keys.
     */
    void delete_calendars(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a calendar.
     */
    std::vector<domain::calendar> get_calendar_history(const std::string& code);

private:
    context ctx_;
    repository::calendar_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::calendar_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::calendar& out);
};

}

#endif
