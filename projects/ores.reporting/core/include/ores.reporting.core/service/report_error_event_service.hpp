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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_ERROR_EVENT_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_ERROR_EVENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_error_event.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report error events.
 *
 * Provides a higher-level interface for report error event operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_error_event_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_error_event_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_error_event_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_error_event_service(context ctx);

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
    ::ores::reporting::messaging::list_report_error_events_response list_report_error_events(
        const ::ores::reporting::messaging::list_report_error_events_request& request);
    ::ores::reporting::messaging::get_report_error_event_response get_report_error_event(
        const ::ores::reporting::messaging::get_report_error_event_request& request);
    ::ores::reporting::messaging::get_many_report_error_events_response
    get_many_report_error_events(
        const ::ores::reporting::messaging::get_many_report_error_events_request& request);
    ::ores::reporting::messaging::list_by_report_instance_id_report_error_events_response
    list_by_report_instance_id_report_error_events(
        const ::ores::reporting::messaging::list_by_report_instance_id_report_error_events_request&
            request);
    /**@}*/

    /**
     * @brief Lists report error events with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report error events for the requested page.
     */
    std::vector<domain::report_error_event> list_errors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report error events.
     *
     * @return Total number of active report error events.
     */
    std::uint32_t count_errors();


    /**
     * @brief Lists report error events filtered by report_instance_id, with pagination.
     *
     * @param report_instance_id The report_instance_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching report error events for the requested page.
     */
    std::vector<domain::report_error_event> list_errors_by_report_instance_id(
        const std::string& report_instance_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report error events filtered by report_instance_id.
     *
     * @param report_instance_id The report_instance_id to filter by.
     * @return Total number of matching report error events.
     */
    std::uint32_t count_errors_by_report_instance_id(const std::string& report_instance_id);


    /**
     * @brief Retrieves a single report error event by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report error event if found, std::nullopt otherwise.
     */
    std::optional<domain::report_error_event> get_error(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of report error events by primary key.
     */
    std::vector<domain::report_error_event> get_errors(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report error event (creates or updates).
     *
     * @param error The report error event to save.
     * @throws std::exception on failure.
     */
    void save_error(const domain::report_error_event& error);

    /**
     * @brief Saves a batch of report error events.
     *
     * @param errors The report error events to save.
     * @throws std::exception on failure.
     */
    void save_errors(const std::vector<domain::report_error_event>& errors);

    /**
     * @brief Deletes a report error event by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_error(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report error events by their primary keys.
     */
    void delete_errors(const std::vector<std::string>& ids);


    /**
     * @brief The problems of each named run, counted apart by severity.
     *
     * A run that recorded no problem has no entry, which is what the screen
     * draws as no problems rather than as a row of zeroes.
     */
    messaging::get_report_error_counts_response
    get_report_error_counts(const messaging::get_report_error_counts_request& request);

private:
    context ctx_;
    repository::report_error_event_repository repo_;
};

}

#endif
