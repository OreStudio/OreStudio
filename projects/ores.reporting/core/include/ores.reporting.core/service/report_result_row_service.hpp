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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_RESULT_ROW_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_RESULT_ROW_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_result_row.hpp"
#include "ores.reporting.api/messaging/report_result_row_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_result_row_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report result rows.
 *
 * Provides a higher-level interface for report result row operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_result_row_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_result_row_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_result_row_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_result_row_service(context ctx);

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
    ::ores::reporting::messaging::list_report_result_rows_response list_report_result_rows(
        const ::ores::reporting::messaging::list_report_result_rows_request& request);
    ::ores::reporting::messaging::get_report_result_row_response get_report_result_row(
        const ::ores::reporting::messaging::get_report_result_row_request& request);
    ::ores::reporting::messaging::get_many_report_result_rows_response get_many_report_result_rows(
        const ::ores::reporting::messaging::get_many_report_result_rows_request& request);
    ::ores::reporting::messaging::put_report_result_row_response put_report_result_row(
        const ::ores::reporting::messaging::put_report_result_row_request& request);
    ::ores::reporting::messaging::put_many_report_result_rows_response put_many_report_result_rows(
        const ::ores::reporting::messaging::put_many_report_result_rows_request& request);
    ::ores::reporting::messaging::delete_report_result_row_response delete_report_result_row(
        const ::ores::reporting::messaging::delete_report_result_row_request& request);
    ::ores::reporting::messaging::delete_many_report_result_rows_response
    delete_many_report_result_rows(
        const ::ores::reporting::messaging::delete_many_report_result_rows_request& request);
    /**@}*/

    /**
     * @brief Lists report result rows with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report result rows for the requested page.
     */
    std::vector<domain::report_result_row> list_rows(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report result rows.
     *
     * @return Total number of active report result rows.
     */
    std::uint32_t count_rows();


    /**
     * @brief Retrieves a single report result row by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report result row if found, std::nullopt otherwise.
     */
    std::optional<domain::report_result_row> get_row(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of report result rows by primary key.
     */
    std::vector<domain::report_result_row> get_rows(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report result row (creates or updates).
     *
     * @param row The report result row to save.
     * @throws std::exception on failure.
     */
    void save_row(const domain::report_result_row& row);

    /**
     * @brief Saves a batch of report result rows.
     *
     * @param rows The report result rows to save.
     * @throws std::exception on failure.
     */
    void save_rows(const std::vector<domain::report_result_row>& rows);

    /**
     * @brief Deletes a report result row by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_row(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report result rows by their primary keys.
     */
    void delete_rows(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report result row.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_result_row> get_row_history(const std::string& id);

private:
    context ctx_;
    repository::report_result_row_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::reporting::messaging::report_result_row_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::report_result_row& out);
};

}

#endif
