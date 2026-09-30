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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_ANALYTIC_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_ANALYTIC_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_analytic.hpp"
#include "ores.reporting.api/messaging/report_analytic_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_analytic_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report analytics.
 *
 * Provides a higher-level interface for report analytic operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_analytic_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_analytic_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_analytic_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_analytic_service(context ctx);

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
    messaging::list_report_analytics_response
    list_report_analytics(const messaging::list_report_analytics_request& request);
    messaging::get_report_analytic_response
    get_report_analytic(const messaging::get_report_analytic_request& request);
    messaging::get_many_report_analytics_response
    get_many_report_analytics(const messaging::get_many_report_analytics_request& request);
    messaging::put_report_analytic_response
    put_report_analytic(const messaging::put_report_analytic_request& request);
    messaging::put_many_report_analytics_response
    put_many_report_analytics(const messaging::put_many_report_analytics_request& request);
    messaging::delete_report_analytic_response
    delete_report_analytic(const messaging::delete_report_analytic_request& request);
    messaging::delete_many_report_analytics_response
    delete_many_report_analytics(const messaging::delete_many_report_analytics_request& request);
    messaging::list_by_analytic_type_code_report_analytics_response
    list_by_analytic_type_code_report_analytics(
        const messaging::list_by_analytic_type_code_report_analytics_request& request);
    messaging::list_report_analytic_versions_response
    list_report_analytic_versions(const messaging::list_report_analytic_versions_request& request);
    messaging::get_report_analytic_version_response
    get_report_analytic_version(const messaging::get_report_analytic_version_request& request);
    /**@}*/

    /**
     * @brief Lists report analytics with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report analytics for the requested page.
     */
    std::vector<domain::report_analytic> list_analytics(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report analytics.
     *
     * @return Total number of active report analytics.
     */
    std::uint32_t count_analytics();


    /**
     * @brief Lists report analytics filtered by analytic_type_code, with pagination.
     *
     * @param analytic_type_code The analytic_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching report analytics for the requested page.
     */
    std::vector<domain::report_analytic> list_analytics_by_analytic_type_code(
        const std::string& analytic_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report analytics filtered by analytic_type_code.
     *
     * @param analytic_type_code The analytic_type_code to filter by.
     * @return Total number of matching report analytics.
     */
    std::uint32_t count_analytics_by_analytic_type_code(const std::string& analytic_type_code);


    /**
     * @brief Retrieves a single report analytic as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report analytic at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_analytic> get_analytic_at_version(const boost::uuids::uuid& id,
                                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single report analytic by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report analytic if found, std::nullopt otherwise.
     */
    std::optional<domain::report_analytic> get_analytic(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of report analytics by primary key.
     */
    std::vector<domain::report_analytic> get_analytics(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report analytic (creates or updates).
     *
     * @param analytic The report analytic to save.
     * @throws std::exception on failure.
     */
    void save_analytic(const domain::report_analytic& analytic);

    /**
     * @brief Saves a batch of report analytics.
     *
     * @param analytics The report analytics to save.
     * @throws std::exception on failure.
     */
    void save_analytics(const std::vector<domain::report_analytic>& analytics);

    /**
     * @brief Deletes a report analytic by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_analytic(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report analytics by their primary keys.
     */
    void delete_analytics(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report analytic.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_analytic> get_analytic_history(const std::string& id);

private:
    context ctx_;
    repository::report_analytic_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::report_analytic_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::report_analytic& out);
};

}

#endif
