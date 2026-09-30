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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_MARKET_BINDING_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_MARKET_BINDING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_market_binding.hpp"
#include "ores.reporting.api/messaging/report_market_binding_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_market_binding_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report market bindings.
 *
 * Provides a higher-level interface for report market binding operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_market_binding_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_market_binding_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_market_binding_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_market_binding_service(context ctx);

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
    messaging::list_report_market_bindings_response
    list_report_market_bindings(const messaging::list_report_market_bindings_request& request);
    messaging::get_report_market_binding_response
    get_report_market_binding(const messaging::get_report_market_binding_request& request);
    messaging::get_many_report_market_bindings_response get_many_report_market_bindings(
        const messaging::get_many_report_market_bindings_request& request);
    messaging::put_report_market_binding_response
    put_report_market_binding(const messaging::put_report_market_binding_request& request);
    messaging::put_many_report_market_bindings_response put_many_report_market_bindings(
        const messaging::put_many_report_market_bindings_request& request);
    messaging::delete_report_market_binding_response
    delete_report_market_binding(const messaging::delete_report_market_binding_request& request);
    messaging::delete_many_report_market_bindings_response delete_many_report_market_bindings(
        const messaging::delete_many_report_market_bindings_request& request);
    messaging::list_report_market_binding_versions_response list_report_market_binding_versions(
        const messaging::list_report_market_binding_versions_request& request);
    messaging::get_report_market_binding_version_response get_report_market_binding_version(
        const messaging::get_report_market_binding_version_request& request);
    /**@}*/

    /**
     * @brief Lists report market bindings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report market bindings for the requested page.
     */
    std::vector<domain::report_market_binding> list_bindings(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active report market bindings.
     *
     * @return Total number of active report market bindings.
     */
    std::uint32_t count_bindings();


    /**
     * @brief Retrieves a single report market binding as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report market binding at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_market_binding>
    get_binding_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single report market binding by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report market binding if found, std::nullopt otherwise.
     */
    std::optional<domain::report_market_binding> get_binding(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of report market bindings by primary key.
     */
    std::vector<domain::report_market_binding> get_bindings(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report market binding (creates or updates).
     *
     * @param binding The report market binding to save.
     * @throws std::exception on failure.
     */
    void save_binding(const domain::report_market_binding& binding);

    /**
     * @brief Saves a batch of report market bindings.
     *
     * @param bindings The report market bindings to save.
     * @throws std::exception on failure.
     */
    void save_bindings(const std::vector<domain::report_market_binding>& bindings);

    /**
     * @brief Deletes a report market binding by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_binding(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report market bindings by their primary keys.
     */
    void delete_bindings(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report market binding.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_market_binding> get_binding_history(const std::string& id);

private:
    context ctx_;
    repository::report_market_binding_repository repo_;

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
    prepare_change(const messaging::report_market_binding_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::report_market_binding& out);
};

}

#endif
