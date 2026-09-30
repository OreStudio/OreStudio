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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_RUN_SETUP_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_RUN_SETUP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_run_setup.hpp"
#include "ores.reporting.api/messaging/report_run_setup_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report run setups.
 *
 * Provides a higher-level interface for report run setup operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_run_setup_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_run_setup_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_run_setup_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_run_setup_service(context ctx);

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
    messaging::list_report_run_setups_response
    list_report_run_setups(const messaging::list_report_run_setups_request& request);
    messaging::get_report_run_setup_response
    get_report_run_setup(const messaging::get_report_run_setup_request& request);
    messaging::get_many_report_run_setups_response
    get_many_report_run_setups(const messaging::get_many_report_run_setups_request& request);
    messaging::put_report_run_setup_response
    put_report_run_setup(const messaging::put_report_run_setup_request& request);
    messaging::put_many_report_run_setups_response
    put_many_report_run_setups(const messaging::put_many_report_run_setups_request& request);
    messaging::delete_report_run_setup_response
    delete_report_run_setup(const messaging::delete_report_run_setup_request& request);
    messaging::delete_many_report_run_setups_response
    delete_many_report_run_setups(const messaging::delete_many_report_run_setups_request& request);
    messaging::list_report_run_setup_versions_response list_report_run_setup_versions(
        const messaging::list_report_run_setup_versions_request& request);
    messaging::get_report_run_setup_version_response
    get_report_run_setup_version(const messaging::get_report_run_setup_version_request& request);
    /**@}*/

    /**
     * @brief Lists report run setups with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report run setups for the requested page.
     */
    std::vector<domain::report_run_setup> list_setups(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report run setups.
     *
     * @return Total number of active report run setups.
     */
    std::uint32_t count_setups();


    /**
     * @brief Retrieves a single report run setup as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report run setup at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_run_setup> get_setup_at_version(const boost::uuids::uuid& id,
                                                                 std::uint32_t version);

    /**
     * @brief Retrieves a single report run setup by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report run setup if found, std::nullopt otherwise.
     */
    std::optional<domain::report_run_setup> get_setup(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of report run setups by primary key.
     */
    std::vector<domain::report_run_setup> get_setups(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report run setup (creates or updates).
     *
     * @param setup The report run setup to save.
     * @throws std::exception on failure.
     */
    void save_setup(const domain::report_run_setup& setup);

    /**
     * @brief Saves a batch of report run setups.
     *
     * @param setups The report run setups to save.
     * @throws std::exception on failure.
     */
    void save_setups(const std::vector<domain::report_run_setup>& setups);

    /**
     * @brief Deletes a report run setup by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_setup(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report run setups by their primary keys.
     */
    void delete_setups(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report run setup.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_run_setup> get_setup_history(const std::string& id);

private:
    context ctx_;
    repository::report_run_setup_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::report_run_setup_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::report_run_setup& out);
};

}

#endif
