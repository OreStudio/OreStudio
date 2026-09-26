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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_INSTANCE_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_INSTANCE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_instance.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing report instances.
 *
 * Provides a higher-level interface for report instance operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_instance_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_instance_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_instance_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_instance_service(context ctx);

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
    messaging::list_report_instances_response
    list_report_instances(const messaging::list_report_instances_request& request);
    messaging::get_report_instance_response
    get_report_instance(const messaging::get_report_instance_request& request);
    messaging::get_many_report_instances_response
    get_many_report_instances(const messaging::get_many_report_instances_request& request);
    messaging::put_report_instance_response
    put_report_instance(const messaging::put_report_instance_request& request);
    messaging::put_many_report_instances_response
    put_many_report_instances(const messaging::put_many_report_instances_request& request);
    messaging::delete_report_instance_response
    delete_report_instance(const messaging::delete_report_instance_request& request);
    messaging::delete_many_report_instances_response
    delete_many_report_instances(const messaging::delete_many_report_instances_request& request);
    messaging::list_report_instance_versions_response
    list_report_instance_versions(const messaging::list_report_instance_versions_request& request);
    messaging::get_report_instance_version_response
    get_report_instance_version(const messaging::get_report_instance_version_request& request);
    /**@}*/

    /**
     * @brief Lists report instances with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of report instances for the requested page.
     */
    std::vector<domain::report_instance> list_instances(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active report instances.
     *
     * @return Total number of active report instances.
     */
    std::uint32_t count_instances();


    /**
     * @brief Retrieves a single report instance as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The report instance at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_instance> get_instance_at_version(const boost::uuids::uuid& id,
                                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single report instance by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The report instance if found, std::nullopt otherwise.
     */
    std::optional<domain::report_instance> get_instance(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single report instance by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The report instance if found, std::nullopt otherwise.
     */
    std::optional<domain::report_instance> get_instance_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of report instances by primary key.
     */
    std::vector<domain::report_instance> get_instances(const std::vector<std::string>& ids);

    /**
     * @brief Saves a report instance (creates or updates).
     *
     * @param instance The report instance to save.
     * @throws std::exception on failure.
     */
    void save_instance(const domain::report_instance& instance);

    /**
     * @brief Saves a batch of report instances.
     *
     * @param instances The report instances to save.
     * @throws std::exception on failure.
     */
    void save_instances(const std::vector<domain::report_instance>& instances);

    /**
     * @brief Deletes a report instance by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instance(const boost::uuids::uuid& id);

    /**
     * @brief Deletes report instances by their primary keys.
     */
    void delete_instances(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a report instance.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::report_instance> get_instance_history(const std::string& key);

private:
    context ctx_;
    repository::report_instance_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::report_instance_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::report_instance& out);
};

}

#endif
