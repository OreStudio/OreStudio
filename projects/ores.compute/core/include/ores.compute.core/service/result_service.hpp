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
#ifndef ORES_COMPUTE_CORE_SERVICE_RESULT_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_RESULT_SERVICE_HPP

#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute results.
 *
 * Provides a higher-level interface for compute result operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT result_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.result_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a result_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit result_service(context ctx);

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
    messaging::list_results_response list_results(const messaging::list_results_request& request);
    messaging::get_result_response get_result(const messaging::get_result_request& request);
    messaging::get_many_results_response
    get_many_results(const messaging::get_many_results_request& request);
    messaging::put_result_response put_result(const messaging::put_result_request& request);
    messaging::put_many_results_response
    put_many_results(const messaging::put_many_results_request& request);
    messaging::delete_result_response
    delete_result(const messaging::delete_result_request& request);
    messaging::delete_many_results_response
    delete_many_results(const messaging::delete_many_results_request& request);
    messaging::list_by_workunit_id_results_response
    list_by_workunit_id_results(const messaging::list_by_workunit_id_results_request& request);
    messaging::list_result_versions_response
    list_result_versions(const messaging::list_result_versions_request& request);
    messaging::get_result_version_response
    get_result_version(const messaging::get_result_version_request& request);
    /**@}*/

    /**
     * @brief Lists compute results with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute results for the requested page.
     */
    std::vector<domain::result> list_results(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute results.
     *
     * @return Total number of active compute results.
     */
    std::uint32_t count_results();


    /**
     * @brief Lists compute results filtered by workunit_id, with pagination.
     *
     * @param workunit_id The workunit_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching compute results for the requested page.
     */
    std::vector<domain::result> list_results_by_workunit_id(const std::string& workunit_id,
                                                            std::uint32_t offset,
                                                            std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute results filtered by workunit_id.
     *
     * @param workunit_id The workunit_id to filter by.
     * @return Total number of matching compute results.
     */
    std::uint32_t count_results_by_workunit_id(const std::string& workunit_id);


    /**
     * @brief Retrieves a single compute result as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute result at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::result> get_result_at_version(const boost::uuids::uuid& id,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single compute result by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The compute result if found, std::nullopt otherwise.
     */
    std::optional<domain::result> get_result(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of compute results by primary key.
     */
    std::vector<domain::result> get_results(const std::vector<std::string>& ids);

    /**
     * @brief Saves a compute result (creates or updates).
     *
     * @param result The compute result to save.
     * @throws std::exception on failure.
     */
    void save_result(const domain::result& result);

    /**
     * @brief Saves a batch of compute results.
     *
     * @param results The compute results to save.
     * @throws std::exception on failure.
     */
    void save_results(const std::vector<domain::result>& results);

    /**
     * @brief Deletes a compute result by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_result(const boost::uuids::uuid& id);

    /**
     * @brief Deletes compute results by their primary keys.
     */
    void delete_results(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute result.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::result> get_result_history(const std::string& id);

    /**
     * @brief Lists results in a given server state, newest first.
     */
    std::vector<domain::result> list_by_state(int server_state);

private:
    context ctx_;
    repository::result_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::result_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::result& out);
};

}

#endif
