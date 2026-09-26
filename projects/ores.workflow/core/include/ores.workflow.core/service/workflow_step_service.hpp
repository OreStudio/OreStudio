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
#ifndef ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_STEP_SERVICE_HPP
#define ORES_WORKFLOW_CORE_SERVICE_WORKFLOW_STEP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.workflow.api/domain/workflow_step.hpp"
#include "ores.workflow.api/messaging/workflow_step_protocol.hpp"
#include "ores.workflow.core/export.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::workflow::service {

/**
 * @brief Service for managing workflow steps.
 *
 * Provides a higher-level interface for workflow step operations,
 * wrapping the underlying repository.
 */
class ORES_WORKFLOW_CORE_EXPORT workflow_step_service {
private:
    inline static std::string_view logger_name = "ores.workflow.service.workflow_step_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a workflow_step_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit workflow_step_service(context ctx);

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
    messaging::list_workflow_steps_response
    list_workflow_steps(const messaging::list_workflow_steps_request& request);
    messaging::get_workflow_step_response
    get_workflow_step(const messaging::get_workflow_step_request& request);
    messaging::get_many_workflow_steps_response
    get_many_workflow_steps(const messaging::get_many_workflow_steps_request& request);
    messaging::put_workflow_step_response
    put_workflow_step(const messaging::put_workflow_step_request& request);
    messaging::put_many_workflow_steps_response
    put_many_workflow_steps(const messaging::put_many_workflow_steps_request& request);
    messaging::delete_workflow_step_response
    delete_workflow_step(const messaging::delete_workflow_step_request& request);
    messaging::delete_many_workflow_steps_response
    delete_many_workflow_steps(const messaging::delete_many_workflow_steps_request& request);
    messaging::list_by_workflow_id_workflow_steps_response list_by_workflow_id_workflow_steps(
        const messaging::list_by_workflow_id_workflow_steps_request& request);
    messaging::list_workflow_step_versions_response
    list_workflow_step_versions(const messaging::list_workflow_step_versions_request& request);
    messaging::get_workflow_step_version_response
    get_workflow_step_version(const messaging::get_workflow_step_version_request& request);
    /**@}*/

    /**
     * @brief Lists workflow steps with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of workflow steps for the requested page.
     */
    std::vector<domain::workflow_step> list_steps(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active workflow steps.
     *
     * @return Total number of active workflow steps.
     */
    std::uint32_t count_steps();


    /**
     * @brief Lists workflow steps filtered by workflow_id, with pagination.
     *
     * @param workflow_id The workflow_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching workflow steps for the requested page.
     */
    std::vector<domain::workflow_step> list_steps_by_workflow_id(const std::string& workflow_id,
                                                                 std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active workflow steps filtered by workflow_id.
     *
     * @param workflow_id The workflow_id to filter by.
     * @return Total number of matching workflow steps.
     */
    std::uint32_t count_steps_by_workflow_id(const std::string& workflow_id);


    /**
     * @brief Retrieves a single workflow step as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The workflow step at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::workflow_step> get_step_at_version(const boost::uuids::uuid& id,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single workflow step by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The workflow step if found, std::nullopt otherwise.
     */
    std::optional<domain::workflow_step> get_step(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of workflow steps by primary key.
     */
    std::vector<domain::workflow_step> get_steps(const std::vector<std::string>& ids);

    /**
     * @brief Saves a workflow step (creates or updates).
     *
     * @param step The workflow step to save.
     * @throws std::exception on failure.
     */
    void save_step(const domain::workflow_step& step);

    /**
     * @brief Saves a batch of workflow steps.
     *
     * @param steps The workflow steps to save.
     * @throws std::exception on failure.
     */
    void save_steps(const std::vector<domain::workflow_step>& steps);

    /**
     * @brief Deletes a workflow step by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_step(const boost::uuids::uuid& id);

    /**
     * @brief Deletes workflow steps by their primary keys.
     */
    void delete_steps(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a workflow step.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::workflow_step> get_step_history(const std::string& id);

private:
    context ctx_;
    repository::workflow_step_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::workflow_step_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::workflow_step& out);
};

}

#endif
