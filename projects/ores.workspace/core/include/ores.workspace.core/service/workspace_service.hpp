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
#ifndef ORES_WORKSPACE_CORE_SERVICE_WORKSPACE_SERVICE_HPP
#define ORES_WORKSPACE_CORE_SERVICE_WORKSPACE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include "ores.workspace.core/export.hpp"
#include "ores.workspace.core/repository/workspace_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::workspace::service {

/**
 * @brief Service for managing workspaces.
 *
 * Provides a higher-level interface for workspace operations,
 * wrapping the underlying repository.
 */
class ORES_WORKSPACE_CORE_EXPORT workspace_service {
private:
    inline static std::string_view logger_name = "ores.workspace.service.workspace_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a workspace_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit workspace_service(context ctx);

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
    messaging::list_workspaces_response
    list_workspaces(const messaging::list_workspaces_request& request);
    messaging::get_workspace_response
    get_workspace(const messaging::get_workspace_request& request);
    messaging::get_many_workspaces_response
    get_many_workspaces(const messaging::get_many_workspaces_request& request);
    messaging::put_workspace_response
    put_workspace(const messaging::put_workspace_request& request);
    messaging::put_many_workspaces_response
    put_many_workspaces(const messaging::put_many_workspaces_request& request);
    messaging::delete_workspace_response
    delete_workspace(const messaging::delete_workspace_request& request);
    messaging::delete_many_workspaces_response
    delete_many_workspaces(const messaging::delete_many_workspaces_request& request);
    messaging::list_workspace_versions_response
    list_workspace_versions(const messaging::list_workspace_versions_request& request);
    messaging::get_workspace_version_response
    get_workspace_version(const messaging::get_workspace_version_request& request);
    /**@}*/

    /**
     * @brief Lists workspaces with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of workspaces for the requested page.
     */
    std::vector<domain::workspace> list_workspaces(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active workspaces.
     *
     * @return Total number of active workspaces.
     */
    std::uint32_t count_workspaces();


    /**
     * @brief Retrieves a single workspace as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The workspace at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::workspace> get_workspace_at_version(const boost::uuids::uuid& id,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single workspace by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The workspace if found, std::nullopt otherwise.
     */
    std::optional<domain::workspace> get_workspace(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of workspaces by primary key.
     */
    std::vector<domain::workspace> get_workspaces(const std::vector<std::string>& ids);

    /**
     * @brief Saves a workspace (creates or updates).
     *
     * @param workspace The workspace to save.
     * @throws std::exception on failure.
     */
    void save_workspace(const domain::workspace& workspace);

    /**
     * @brief Saves a batch of workspaces.
     *
     * @param workspaces The workspaces to save.
     * @throws std::exception on failure.
     */
    void save_workspaces(const std::vector<domain::workspace>& workspaces);

    /**
     * @brief Deletes a workspace by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_workspace(const boost::uuids::uuid& id);

    /**
     * @brief Deletes workspaces by their primary keys.
     */
    void delete_workspaces(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a workspace.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::workspace> get_workspace_history(const std::string& id);

    /**
     * @brief Gets the workspace hierarchy (as a forest of trees) rooted
     * at, or containing, the given workspace.
     *
     * @param root_id The workspace to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::workspace_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::workspace_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::workspace& out);
};

}

#endif
