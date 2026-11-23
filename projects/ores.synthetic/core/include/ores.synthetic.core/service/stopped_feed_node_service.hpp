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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_STOPPED_FEED_NODE_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_STOPPED_FEED_NODE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.synthetic.api/domain/stopped_feed_node.hpp"
#include "ores.synthetic.api/messaging/stopped_feed_node_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/stopped_feed_node_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing stopped feed nodes.
 *
 * Provides a higher-level interface for stopped feed node operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT stopped_feed_node_service {
private:
    inline static std::string_view logger_name = "ores.synthetic.service.stopped_feed_node_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a stopped_feed_node_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit stopped_feed_node_service(const context& ctx);

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
    ::ores::synthetic::messaging::list_stopped_feed_nodes_response list_stopped_feed_nodes(
        const ::ores::synthetic::messaging::list_stopped_feed_nodes_request& request);
    ::ores::synthetic::messaging::get_stopped_feed_node_response get_stopped_feed_node(
        const ::ores::synthetic::messaging::get_stopped_feed_node_request& request);
    ::ores::synthetic::messaging::get_many_stopped_feed_nodes_response get_many_stopped_feed_nodes(
        const ::ores::synthetic::messaging::get_many_stopped_feed_nodes_request& request);
    ::ores::synthetic::messaging::put_stopped_feed_node_response put_stopped_feed_node(
        const ::ores::synthetic::messaging::put_stopped_feed_node_request& request);
    ::ores::synthetic::messaging::put_many_stopped_feed_nodes_response put_many_stopped_feed_nodes(
        const ::ores::synthetic::messaging::put_many_stopped_feed_nodes_request& request);
    ::ores::synthetic::messaging::delete_stopped_feed_node_response delete_stopped_feed_node(
        const ::ores::synthetic::messaging::delete_stopped_feed_node_request& request);
    ::ores::synthetic::messaging::delete_many_stopped_feed_nodes_response
    delete_many_stopped_feed_nodes(
        const ::ores::synthetic::messaging::delete_many_stopped_feed_nodes_request& request);
    ::ores::synthetic::messaging::list_stopped_feed_node_versions_response
    list_stopped_feed_node_versions(
        const ::ores::synthetic::messaging::list_stopped_feed_node_versions_request& request);
    ::ores::synthetic::messaging::get_stopped_feed_node_version_response
    get_stopped_feed_node_version(
        const ::ores::synthetic::messaging::get_stopped_feed_node_version_request& request);
    /**@}*/

    /**
     * @brief Lists stopped feed nodes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of stopped feed nodes for the requested page.
     */
    std::vector<domain::stopped_feed_node> list_stopped_nodes(std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active stopped feed nodes.
     *
     * @return Total number of active stopped feed nodes.
     */
    std::uint32_t count_stopped_nodes();


    /**
     * @brief Retrieves a single stopped feed node as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The stopped feed node at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::stopped_feed_node>
    get_stopped_node_at_version(const std::string& node_name, std::uint32_t version);

    /**
     * @brief Retrieves a single stopped feed node by its primary key.
     *
     * @return The stopped feed node if found, std::nullopt otherwise.
     */
    std::optional<domain::stopped_feed_node> get_stopped_node(const std::string& node_name);

    /**
     * @brief Retrieves a batch of stopped feed nodes by primary key.
     */
    std::vector<domain::stopped_feed_node>
    get_stopped_nodes(const std::vector<std::string>& node_names);

    /**
     * @brief Saves a stopped feed node (creates or updates).
     *
     * @param stopped_node The stopped feed node to save.
     * @throws std::exception on failure.
     */
    void save_stopped_node(const domain::stopped_feed_node& stopped_node);

    /**
     * @brief Saves a batch of stopped feed nodes.
     *
     * @param stopped_nodes The stopped feed nodes to save.
     * @throws std::exception on failure.
     */
    void save_stopped_nodes(const std::vector<domain::stopped_feed_node>& stopped_nodes);

    /**
     * @brief Deletes a stopped feed node by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_stopped_node(const std::string& node_name);

    /**
     * @brief Deletes stopped feed nodes by their primary keys.
     */
    void delete_stopped_nodes(const std::vector<std::string>& node_names);

    /**
     * @brief Retrieves all historical versions of a stopped feed node.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::stopped_feed_node> get_stopped_node_history(const std::string& node_name);

private:
    context ctx_;
    repository::stopped_feed_node_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::synthetic::messaging::stopped_feed_node_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::stopped_feed_node>& current,
                   domain::stopped_feed_node& out);
};

}

#endif
