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
#ifndef ORES_COMPUTE_CORE_SERVICE_HOST_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_HOST_SERVICE_HPP

#include "ores.compute.api/domain/host.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/host_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing compute hosts.
 *
 * Provides a higher-level interface for compute host operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT host_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.host_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a host_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit host_service(context ctx);

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
    messaging::list_hosts_response list_hosts(const messaging::list_hosts_request& request);
    messaging::get_host_response get_host(const messaging::get_host_request& request);
    messaging::get_many_hosts_response
    get_many_hosts(const messaging::get_many_hosts_request& request);
    messaging::put_host_response put_host(const messaging::put_host_request& request);
    messaging::put_many_hosts_response
    put_many_hosts(const messaging::put_many_hosts_request& request);
    messaging::delete_host_response delete_host(const messaging::delete_host_request& request);
    messaging::delete_many_hosts_response
    delete_many_hosts(const messaging::delete_many_hosts_request& request);
    messaging::list_host_versions_response
    list_host_versions(const messaging::list_host_versions_request& request);
    messaging::get_host_version_response
    get_host_version(const messaging::get_host_version_request& request);
    /**@}*/

    /**
     * @brief Lists compute hosts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of compute hosts for the requested page.
     */
    std::vector<domain::host> list_hosts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active compute hosts.
     *
     * @return Total number of active compute hosts.
     */
    std::uint32_t count_hosts();


    /**
     * @brief Retrieves a single compute host as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The compute host at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::host> get_host_at_version(const boost::uuids::uuid& id,
                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single compute host by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The compute host if found, std::nullopt otherwise.
     */
    std::optional<domain::host> get_host(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single compute host by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The compute host if found, std::nullopt otherwise.
     */
    std::optional<domain::host> get_host_by_external_id(const std::string& external_id);

    /**
     * @brief Retrieves a batch of compute hosts by primary key.
     */
    std::vector<domain::host> get_hosts(const std::vector<std::string>& ids);

    /**
     * @brief Saves a compute host (creates or updates).
     *
     * @param host The compute host to save.
     * @throws std::exception on failure.
     */
    void save_host(const domain::host& host);

    /**
     * @brief Saves a batch of compute hosts.
     *
     * @param hosts The compute hosts to save.
     * @throws std::exception on failure.
     */
    void save_hosts(const std::vector<domain::host>& hosts);

    /**
     * @brief Deletes a compute host by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_host(const boost::uuids::uuid& id);

    /**
     * @brief Deletes compute hosts by their primary keys.
     */
    void delete_hosts(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a compute host.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::host> get_host_history(const std::string& key);

private:
    context ctx_;
    repository::host_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::host_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::host& out);
};

}

#endif
