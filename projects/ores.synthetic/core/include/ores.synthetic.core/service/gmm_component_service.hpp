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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_GMM_COMPONENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing GMM components.
 *
 * Provides a higher-level interface for GMM component operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT gmm_component_service {
private:
    inline static std::string_view logger_name = "ores.synthetic.service.gmm_component_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a gmm_component_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit gmm_component_service(context ctx);

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
    messaging::list_gmm_components_response
    list_gmm_components(const messaging::list_gmm_components_request& request);
    messaging::get_gmm_component_response
    get_gmm_component(const messaging::get_gmm_component_request& request);
    messaging::get_many_gmm_components_response
    get_many_gmm_components(const messaging::get_many_gmm_components_request& request);
    messaging::put_gmm_component_response
    put_gmm_component(const messaging::put_gmm_component_request& request);
    messaging::put_many_gmm_components_response
    put_many_gmm_components(const messaging::put_many_gmm_components_request& request);
    messaging::delete_gmm_component_response
    delete_gmm_component(const messaging::delete_gmm_component_request& request);
    messaging::delete_many_gmm_components_response
    delete_many_gmm_components(const messaging::delete_many_gmm_components_request& request);
    messaging::list_gmm_component_versions_response
    list_gmm_component_versions(const messaging::list_gmm_component_versions_request& request);
    messaging::get_gmm_component_version_response
    get_gmm_component_version(const messaging::get_gmm_component_version_request& request);
    /**@}*/

    /**
     * @brief Lists GMM components with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of GMM components for the requested page.
     */
    std::vector<domain::gmm_component> list_gmm_components(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active GMM components.
     *
     * @return Total number of active GMM components.
     */
    std::uint32_t count_gmm_components();


    /**
     * @brief Retrieves a single GMM component as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The GMM component at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::gmm_component> get_gmm_component_at_version(const boost::uuids::uuid& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single GMM component by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The GMM component if found, std::nullopt otherwise.
     */
    std::optional<domain::gmm_component> get_gmm_component(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of GMM components by primary key.
     */
    std::vector<domain::gmm_component> get_gmm_components(const std::vector<std::string>& ids);

    /**
     * @brief Saves a GMM component (creates or updates).
     *
     * @param gmm_component The GMM component to save.
     * @throws std::exception on failure.
     */
    void save_gmm_component(const domain::gmm_component& gmm_component);

    /**
     * @brief Saves a batch of GMM components.
     *
     * @param gmm_components The GMM components to save.
     * @throws std::exception on failure.
     */
    void save_gmm_components(const std::vector<domain::gmm_component>& gmm_components);

    /**
     * @brief Deletes a GMM component by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_gmm_component(const boost::uuids::uuid& id);

    /**
     * @brief Deletes GMM components by their primary keys.
     */
    void delete_gmm_components(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a GMM component.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::gmm_component> get_gmm_component_history(const std::string& id);

private:
    context ctx_;
    repository::gmm_component_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::gmm_component_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::gmm_component& out);
};

}

#endif
