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
#ifndef ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_LINEAGE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_OBSERVATION_LINEAGE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.api/messaging/observation_lineage_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing observation lineages.
 *
 * Provides a higher-level interface for observation lineage operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT observation_lineage_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.observation_lineage_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a observation_lineage_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit observation_lineage_service(context ctx);

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
    messaging::list_observation_lineages_response
    list_observation_lineages(const messaging::list_observation_lineages_request& request);
    messaging::get_observation_lineage_response
    get_observation_lineage(const messaging::get_observation_lineage_request& request);
    messaging::get_many_observation_lineages_response
    get_many_observation_lineages(const messaging::get_many_observation_lineages_request& request);
    messaging::put_observation_lineage_response
    put_observation_lineage(const messaging::put_observation_lineage_request& request);
    messaging::put_many_observation_lineages_response
    put_many_observation_lineages(const messaging::put_many_observation_lineages_request& request);
    messaging::delete_observation_lineage_response
    delete_observation_lineage(const messaging::delete_observation_lineage_request& request);
    messaging::delete_many_observation_lineages_response delete_many_observation_lineages(
        const messaging::delete_many_observation_lineages_request& request);
    messaging::list_observation_lineage_versions_response list_observation_lineage_versions(
        const messaging::list_observation_lineage_versions_request& request);
    messaging::get_observation_lineage_version_response get_observation_lineage_version(
        const messaging::get_observation_lineage_version_request& request);
    /**@}*/

    /**
     * @brief Lists observation lineages with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of observation lineages for the requested page.
     */
    std::vector<domain::observation_lineage> list_observation_lineages(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active observation lineages.
     *
     * @return Total number of active observation lineages.
     */
    std::uint32_t count_observation_lineages();


    /**
     * @brief Retrieves a single observation lineage as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The observation lineage at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::observation_lineage>
    get_observation_lineage_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single observation lineage by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The observation lineage if found, std::nullopt otherwise.
     */
    std::optional<domain::observation_lineage>
    get_observation_lineage(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of observation lineages by primary key.
     */
    std::vector<domain::observation_lineage>
    get_observation_lineages(const std::vector<std::string>& ids);

    /**
     * @brief Saves a observation lineage (creates or updates).
     *
     * @param observation_lineage The observation lineage to save.
     * @throws std::exception on failure.
     */
    void save_observation_lineage(const domain::observation_lineage& observation_lineage);

    /**
     * @brief Saves a batch of observation lineages.
     *
     * @param observation_lineages The observation lineages to save.
     * @throws std::exception on failure.
     */
    void
    save_observation_lineages(const std::vector<domain::observation_lineage>& observation_lineages);

    /**
     * @brief Deletes a observation lineage by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_observation_lineage(const boost::uuids::uuid& id);

    /**
     * @brief Deletes observation lineages by their primary keys.
     */
    void delete_observation_lineages(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a observation lineage.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::observation_lineage> get_observation_lineage_history(const std::string& id);

private:
    context ctx_;
    repository::observation_lineage_repository repo_;

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
    prepare_change(const messaging::observation_lineage_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::observation_lineage& out);
};

}

#endif
