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
#ifndef ORES_REFDATA_CORE_SERVICE_CRM_ENABLED_DERIVED_PAIR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CRM_ENABLED_DERIVED_PAIR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/crm_enabled_derived_pair.hpp"
#include "ores.refdata.api/messaging/crm_enabled_derived_pair_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/crm_enabled_derived_pair_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing CRM enabled derived pairs.
 *
 * Provides a higher-level interface for CRM enabled derived pair operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT crm_enabled_derived_pair_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.crm_enabled_derived_pair_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a crm_enabled_derived_pair_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit crm_enabled_derived_pair_service(context ctx);

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
    messaging::list_crm_enabled_derived_pairs_response list_crm_enabled_derived_pairs(
        const messaging::list_crm_enabled_derived_pairs_request& request);
    messaging::get_crm_enabled_derived_pair_response
    get_crm_enabled_derived_pair(const messaging::get_crm_enabled_derived_pair_request& request);
    messaging::get_many_crm_enabled_derived_pairs_response get_many_crm_enabled_derived_pairs(
        const messaging::get_many_crm_enabled_derived_pairs_request& request);
    messaging::put_crm_enabled_derived_pair_response
    put_crm_enabled_derived_pair(const messaging::put_crm_enabled_derived_pair_request& request);
    messaging::put_many_crm_enabled_derived_pairs_response put_many_crm_enabled_derived_pairs(
        const messaging::put_many_crm_enabled_derived_pairs_request& request);
    messaging::delete_crm_enabled_derived_pair_response delete_crm_enabled_derived_pair(
        const messaging::delete_crm_enabled_derived_pair_request& request);
    messaging::delete_many_crm_enabled_derived_pairs_response delete_many_crm_enabled_derived_pairs(
        const messaging::delete_many_crm_enabled_derived_pairs_request& request);
    messaging::list_crm_enabled_derived_pair_versions_response
    list_crm_enabled_derived_pair_versions(
        const messaging::list_crm_enabled_derived_pair_versions_request& request);
    messaging::get_crm_enabled_derived_pair_version_response get_crm_enabled_derived_pair_version(
        const messaging::get_crm_enabled_derived_pair_version_request& request);
    /**@}*/

    /**
     * @brief Lists CRM enabled derived pairs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CRM enabled derived pairs for the requested page.
     */
    std::vector<domain::crm_enabled_derived_pair>
    list_crm_enabled_derived_pairs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active CRM enabled derived pairs.
     *
     * @return Total number of active CRM enabled derived pairs.
     */
    std::uint32_t count_crm_enabled_derived_pairs();


    /**
     * @brief Retrieves a single CRM enabled derived pair as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The CRM enabled derived pair at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_enabled_derived_pair>
    get_crm_enabled_derived_pair_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single CRM enabled derived pair by its primary key.
     *
     * @return The CRM enabled derived pair if found, std::nullopt otherwise.
     */
    std::optional<domain::crm_enabled_derived_pair>
    get_crm_enabled_derived_pair(const std::string& id);

    /**
     * @brief Retrieves a batch of CRM enabled derived pairs by primary key.
     */
    std::vector<domain::crm_enabled_derived_pair>
    get_crm_enabled_derived_pairs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a CRM enabled derived pair (creates or updates).
     *
     * @param crm_enabled_derived_pair The CRM enabled derived pair to save.
     * @throws std::exception on failure.
     */
    void
    save_crm_enabled_derived_pair(const domain::crm_enabled_derived_pair& crm_enabled_derived_pair);

    /**
     * @brief Saves a batch of CRM enabled derived pairs.
     *
     * @param crm_enabled_derived_pairs The CRM enabled derived pairs to save.
     * @throws std::exception on failure.
     */
    void save_crm_enabled_derived_pairs(
        const std::vector<domain::crm_enabled_derived_pair>& crm_enabled_derived_pairs);

    /**
     * @brief Deletes a CRM enabled derived pair by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_crm_enabled_derived_pair(const std::string& id);

    /**
     * @brief Deletes CRM enabled derived pairs by their primary keys.
     */
    void delete_crm_enabled_derived_pairs(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CRM enabled derived pair.
     */
    std::vector<domain::crm_enabled_derived_pair>
    get_crm_enabled_derived_pair_history(const std::string& id);

private:
    context ctx_;
    repository::crm_enabled_derived_pair_repository repo_;

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
    prepare_change(const messaging::crm_enabled_derived_pair_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::crm_enabled_derived_pair& out);
};

}

#endif
