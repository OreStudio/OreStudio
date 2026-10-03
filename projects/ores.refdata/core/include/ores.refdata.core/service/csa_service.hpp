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
#ifndef ORES_REFDATA_CORE_SERVICE_CSA_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CSA_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/csa.hpp"
#include "ores.refdata.api/messaging/csa_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/csa_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing CSAs.
 *
 * Provides a higher-level interface for CSA operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT csa_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.csa_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a csa_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit csa_service(context ctx);

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
    messaging::list_csas_response list_csas(const messaging::list_csas_request& request);
    messaging::get_csa_response get_csa(const messaging::get_csa_request& request);
    messaging::get_many_csas_response
    get_many_csas(const messaging::get_many_csas_request& request);
    messaging::put_csa_response put_csa(const messaging::put_csa_request& request);
    messaging::put_many_csas_response
    put_many_csas(const messaging::put_many_csas_request& request);
    messaging::delete_csa_response delete_csa(const messaging::delete_csa_request& request);
    messaging::delete_many_csas_response
    delete_many_csas(const messaging::delete_many_csas_request& request);
    messaging::list_by_netting_set_id_csas_response
    list_by_netting_set_id_csas(const messaging::list_by_netting_set_id_csas_request& request);
    messaging::list_csa_versions_response
    list_csa_versions(const messaging::list_csa_versions_request& request);
    messaging::get_csa_version_response
    get_csa_version(const messaging::get_csa_version_request& request);
    /**@}*/

    /**
     * @brief Lists CSAs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of CSAs for the requested page.
     */
    std::vector<domain::csa> list_csas(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSAs.
     *
     * @return Total number of active CSAs.
     */
    std::uint32_t count_csas();


    /**
     * @brief Lists CSAs filtered by netting_set_id, with pagination.
     *
     * @param netting_set_id The netting_set_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching CSAs for the requested page.
     */
    std::vector<domain::csa> list_csas_by_netting_set_id(const std::string& netting_set_id,
                                                         std::uint32_t offset,
                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSAs filtered by netting_set_id.
     *
     * @param netting_set_id The netting_set_id to filter by.
     * @return Total number of matching CSAs.
     */
    std::uint32_t count_csas_by_netting_set_id(const std::string& netting_set_id);

    /**
     * @brief Lists CSAs filtered by netting_set_id, with pagination.
     *
     * @param netting_set_id The netting_set_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching CSAs for the requested page.
     */
    std::vector<domain::csa> list_csas_by_netting_set_id(const boost::uuids::uuid& netting_set_id,
                                                         std::uint32_t offset,
                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active CSAs filtered by netting_set_id.
     *
     * @param netting_set_id The netting_set_id to filter by.
     * @return Total number of matching CSAs.
     */
    std::uint32_t count_csas_by_netting_set_id(const boost::uuids::uuid& netting_set_id);


    /**
     * @brief Retrieves a single CSA as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The CSA at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::csa> get_csa_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single CSA by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The CSA if found, std::nullopt otherwise.
     */
    std::optional<domain::csa> get_csa(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single CSA by its uuid primary key.
     *
     * @return The CSA if found, std::nullopt otherwise.
     */
    std::optional<domain::csa> find_csa(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of CSAs by primary key.
     */
    std::vector<domain::csa> get_csas(const std::vector<std::string>& ids);

    /**
     * @brief Saves a CSA (creates or updates).
     *
     * @param csa The CSA to save.
     * @throws std::exception on failure.
     */
    void save_csa(const domain::csa& csa);

    /**
     * @brief Saves a batch of CSAs.
     *
     * @param csas The CSAs to save.
     * @throws std::exception on failure.
     */
    void save_csas(const std::vector<domain::csa>& csas);

    /**
     * @brief Deletes a CSA by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_csa(const boost::uuids::uuid& id);

    /**
     * @brief Removes a CSA by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_csa(const boost::uuids::uuid& id);

    /**
     * @brief Deletes CSAs by their primary keys.
     */
    void delete_csas(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a CSA.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::csa> get_csa_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a CSA
     * by its uuid primary key.
     */
    std::vector<domain::csa> get_csa_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::csa_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::csa_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::csa& out);
};

}

#endif
