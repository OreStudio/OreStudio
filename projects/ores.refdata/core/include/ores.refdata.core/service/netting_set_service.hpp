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
#ifndef ORES_REFDATA_CORE_SERVICE_NETTING_SET_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_NETTING_SET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/netting_set.hpp"
#include "ores.refdata.api/messaging/netting_set_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/netting_set_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing netting sets.
 *
 * Provides a higher-level interface for netting set operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT netting_set_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.netting_set_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a netting_set_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit netting_set_service(context ctx);

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
    messaging::list_netting_sets_response
    list_netting_sets(const messaging::list_netting_sets_request& request);
    messaging::get_netting_set_response
    get_netting_set(const messaging::get_netting_set_request& request);
    messaging::get_many_netting_sets_response
    get_many_netting_sets(const messaging::get_many_netting_sets_request& request);
    messaging::put_netting_set_response
    put_netting_set(const messaging::put_netting_set_request& request);
    messaging::put_many_netting_sets_response
    put_many_netting_sets(const messaging::put_many_netting_sets_request& request);
    messaging::delete_netting_set_response
    delete_netting_set(const messaging::delete_netting_set_request& request);
    messaging::delete_many_netting_sets_response
    delete_many_netting_sets(const messaging::delete_many_netting_sets_request& request);
    messaging::list_by_netting_agreement_id_netting_sets_response
    list_by_netting_agreement_id_netting_sets(
        const messaging::list_by_netting_agreement_id_netting_sets_request& request);
    messaging::list_netting_set_versions_response
    list_netting_set_versions(const messaging::list_netting_set_versions_request& request);
    messaging::get_netting_set_version_response
    get_netting_set_version(const messaging::get_netting_set_version_request& request);
    /**@}*/

    /**
     * @brief Lists netting sets with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of netting sets for the requested page.
     */
    std::vector<domain::netting_set> list_netting_sets(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting sets.
     *
     * @return Total number of active netting sets.
     */
    std::uint32_t count_netting_sets();


    /**
     * @brief Lists netting sets filtered by netting_agreement_id, with pagination.
     *
     * @param netting_agreement_id The netting_agreement_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching netting sets for the requested page.
     */
    std::vector<domain::netting_set> list_netting_sets_by_netting_agreement_id(
        const std::string& netting_agreement_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting sets filtered by netting_agreement_id.
     *
     * @param netting_agreement_id The netting_agreement_id to filter by.
     * @return Total number of matching netting sets.
     */
    std::uint32_t
    count_netting_sets_by_netting_agreement_id(const std::string& netting_agreement_id);

    /**
     * @brief Lists netting sets filtered by netting_agreement_id, with pagination.
     *
     * @param netting_agreement_id The netting_agreement_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching netting sets for the requested page.
     */
    std::vector<domain::netting_set> list_netting_sets_by_netting_agreement_id(
        const boost::uuids::uuid& netting_agreement_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting sets filtered by netting_agreement_id.
     *
     * @param netting_agreement_id The netting_agreement_id to filter by.
     * @return Total number of matching netting sets.
     */
    std::uint32_t
    count_netting_sets_by_netting_agreement_id(const boost::uuids::uuid& netting_agreement_id);


    /**
     * @brief Retrieves a single netting set as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The netting set at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set> get_netting_set_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single netting set by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The netting set if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set> get_netting_set(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single netting set by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The netting set if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set> get_netting_set_by_code(const std::string& code);

    /**
     * @brief Retrieves a single netting set by its uuid primary key.
     *
     * @return The netting set if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set> find_netting_set(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of netting sets by primary key.
     */
    std::vector<domain::netting_set> get_netting_sets(const std::vector<std::string>& ids);

    /**
     * @brief Saves a netting set (creates or updates).
     *
     * @param netting_set The netting set to save.
     * @throws std::exception on failure.
     */
    void save_netting_set(const domain::netting_set& netting_set);

    /**
     * @brief Saves a batch of netting sets.
     *
     * @param netting_sets The netting sets to save.
     * @throws std::exception on failure.
     */
    void save_netting_sets(const std::vector<domain::netting_set>& netting_sets);

    /**
     * @brief Deletes a netting set by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_netting_set(const boost::uuids::uuid& id);

    /**
     * @brief Removes a netting set by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_netting_set(const boost::uuids::uuid& id);

    /**
     * @brief Deletes netting sets by their primary keys.
     */
    void delete_netting_sets(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a netting set.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::netting_set> get_netting_set_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a netting set
     * by its uuid primary key.
     */
    std::vector<domain::netting_set> get_netting_set_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::netting_set_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::netting_set_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::netting_set& out);
};

}

#endif
