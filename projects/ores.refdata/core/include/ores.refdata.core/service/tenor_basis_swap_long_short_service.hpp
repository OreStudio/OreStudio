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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_BASIS_SWAP_LONG_SHORT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_BASIS_SWAP_LONG_SHORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_long_short.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_long_short_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_long_short_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor basis swap long shorts.
 *
 * Provides a higher-level interface for tenor basis swap long short operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_basis_swap_long_short_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.tenor_basis_swap_long_short_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_basis_swap_long_short_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_basis_swap_long_short_service(context ctx);

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
    messaging::list_tenor_basis_swap_long_shorts_response list_tenor_basis_swap_long_shorts(
        const messaging::list_tenor_basis_swap_long_shorts_request& request);
    messaging::get_tenor_basis_swap_long_short_response get_tenor_basis_swap_long_short(
        const messaging::get_tenor_basis_swap_long_short_request& request);
    messaging::get_many_tenor_basis_swap_long_shorts_response get_many_tenor_basis_swap_long_shorts(
        const messaging::get_many_tenor_basis_swap_long_shorts_request& request);
    messaging::put_tenor_basis_swap_long_short_response put_tenor_basis_swap_long_short(
        const messaging::put_tenor_basis_swap_long_short_request& request);
    messaging::put_many_tenor_basis_swap_long_shorts_response put_many_tenor_basis_swap_long_shorts(
        const messaging::put_many_tenor_basis_swap_long_shorts_request& request);
    messaging::delete_tenor_basis_swap_long_short_response delete_tenor_basis_swap_long_short(
        const messaging::delete_tenor_basis_swap_long_short_request& request);
    messaging::delete_many_tenor_basis_swap_long_shorts_response
    delete_many_tenor_basis_swap_long_shorts(
        const messaging::delete_many_tenor_basis_swap_long_shorts_request& request);
    messaging::list_by_convention_id_tenor_basis_swap_long_shorts_response
    list_by_convention_id_tenor_basis_swap_long_shorts(
        const messaging::list_by_convention_id_tenor_basis_swap_long_shorts_request& request);
    messaging::list_tenor_basis_swap_long_short_versions_response
    list_tenor_basis_swap_long_short_versions(
        const messaging::list_tenor_basis_swap_long_short_versions_request& request);
    messaging::get_tenor_basis_swap_long_short_version_response
    get_tenor_basis_swap_long_short_version(
        const messaging::get_tenor_basis_swap_long_short_version_request& request);
    /**@}*/

    /**
     * @brief Lists tenor basis swap long shorts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor basis swap long shorts for the requested page.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    list_tenor_basis_swap_long_shorts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor basis swap long shorts.
     *
     * @return Total number of active tenor basis swap long shorts.
     */
    std::uint32_t count_tenor_basis_swap_long_shorts();


    /**
     * @brief Lists tenor basis swap long shorts filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching tenor basis swap long shorts for the requested page.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    list_tenor_basis_swap_long_shorts_by_convention_id(const std::string& convention_id,
                                                       std::uint32_t offset,
                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor basis swap long shorts filtered by convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching tenor basis swap long shorts.
     */
    std::uint32_t
    count_tenor_basis_swap_long_shorts_by_convention_id(const std::string& convention_id);

    /**
     * @brief Lists tenor basis swap long shorts filtered by convention_id, with pagination.
     *
     * @param convention_id The convention_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching tenor basis swap long shorts for the requested page.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    list_tenor_basis_swap_long_shorts_by_convention_id(const boost::uuids::uuid& convention_id,
                                                       std::uint32_t offset,
                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor basis swap long shorts filtered by convention_id.
     *
     * @param convention_id The convention_id to filter by.
     * @return Total number of matching tenor basis swap long shorts.
     */
    std::uint32_t
    count_tenor_basis_swap_long_shorts_by_convention_id(const boost::uuids::uuid& convention_id);


    /**
     * @brief Lists tenor basis swap long shorts filtered by convention_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param convention_id The convention_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching tenor basis swap long shorts.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    list_tenor_basis_swap_long_shorts_by_convention_id_as_of(
        const std::string& convention_id,
        std::chrono::system_clock::time_point valid_from_bound,
        std::chrono::system_clock::time_point valid_to_bound);

    /**
     * @brief Retrieves a single tenor basis swap long short as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenor basis swap long short at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_basis_swap_long_short>
    get_tenor_basis_swap_long_short_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single tenor basis swap long short by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The tenor basis swap long short if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_basis_swap_long_short>
    get_tenor_basis_swap_long_short(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of tenor basis swap long shorts by primary key.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    get_tenor_basis_swap_long_shorts(const std::vector<std::string>& ids);

    /**
     * @brief Saves a tenor basis swap long short (creates or updates).
     *
     * @param tenor_basis_swap_long_short The tenor basis swap long short to save.
     * @throws std::exception on failure.
     */
    void save_tenor_basis_swap_long_short(
        const domain::tenor_basis_swap_long_short& tenor_basis_swap_long_short);

    /**
     * @brief Saves a batch of tenor basis swap long shorts.
     *
     * @param tenor_basis_swap_long_shorts The tenor basis swap long shorts to save.
     * @throws std::exception on failure.
     */
    void save_tenor_basis_swap_long_shorts(
        const std::vector<domain::tenor_basis_swap_long_short>& tenor_basis_swap_long_shorts);

    /**
     * @brief Deletes a tenor basis swap long short by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_tenor_basis_swap_long_short(const boost::uuids::uuid& id);

    /**
     * @brief Deletes tenor basis swap long shorts by their primary keys.
     */
    void delete_tenor_basis_swap_long_shorts(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a tenor basis swap long short.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::tenor_basis_swap_long_short>
    get_tenor_basis_swap_long_short_history(const std::string& id);

private:
    context ctx_;
    repository::tenor_basis_swap_long_short_repository repo_;

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
    prepare_change(const messaging::tenor_basis_swap_long_short_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::tenor_basis_swap_long_short& out);
};

}

#endif
