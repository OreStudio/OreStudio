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
#ifndef ORES_REFDATA_CORE_SERVICE_BOND_YIELD_CONVENTION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BOND_YIELD_CONVENTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/bond_yield_convention.hpp"
#include "ores.refdata.api/messaging/bond_yield_convention_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/bond_yield_convention_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing bond yield conventions.
 *
 * Provides a higher-level interface for bond yield convention operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT bond_yield_convention_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.bond_yield_convention_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_yield_convention_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_yield_convention_service(context ctx);

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
    messaging::list_bond_yield_conventions_response
    list_bond_yield_conventions(const messaging::list_bond_yield_conventions_request& request);
    messaging::get_bond_yield_convention_response
    get_bond_yield_convention(const messaging::get_bond_yield_convention_request& request);
    messaging::get_many_bond_yield_conventions_response get_many_bond_yield_conventions(
        const messaging::get_many_bond_yield_conventions_request& request);
    messaging::put_bond_yield_convention_response
    put_bond_yield_convention(const messaging::put_bond_yield_convention_request& request);
    messaging::put_many_bond_yield_conventions_response put_many_bond_yield_conventions(
        const messaging::put_many_bond_yield_conventions_request& request);
    messaging::delete_bond_yield_convention_response
    delete_bond_yield_convention(const messaging::delete_bond_yield_convention_request& request);
    messaging::delete_many_bond_yield_conventions_response delete_many_bond_yield_conventions(
        const messaging::delete_many_bond_yield_conventions_request& request);
    messaging::list_bond_yield_convention_versions_response list_bond_yield_convention_versions(
        const messaging::list_bond_yield_convention_versions_request& request);
    messaging::get_bond_yield_convention_version_response get_bond_yield_convention_version(
        const messaging::get_bond_yield_convention_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond yield conventions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond yield conventions for the requested page.
     */
    std::vector<domain::bond_yield_convention> list_bond_yield_conventions(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond yield conventions.
     *
     * @return Total number of active bond yield conventions.
     */
    std::uint32_t count_bond_yield_conventions();


    /**
     * @brief Retrieves a single bond yield convention as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond yield convention at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_yield_convention>
    get_bond_yield_convention_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single bond yield convention by its primary key.
     *
     * @return The bond yield convention if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_yield_convention> get_bond_yield_convention(const std::string& id);

    /**
     * @brief Retrieves a batch of bond yield conventions by primary key.
     */
    std::vector<domain::bond_yield_convention>
    get_bond_yield_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a bond yield convention (creates or updates).
     *
     * @param bond_yield_convention The bond yield convention to save.
     * @throws std::exception on failure.
     */
    void save_bond_yield_convention(const domain::bond_yield_convention& bond_yield_convention);

    /**
     * @brief Saves a batch of bond yield conventions.
     *
     * @param bond_yield_conventions The bond yield conventions to save.
     * @throws std::exception on failure.
     */
    void save_bond_yield_conventions(
        const std::vector<domain::bond_yield_convention>& bond_yield_conventions);

    /**
     * @brief Deletes a bond yield convention by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_yield_convention(const std::string& id);

    /**
     * @brief Deletes bond yield conventions by their primary keys.
     */
    void delete_bond_yield_conventions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a bond yield convention.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_yield_convention>
    get_bond_yield_convention_history(const std::string& id);

private:
    context ctx_;
    repository::bond_yield_convention_repository repo_;

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
    prepare_change(const messaging::bond_yield_convention_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::bond_yield_convention& out);
};

}

#endif
