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
#ifndef ORES_REFDATA_CORE_SERVICE_FIXING_SOURCE_KIND_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_FIXING_SOURCE_KIND_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/domain/fixing_source_kind.hpp"
#include "ores.refdata.api/messaging/fixing_source_kind_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/fixing_source_kind_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing fixing source kinds.
 *
 * Provides a higher-level interface for fixing source kind operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT fixing_source_kind_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.fixing_source_kind_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fixing_source_kind_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fixing_source_kind_service(const context& ctx);

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
    ::ores::refdata::messaging::list_fixing_source_kinds_response list_fixing_source_kinds(
        const ::ores::refdata::messaging::list_fixing_source_kinds_request& request);
    ::ores::refdata::messaging::get_fixing_source_kind_response get_fixing_source_kind(
        const ::ores::refdata::messaging::get_fixing_source_kind_request& request);
    ::ores::refdata::messaging::get_many_fixing_source_kinds_response get_many_fixing_source_kinds(
        const ::ores::refdata::messaging::get_many_fixing_source_kinds_request& request);
    ::ores::refdata::messaging::put_fixing_source_kind_response put_fixing_source_kind(
        const ::ores::refdata::messaging::put_fixing_source_kind_request& request);
    ::ores::refdata::messaging::put_many_fixing_source_kinds_response put_many_fixing_source_kinds(
        const ::ores::refdata::messaging::put_many_fixing_source_kinds_request& request);
    ::ores::refdata::messaging::delete_fixing_source_kind_response delete_fixing_source_kind(
        const ::ores::refdata::messaging::delete_fixing_source_kind_request& request);
    ::ores::refdata::messaging::delete_many_fixing_source_kinds_response
    delete_many_fixing_source_kinds(
        const ::ores::refdata::messaging::delete_many_fixing_source_kinds_request& request);
    ::ores::refdata::messaging::list_fixing_source_kind_versions_response
    list_fixing_source_kind_versions(
        const ::ores::refdata::messaging::list_fixing_source_kind_versions_request& request);
    ::ores::refdata::messaging::get_fixing_source_kind_version_response
    get_fixing_source_kind_version(
        const ::ores::refdata::messaging::get_fixing_source_kind_version_request& request);
    /**@}*/

    /**
     * @brief Lists fixing source kinds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of fixing source kinds for the requested page.
     */
    std::vector<domain::fixing_source_kind> list_kinds(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active fixing source kinds.
     *
     * @return Total number of active fixing source kinds.
     */
    std::uint32_t count_kinds();


    /**
     * @brief Retrieves a single fixing source kind as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The fixing source kind at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::fixing_source_kind> get_kind_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single fixing source kind by its primary key.
     *
     * @return The fixing source kind if found, std::nullopt otherwise.
     */
    std::optional<domain::fixing_source_kind> get_kind(const std::string& code);

    /**
     * @brief Retrieves a batch of fixing source kinds by primary key.
     */
    std::vector<domain::fixing_source_kind> get_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Saves a fixing source kind (creates or updates).
     *
     * @param kind The fixing source kind to save.
     * @throws std::exception on failure.
     */
    void save_kind(const domain::fixing_source_kind& kind);

    /**
     * @brief Saves a batch of fixing source kinds.
     *
     * @param kinds The fixing source kinds to save.
     * @throws std::exception on failure.
     */
    void save_kinds(const std::vector<domain::fixing_source_kind>& kinds);

    /**
     * @brief Deletes a fixing source kind by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_kind(const std::string& code);

    /**
     * @brief Deletes fixing source kinds by their primary keys.
     */
    void delete_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a fixing source kind.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fixing_source_kind> get_kind_history(const std::string& code);

private:
    context ctx_;
    repository::fixing_source_kind_repository repo_;

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
    prepare_change(const ::ores::refdata::messaging::fixing_source_kind_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::fixing_source_kind>& current,
                   domain::fixing_source_kind& out);
};

}

#endif
