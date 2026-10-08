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
#ifndef ORES_TRADING_CORE_SERVICE_STRUCTURE_KIND_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_STRUCTURE_KIND_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/structure_kind.hpp"
#include "ores.trading.api/messaging/structure_kind_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/structure_kind_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing structure kinds.
 *
 * Provides a higher-level interface for structure kind operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT structure_kind_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.structure_kind_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a structure_kind_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit structure_kind_service(context ctx);

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
    messaging::list_structure_kinds_response
    list_structure_kinds(const messaging::list_structure_kinds_request& request);
    messaging::get_structure_kind_response
    get_structure_kind(const messaging::get_structure_kind_request& request);
    messaging::get_many_structure_kinds_response
    get_many_structure_kinds(const messaging::get_many_structure_kinds_request& request);
    messaging::put_structure_kind_response
    put_structure_kind(const messaging::put_structure_kind_request& request);
    messaging::put_many_structure_kinds_response
    put_many_structure_kinds(const messaging::put_many_structure_kinds_request& request);
    messaging::delete_structure_kind_response
    delete_structure_kind(const messaging::delete_structure_kind_request& request);
    messaging::delete_many_structure_kinds_response
    delete_many_structure_kinds(const messaging::delete_many_structure_kinds_request& request);
    messaging::list_structure_kind_versions_response
    list_structure_kind_versions(const messaging::list_structure_kind_versions_request& request);
    messaging::get_structure_kind_version_response
    get_structure_kind_version(const messaging::get_structure_kind_version_request& request);
    /**@}*/

    /**
     * @brief Lists structure kinds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of structure kinds for the requested page.
     */
    std::vector<domain::structure_kind> list_structure_kinds(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active structure kinds.
     *
     * @return Total number of active structure kinds.
     */
    std::uint32_t count_structure_kinds();


    /**
     * @brief Retrieves a single structure kind as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The structure kind at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::structure_kind> get_structure_kind_at_version(const std::string& code,
                                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single structure kind by its primary key.
     *
     * @return The structure kind if found, std::nullopt otherwise.
     */
    std::optional<domain::structure_kind> get_structure_kind(const std::string& code);

    /**
     * @brief Retrieves a batch of structure kinds by primary key.
     */
    std::vector<domain::structure_kind> get_structure_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Saves a structure kind (creates or updates).
     *
     * @param structure_kind The structure kind to save.
     * @throws std::exception on failure.
     */
    void save_structure_kind(const domain::structure_kind& structure_kind);

    /**
     * @brief Saves a batch of structure kinds.
     *
     * @param structure_kinds The structure kinds to save.
     * @throws std::exception on failure.
     */
    void save_structure_kinds(const std::vector<domain::structure_kind>& structure_kinds);

    /**
     * @brief Deletes a structure kind by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_structure_kind(const std::string& code);

    /**
     * @brief Deletes structure kinds by their primary keys.
     */
    void delete_structure_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a structure kind.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::structure_kind> get_structure_kind_history(const std::string& code);

private:
    context ctx_;
    repository::structure_kind_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::structure_kind_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::structure_kind& out);
};

}

#endif
