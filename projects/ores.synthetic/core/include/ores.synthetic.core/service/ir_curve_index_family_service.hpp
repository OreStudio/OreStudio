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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_IR_CURVE_INDEX_FAMILY_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_IR_CURVE_INDEX_FAMILY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.synthetic.api/domain/ir_curve_index_family.hpp"
#include "ores.synthetic.api/messaging/ir_curve_index_family_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/ir_curve_index_family_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing IR curve index families.
 *
 * Provides a higher-level interface for IR curve index family operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT ir_curve_index_family_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.ir_curve_index_family_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ir_curve_index_family_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ir_curve_index_family_service(const context& ctx);

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
    ::ores::synthetic::messaging::list_ir_curve_index_families_response
    list_ir_curve_index_families(
        const ::ores::synthetic::messaging::list_ir_curve_index_families_request& request);
    ::ores::synthetic::messaging::get_ir_curve_index_family_response get_ir_curve_index_family(
        const ::ores::synthetic::messaging::get_ir_curve_index_family_request& request);
    ::ores::synthetic::messaging::get_many_ir_curve_index_families_response
    get_many_ir_curve_index_families(
        const ::ores::synthetic::messaging::get_many_ir_curve_index_families_request& request);
    ::ores::synthetic::messaging::put_ir_curve_index_family_response put_ir_curve_index_family(
        const ::ores::synthetic::messaging::put_ir_curve_index_family_request& request);
    ::ores::synthetic::messaging::put_many_ir_curve_index_families_response
    put_many_ir_curve_index_families(
        const ::ores::synthetic::messaging::put_many_ir_curve_index_families_request& request);
    ::ores::synthetic::messaging::delete_ir_curve_index_family_response
    delete_ir_curve_index_family(
        const ::ores::synthetic::messaging::delete_ir_curve_index_family_request& request);
    ::ores::synthetic::messaging::delete_many_ir_curve_index_families_response
    delete_many_ir_curve_index_families(
        const ::ores::synthetic::messaging::delete_many_ir_curve_index_families_request& request);
    ::ores::synthetic::messaging::list_ir_curve_index_family_versions_response
    list_ir_curve_index_family_versions(
        const ::ores::synthetic::messaging::list_ir_curve_index_family_versions_request& request);
    ::ores::synthetic::messaging::get_ir_curve_index_family_version_response
    get_ir_curve_index_family_version(
        const ::ores::synthetic::messaging::get_ir_curve_index_family_version_request& request);
    /**@}*/

    /**
     * @brief Lists IR curve index families with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of IR curve index families for the requested page.
     */
    std::vector<domain::ir_curve_index_family> list_index_families(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active IR curve index families.
     *
     * @return Total number of active IR curve index families.
     */
    std::uint32_t count_index_families();


    /**
     * @brief Retrieves a single IR curve index family as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The IR curve index family at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_index_family>
    get_index_family_at_version(const std::string& code, std::uint32_t version);

    /**
     * @brief Retrieves a single IR curve index family by its primary key.
     *
     * @return The IR curve index family if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_index_family> get_index_family(const std::string& code);

    /**
     * @brief Retrieves a batch of IR curve index families by primary key.
     */
    std::vector<domain::ir_curve_index_family>
    get_index_families(const std::vector<std::string>& codes);

    /**
     * @brief Saves a IR curve index family (creates or updates).
     *
     * @param index_family The IR curve index family to save.
     * @throws std::exception on failure.
     */
    void save_index_family(const domain::ir_curve_index_family& index_family);

    /**
     * @brief Saves a batch of IR curve index families.
     *
     * @param index_families The IR curve index families to save.
     * @throws std::exception on failure.
     */
    void save_index_families(const std::vector<domain::ir_curve_index_family>& index_families);

    /**
     * @brief Deletes a IR curve index family by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_index_family(const std::string& code);

    /**
     * @brief Deletes IR curve index families by their primary keys.
     */
    void delete_index_families(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a IR curve index family.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ir_curve_index_family> get_index_family_history(const std::string& code);

private:
    context ctx_;
    repository::ir_curve_index_family_repository repo_;

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
    prepare_change(const ::ores::synthetic::messaging::ir_curve_index_family_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::ir_curve_index_family>& current,
                   domain::ir_curve_index_family& out);
};

}

#endif
