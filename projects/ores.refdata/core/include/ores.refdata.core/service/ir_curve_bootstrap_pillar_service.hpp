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
#ifndef ORES_REFDATA_CORE_SERVICE_IR_CURVE_BOOTSTRAP_PILLAR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_IR_CURVE_BOOTSTRAP_PILLAR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_pillar_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing IR curve bootstrap pillars.
 *
 * Provides a higher-level interface for IR curve bootstrap pillar operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT ir_curve_bootstrap_pillar_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.ir_curve_bootstrap_pillar_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ir_curve_bootstrap_pillar_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ir_curve_bootstrap_pillar_service(context ctx);

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
    messaging::list_ir_curve_bootstrap_pillars_response list_ir_curve_bootstrap_pillars(
        const messaging::list_ir_curve_bootstrap_pillars_request& request);
    messaging::get_ir_curve_bootstrap_pillar_response
    get_ir_curve_bootstrap_pillar(const messaging::get_ir_curve_bootstrap_pillar_request& request);
    messaging::get_many_ir_curve_bootstrap_pillars_response get_many_ir_curve_bootstrap_pillars(
        const messaging::get_many_ir_curve_bootstrap_pillars_request& request);
    messaging::put_ir_curve_bootstrap_pillar_response
    put_ir_curve_bootstrap_pillar(const messaging::put_ir_curve_bootstrap_pillar_request& request);
    messaging::put_many_ir_curve_bootstrap_pillars_response put_many_ir_curve_bootstrap_pillars(
        const messaging::put_many_ir_curve_bootstrap_pillars_request& request);
    messaging::delete_ir_curve_bootstrap_pillar_response delete_ir_curve_bootstrap_pillar(
        const messaging::delete_ir_curve_bootstrap_pillar_request& request);
    messaging::delete_many_ir_curve_bootstrap_pillars_response
    delete_many_ir_curve_bootstrap_pillars(
        const messaging::delete_many_ir_curve_bootstrap_pillars_request& request);
    messaging::list_ir_curve_bootstrap_pillar_versions_response
    list_ir_curve_bootstrap_pillar_versions(
        const messaging::list_ir_curve_bootstrap_pillar_versions_request& request);
    messaging::get_ir_curve_bootstrap_pillar_version_response get_ir_curve_bootstrap_pillar_version(
        const messaging::get_ir_curve_bootstrap_pillar_version_request& request);
    /**@}*/

    /**
     * @brief Lists IR curve bootstrap pillars with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of IR curve bootstrap pillars for the requested page.
     */
    std::vector<domain::ir_curve_bootstrap_pillar> list_pillars(std::uint32_t offset,
                                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active IR curve bootstrap pillars.
     *
     * @return Total number of active IR curve bootstrap pillars.
     */
    std::uint32_t count_pillars();


    /**
     * @brief Retrieves a single IR curve bootstrap pillar as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The IR curve bootstrap pillar at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_bootstrap_pillar> get_pillar_at_version(const std::string& id,
                                                                           std::uint32_t version);

    /**
     * @brief Retrieves a single IR curve bootstrap pillar by its primary key.
     *
     * @return The IR curve bootstrap pillar if found, std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_bootstrap_pillar> get_pillar(const std::string& id);

    /**
     * @brief Retrieves a batch of IR curve bootstrap pillars by primary key.
     */
    std::vector<domain::ir_curve_bootstrap_pillar> get_pillars(const std::vector<std::string>& ids);

    /**
     * @brief Saves a IR curve bootstrap pillar (creates or updates).
     *
     * @param pillar The IR curve bootstrap pillar to save.
     * @throws std::exception on failure.
     */
    void save_pillar(const domain::ir_curve_bootstrap_pillar& pillar);

    /**
     * @brief Saves a batch of IR curve bootstrap pillars.
     *
     * @param pillars The IR curve bootstrap pillars to save.
     * @throws std::exception on failure.
     */
    void save_pillars(const std::vector<domain::ir_curve_bootstrap_pillar>& pillars);

    /**
     * @brief Deletes a IR curve bootstrap pillar by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_pillar(const std::string& id);

    /**
     * @brief Deletes IR curve bootstrap pillars by their primary keys.
     */
    void delete_pillars(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a IR curve bootstrap pillar.
     */
    std::vector<domain::ir_curve_bootstrap_pillar> get_pillar_history(const std::string& id);

private:
    context ctx_;
    repository::ir_curve_bootstrap_pillar_repository repo_;

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
    prepare_change(const messaging::ir_curve_bootstrap_pillar_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::ir_curve_bootstrap_pillar& out);
};

}

#endif
