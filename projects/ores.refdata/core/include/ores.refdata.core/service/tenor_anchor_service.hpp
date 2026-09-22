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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_ANCHOR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_ANCHOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_anchor.hpp"
#include "ores.refdata.api/messaging/tenor_anchor_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_anchor_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor anchors.
 *
 * Provides a higher-level interface for tenor anchor operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_anchor_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_anchor_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_anchor_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_anchor_service(context ctx);

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
    messaging::list_tenor_anchors_response
    list_tenor_anchors(const messaging::list_tenor_anchors_request& request);
    messaging::get_tenor_anchor_response
    get_tenor_anchor(const messaging::get_tenor_anchor_request& request);
    messaging::get_many_tenor_anchors_response
    get_many_tenor_anchors(const messaging::get_many_tenor_anchors_request& request);
    messaging::put_tenor_anchor_response
    put_tenor_anchor(const messaging::put_tenor_anchor_request& request);
    messaging::put_many_tenor_anchors_response
    put_many_tenor_anchors(const messaging::put_many_tenor_anchors_request& request);
    messaging::delete_tenor_anchor_response
    delete_tenor_anchor(const messaging::delete_tenor_anchor_request& request);
    messaging::delete_many_tenor_anchors_response
    delete_many_tenor_anchors(const messaging::delete_many_tenor_anchors_request& request);
    messaging::list_tenor_anchor_versions_response
    list_tenor_anchor_versions(const messaging::list_tenor_anchor_versions_request& request);
    messaging::get_tenor_anchor_version_response
    get_tenor_anchor_version(const messaging::get_tenor_anchor_version_request& request);
    /**@}*/

    /**
     * @brief Lists tenor anchors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor anchors for the requested page.
     */
    std::vector<domain::tenor_anchor> list_anchors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor anchors.
     *
     * @return Total number of active tenor anchors.
     */
    std::uint32_t count_anchors();


    /**
     * @brief Retrieves a single tenor anchor as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenor anchor at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_anchor> get_anchor_at_version(const std::string& code,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single tenor anchor by its primary key.
     *
     * @return The tenor anchor if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_anchor> get_anchor(const std::string& code);

    /**
     * @brief Retrieves a batch of tenor anchors by primary key.
     */
    std::vector<domain::tenor_anchor> get_anchors(const std::vector<std::string>& codes);

    /**
     * @brief Saves a tenor anchor (creates or updates).
     *
     * @param anchor The tenor anchor to save.
     * @throws std::exception on failure.
     */
    void save_anchor(const domain::tenor_anchor& anchor);

    /**
     * @brief Saves a batch of tenor anchors.
     *
     * @param anchors The tenor anchors to save.
     * @throws std::exception on failure.
     */
    void save_anchors(const std::vector<domain::tenor_anchor>& anchors);

    /**
     * @brief Deletes a tenor anchor by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_anchor(const std::string& code);

    /**
     * @brief Deletes tenor anchors by their primary keys.
     */
    void delete_anchors(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor anchor.
     */
    std::vector<domain::tenor_anchor> get_anchor_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_anchor_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::tenor_anchor_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::tenor_anchor& out);
};

}

#endif
