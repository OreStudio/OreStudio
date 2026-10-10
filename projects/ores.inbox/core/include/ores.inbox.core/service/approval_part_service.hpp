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
#ifndef ORES_INBOX_CORE_SERVICE_APPROVAL_PART_SERVICE_HPP
#define ORES_INBOX_CORE_SERVICE_APPROVAL_PART_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.inbox.api/domain/approval_part.hpp"
#include "ores.inbox.api/messaging/approval_part_protocol.hpp"
#include "ores.inbox.core/export.hpp"
#include "ores.inbox.core/repository/approval_part_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::service {

/**
 * @brief Service for managing approval parts.
 *
 * Provides a higher-level interface for approval part operations,
 * wrapping the underlying repository.
 */
class ORES_INBOX_CORE_EXPORT approval_part_service {
private:
    inline static std::string_view logger_name = "ores.inbox.service.approval_part_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a approval_part_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit approval_part_service(context ctx);

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
    messaging::list_approval_parts_response
    list_approval_parts(const messaging::list_approval_parts_request& request);
    messaging::get_approval_part_response
    get_approval_part(const messaging::get_approval_part_request& request);
    messaging::get_many_approval_parts_response
    get_many_approval_parts(const messaging::get_many_approval_parts_request& request);
    messaging::put_approval_part_response
    put_approval_part(const messaging::put_approval_part_request& request);
    messaging::put_many_approval_parts_response
    put_many_approval_parts(const messaging::put_many_approval_parts_request& request);
    messaging::delete_approval_part_response
    delete_approval_part(const messaging::delete_approval_part_request& request);
    messaging::delete_many_approval_parts_response
    delete_many_approval_parts(const messaging::delete_many_approval_parts_request& request);
    messaging::list_approval_part_versions_response
    list_approval_part_versions(const messaging::list_approval_part_versions_request& request);
    messaging::get_approval_part_version_response
    get_approval_part_version(const messaging::get_approval_part_version_request& request);
    /**@}*/

    /**
     * @brief Lists approval parts with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of approval parts for the requested page.
     */
    std::vector<domain::approval_part> list_parts(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active approval parts.
     *
     * @return Total number of active approval parts.
     */
    std::uint32_t count_parts();


    /**
     * @brief Retrieves a single approval part as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The approval part at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::approval_part> get_part_at_version(const std::string& code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single approval part by its primary key.
     *
     * @return The approval part if found, std::nullopt otherwise.
     */
    std::optional<domain::approval_part> find_part(const std::string& code);

    /**
     * @brief Retrieves a batch of approval parts by primary key.
     */
    std::vector<domain::approval_part> get_parts(const std::vector<std::string>& codes);

    /**
     * @brief Saves a approval part (creates or updates).
     *
     * @param part The approval part to save.
     * @throws std::exception on failure.
     */
    void save_part(const domain::approval_part& part);

    /**
     * @brief Saves a batch of approval parts.
     *
     * @param parts The approval parts to save.
     * @throws std::exception on failure.
     */
    void save_parts(const std::vector<domain::approval_part>& parts);

    /**
     * @brief Deletes a approval part by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_part(const std::string& code);

    /**
     * @brief Deletes approval parts by their primary keys.
     */
    void delete_parts(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a approval part.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::approval_part> get_part_history(const std::string& code);

private:
    context ctx_;
    repository::approval_part_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::approval_part_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::approval_part& out);
};

}

#endif
