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
#ifndef ORES_REFDATA_CORE_SERVICE_CURRENCY_GROUP_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURRENCY_GROUP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_group.hpp"
#include "ores.refdata.api/messaging/currency_group_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/currency_group_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing currency groups.
 *
 * Provides a higher-level interface for currency group operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT currency_group_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.currency_group_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a currency_group_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit currency_group_service(context ctx);

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
    messaging::list_currency_groups_response
    list_currency_groups(const messaging::list_currency_groups_request& request);
    messaging::get_currency_group_response
    get_currency_group(const messaging::get_currency_group_request& request);
    messaging::get_many_currency_groups_response
    get_many_currency_groups(const messaging::get_many_currency_groups_request& request);
    messaging::put_currency_group_response
    put_currency_group(const messaging::put_currency_group_request& request);
    messaging::put_many_currency_groups_response
    put_many_currency_groups(const messaging::put_many_currency_groups_request& request);
    messaging::delete_currency_group_response
    delete_currency_group(const messaging::delete_currency_group_request& request);
    messaging::delete_many_currency_groups_response
    delete_many_currency_groups(const messaging::delete_many_currency_groups_request& request);
    messaging::list_currency_group_versions_response
    list_currency_group_versions(const messaging::list_currency_group_versions_request& request);
    messaging::get_currency_group_version_response
    get_currency_group_version(const messaging::get_currency_group_version_request& request);
    /**@}*/

    /**
     * @brief Lists currency groups with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of currency groups for the requested page.
     */
    std::vector<domain::currency_group> list_groups(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active currency groups.
     *
     * @return Total number of active currency groups.
     */
    std::uint32_t count_groups();


    /**
     * @brief Retrieves a single currency group as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The currency group at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_group> get_group_at_version(const std::string& code,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single currency group by its primary key.
     *
     * @return The currency group if found, std::nullopt otherwise.
     */
    std::optional<domain::currency_group> get_group(const std::string& code);

    /**
     * @brief Retrieves a batch of currency groups by primary key.
     */
    std::vector<domain::currency_group> get_groups(const std::vector<std::string>& codes);

    /**
     * @brief Saves a currency group (creates or updates).
     *
     * @param group The currency group to save.
     * @throws std::exception on failure.
     */
    void save_group(const domain::currency_group& group);

    /**
     * @brief Saves a batch of currency groups.
     *
     * @param groups The currency groups to save.
     * @throws std::exception on failure.
     */
    void save_groups(const std::vector<domain::currency_group>& groups);

    /**
     * @brief Deletes a currency group by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_group(const std::string& code);

    /**
     * @brief Deletes currency groups by their primary keys.
     */
    void delete_groups(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a currency group.
     */
    std::vector<domain::currency_group> get_group_history(const std::string& code);

private:
    context ctx_;
    repository::currency_group_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::currency_group_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::currency_group& out);
};

}

#endif
