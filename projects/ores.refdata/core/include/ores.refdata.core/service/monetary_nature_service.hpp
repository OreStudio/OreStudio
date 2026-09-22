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
#ifndef ORES_REFDATA_CORE_SERVICE_MONETARY_NATURE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_MONETARY_NATURE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/monetary_nature.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/monetary_nature_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing monetary natures.
 *
 * Provides a higher-level interface for monetary nature operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT monetary_nature_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.monetary_nature_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a monetary_nature_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit monetary_nature_service(context ctx);

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
    messaging::list_monetary_natures_response
    list_monetary_natures(const messaging::list_monetary_natures_request& request);
    messaging::get_monetary_nature_response
    get_monetary_nature(const messaging::get_monetary_nature_request& request);
    messaging::get_many_monetary_natures_response
    get_many_monetary_natures(const messaging::get_many_monetary_natures_request& request);
    messaging::put_monetary_nature_response
    put_monetary_nature(const messaging::put_monetary_nature_request& request);
    messaging::put_many_monetary_natures_response
    put_many_monetary_natures(const messaging::put_many_monetary_natures_request& request);
    messaging::delete_monetary_nature_response
    delete_monetary_nature(const messaging::delete_monetary_nature_request& request);
    messaging::delete_many_monetary_natures_response
    delete_many_monetary_natures(const messaging::delete_many_monetary_natures_request& request);
    messaging::list_monetary_nature_versions_response
    list_monetary_nature_versions(const messaging::list_monetary_nature_versions_request& request);
    messaging::get_monetary_nature_version_response
    get_monetary_nature_version(const messaging::get_monetary_nature_version_request& request);
    /**@}*/

    /**
     * @brief Lists monetary natures with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of monetary natures for the requested page.
     */
    std::vector<domain::monetary_nature> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active monetary natures.
     *
     * @return Total number of active monetary natures.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single monetary nature as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The monetary nature at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::monetary_nature> get_type_at_version(const std::string& code,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single monetary nature by its primary key.
     *
     * @return The monetary nature if found, std::nullopt otherwise.
     */
    std::optional<domain::monetary_nature> get_type(const std::string& code);

    /**
     * @brief Retrieves a batch of monetary natures by primary key.
     */
    std::vector<domain::monetary_nature> get_types(const std::vector<std::string>& codes);

    /**
     * @brief Saves a monetary nature (creates or updates).
     *
     * @param type The monetary nature to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::monetary_nature& type);

    /**
     * @brief Saves a batch of monetary natures.
     *
     * @param types The monetary natures to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::monetary_nature>& types);

    /**
     * @brief Deletes a monetary nature by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes monetary natures by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a monetary nature.
     */
    std::vector<domain::monetary_nature> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::monetary_nature_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::monetary_nature_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::monetary_nature& out);
};

}

#endif
