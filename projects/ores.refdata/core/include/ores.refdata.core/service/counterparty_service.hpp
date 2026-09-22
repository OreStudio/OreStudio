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
#ifndef ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_COUNTERPARTY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing counterparties.
 *
 * Provides a higher-level interface for counterparty operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT counterparty_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.counterparty_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_service(context ctx);

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
    messaging::list_counterparties_response
    list_counterparties(const messaging::list_counterparties_request& request);
    messaging::get_counterparty_response
    get_counterparty(const messaging::get_counterparty_request& request);
    messaging::get_many_counterparties_response
    get_many_counterparties(const messaging::get_many_counterparties_request& request);
    messaging::put_counterparty_response
    put_counterparty(const messaging::put_counterparty_request& request);
    messaging::put_many_counterparties_response
    put_many_counterparties(const messaging::put_many_counterparties_request& request);
    messaging::delete_counterparty_response
    delete_counterparty(const messaging::delete_counterparty_request& request);
    messaging::delete_many_counterparties_response
    delete_many_counterparties(const messaging::delete_many_counterparties_request& request);
    messaging::list_counterparty_versions_response
    list_counterparty_versions(const messaging::list_counterparty_versions_request& request);
    messaging::get_counterparty_version_response
    get_counterparty_version(const messaging::get_counterparty_version_request& request);
    /**@}*/

    /**
     * @brief Lists counterparties with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparties for the requested page.
     */
    std::vector<domain::counterparty> list_counterparties(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparties.
     *
     * @return Total number of active counterparties.
     */
    std::uint32_t count_counterparties();


    /**
     * @brief Retrieves a single counterparty as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The counterparty at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> get_counterparty_at_version(const std::string& id,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single counterparty by its primary key.
     *
     * @return The counterparty if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> get_counterparty(const std::string& id);

    /**
     * @brief Retrieves a single counterparty by its uuid primary key.
     *
     * @return The counterparty if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> find_counterparty(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single counterparty by its short_code.
     *
     * @return The counterparty if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty> find_counterparty_by_code(const std::string& short_code);

    /**
     * @brief Retrieves a batch of counterparties by primary key.
     */
    std::vector<domain::counterparty> get_counterparties(const std::vector<std::string>& ids);

    /**
     * @brief Saves a counterparty (creates or updates).
     *
     * @param counterparty The counterparty to save.
     * @throws std::exception on failure.
     */
    void save_counterparty(const domain::counterparty& counterparty);

    /**
     * @brief Saves a batch of counterparties.
     *
     * @param counterparties The counterparties to save.
     * @throws std::exception on failure.
     */
    void save_counterparties(const std::vector<domain::counterparty>& counterparties);

    /**
     * @brief Deletes a counterparty by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_counterparty(const std::string& id);

    /**
     * @brief Removes a counterparty by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_counterparty(const boost::uuids::uuid& id);

    /**
     * @brief Deletes counterparties by their primary keys.
     */
    void delete_counterparties(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a counterparty.
     */
    std::vector<domain::counterparty> get_counterparty_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a counterparty
     * by its uuid primary key.
     */
    std::vector<domain::counterparty> get_counterparty_history(const boost::uuids::uuid& id);

    /**
     * @brief Gets the counterparty hierarchy (as a forest of trees) rooted
     * at, or containing, the given counterparty.
     *
     * @param root_id The counterparty to start from.
     * @param from_root If true, returns the whole tree the given node
     * belongs to instead of just its subtree.
     * @return A forest of hierarchy_node trees (normally a single root).
     */
    std::vector<ores::utility::domain::hierarchy_node>
    get_hierarchy(const boost::uuids::uuid& root_id, bool from_root);

private:
    context ctx_;
    repository::counterparty_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::counterparty_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::counterparty& out);
};

}

#endif
