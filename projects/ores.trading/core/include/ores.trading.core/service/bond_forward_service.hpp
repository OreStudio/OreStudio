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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_FORWARD_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_FORWARD_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_forward.hpp"
#include "ores.trading.api/messaging/bond_forward_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_forward_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond forwards.
 *
 * Provides a higher-level interface for bond forward operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_forward_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_forward_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_forward_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_forward_service(context ctx);

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
    messaging::list_bond_forwards_response
    list_bond_forwards(const messaging::list_bond_forwards_request& request);
    messaging::get_bond_forward_response
    get_bond_forward(const messaging::get_bond_forward_request& request);
    messaging::get_many_bond_forwards_response
    get_many_bond_forwards(const messaging::get_many_bond_forwards_request& request);
    messaging::put_bond_forward_response
    put_bond_forward(const messaging::put_bond_forward_request& request);
    messaging::put_many_bond_forwards_response
    put_many_bond_forwards(const messaging::put_many_bond_forwards_request& request);
    messaging::delete_bond_forward_response
    delete_bond_forward(const messaging::delete_bond_forward_request& request);
    messaging::delete_many_bond_forwards_response
    delete_many_bond_forwards(const messaging::delete_many_bond_forwards_request& request);
    messaging::list_bond_forward_versions_response
    list_bond_forward_versions(const messaging::list_bond_forward_versions_request& request);
    messaging::get_bond_forward_version_response
    get_bond_forward_version(const messaging::get_bond_forward_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond forwards with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond forwards for the requested page.
     */
    std::vector<domain::bond_forward> list_bond_forwards(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond forwards.
     *
     * @return Total number of active bond forwards.
     */
    std::uint32_t count_bond_forwards();


    /**
     * @brief Retrieves a single bond forward as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond forward at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_forward>
    get_bond_forward_at_version(const boost::uuids::uuid& instrument_id, std::uint32_t version);

    /**
     * @brief Retrieves a single bond forward by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The bond forward if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_forward> get_bond_forward(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of bond forwards by primary key.
     */
    std::vector<domain::bond_forward>
    get_bond_forwards(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a bond forward (creates or updates).
     *
     * @param bond_forward The bond forward to save.
     * @throws std::exception on failure.
     */
    void save_bond_forward(const domain::bond_forward& bond_forward);

    /**
     * @brief Saves a batch of bond forwards.
     *
     * @param bond_forwards The bond forwards to save.
     * @throws std::exception on failure.
     */
    void save_bond_forwards(const std::vector<domain::bond_forward>& bond_forwards);

    /**
     * @brief Deletes a bond forward by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_bond_forward(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes bond forwards by their primary keys.
     */
    void delete_bond_forwards(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a bond forward.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_forward> get_bond_forward_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::bond_forward_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::bond_forward_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::bond_forward& out);
};

}

#endif
