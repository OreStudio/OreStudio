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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_TRS_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_TRS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_trs.hpp"
#include "ores.trading.api/messaging/bond_trs_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_trs_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond trs.
 *
 * Provides a higher-level interface for bond trs operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_trs_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_trs_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_trs_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_trs_service(context ctx);

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
    messaging::list_bond_trs_response
    list_bond_trs(const messaging::list_bond_trs_request& request);
    messaging::get_bond_trs_response get_bond_trs(const messaging::get_bond_trs_request& request);
    messaging::get_many_bond_trs_response
    get_many_bond_trs(const messaging::get_many_bond_trs_request& request);
    messaging::put_bond_trs_response put_bond_trs(const messaging::put_bond_trs_request& request);
    messaging::put_many_bond_trs_response
    put_many_bond_trs(const messaging::put_many_bond_trs_request& request);
    messaging::delete_bond_trs_response
    delete_bond_trs(const messaging::delete_bond_trs_request& request);
    messaging::delete_many_bond_trs_response
    delete_many_bond_trs(const messaging::delete_many_bond_trs_request& request);
    messaging::list_bond_trs_versions_response
    list_bond_trs_versions(const messaging::list_bond_trs_versions_request& request);
    messaging::get_bond_trs_version_response
    get_bond_trs_version(const messaging::get_bond_trs_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond trs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond trs for the requested page.
     */
    std::vector<domain::bond_trs> list_trs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond trs.
     *
     * @return Total number of active bond trs.
     */
    std::uint32_t count_trs();


    /**
     * @brief Retrieves a single bond trs as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond trs at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_trs> get_trs_at_version(const boost::uuids::uuid& instrument_id,
                                                       std::uint32_t version);

    /**
     * @brief Retrieves a single bond trs by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The bond trs if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_trs> get_trs(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of bond trs by primary key.
     */
    std::vector<domain::bond_trs> get_trs(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a bond trs (creates or updates).
     *
     * @param trs The bond trs to save.
     * @throws std::exception on failure.
     */
    void save_trs(const domain::bond_trs& trs);

    /**
     * @brief Saves a batch of bond trs.
     *
     * @param trs The bond trs to save.
     * @throws std::exception on failure.
     */
    void save_trs(const std::vector<domain::bond_trs>& trs);

    /**
     * @brief Deletes a bond trs by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trs(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes bond trs by their primary keys.
     */
    void delete_trs(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a bond trs.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_trs> get_trs_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::bond_trs_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::bond_trs_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::bond_trs& out);
};

}

#endif
